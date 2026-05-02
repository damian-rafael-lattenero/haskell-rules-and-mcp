-- | @ghc_doc@ — Phase-2 tool (GHC-API migrated).
--
-- Looks up Haddock documentation for a name. Pre-migration wrapped
-- @:doc@ over stdio; post-migration calls 'GHC.getDocs' directly.
--
-- Packages without @-haddock@ still degrade gracefully: 'getDocs'
-- returns 'Left', which we surface as @{success: true, hasDoc: false}@.
-- Same shape as before, same 'success: true' invariant that
-- @FlowExploratory@ checks.
module HaskellFlows.Tool.Doc
  ( descriptor
  , handle
  , DocArgs (..)
  , extractHaddockAbove
    -- * Response shaping (exported for unit tests)
  , hasDocPayload
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class (liftIO)
import GHC (Ghc, getDocs, getNamesInScope)
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (nameOccName, nameSrcSpan)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (SrcSpan (RealSrcSpan), srcSpanFile, srcSpanStartLine)
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcDoc
    , tdDescription =
        "Look up Haddock documentation for a name via the GHC API. "
          <> "Returns the doc block as plain text. If the hosting "
          <> "package was built without -haddock or the name has no "
          <> "doc, reports that cleanly without failing."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name to look up. Examples: \"map\", \"Functor\", \
                       \\"(++)\"." :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype DocArgs = DocArgs
  { daName :: Text
  }
  deriving stock (Show)

instance FromJSON DocArgs where
  parseJSON = withObject "DocArgs" $ \o ->
    DocArgs <$> o .: "name"

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right (DocArgs nm) -> case sanitizeExpression nm of
    Left e ->
      pure (Env.toolResponseToResult (Env.mkRefused (Env.sanitizeRejection "name" e)))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryDoc safe))
      case eRes of
        Left (se :: SomeException) ->
          pure $ Env.toolResponseToResult $
            Env.mkFailed
              ((Env.mkErrorEnvelope Env.InternalError
                  (T.pack ("GHC API error: " <> show se)))
                    { Env.eeCause = Just (T.pack (show se)) })
        Right Nothing ->
          -- Issue #87 + #90: name not in scope → no_match, NOT a
          -- success-shaped 'hasDoc: false'.
          pure $ Env.toolResponseToResult $
            Env.mkNoMatch (noDocPayload safe "Name not in scope")
        Right (Just (Just t)) ->
          pure $ Env.toolResponseToResult $ Env.mkOk (hasDocPayload safe t)
        Right (Just Nothing) ->
          -- Name is in scope but the package was built without
          -- -haddock (or carries no doc string).  Fall back to
          -- scanning the source file for a @-- |@ comment block
          -- directly above the definition (issue #103).
          do
            mSrc <- (try (withGhcSession ghcSess (sourceDoc safe))
                      :: IO (Either SomeException (Maybe Text)))
            pure $ Env.toolResponseToResult $ case mSrc of
              Right (Just txt) ->
                Env.mkOk (hasDocPayload safe txt)
              _ ->
                Env.mkNoMatch (noDocPayload safe
                  "No Haddock available (package may have been built without -haddock)")

-- | Discriminate the FromJSON failure shape — a missing required
-- field maps to 'MissingArg'; everything else falls back to
-- 'TypeMismatch'.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Result shape:
--
-- * 'Nothing'          — name isn't in scope at all
-- * 'Just Nothing'     — name found but no doc (no -haddock, or no doc string)
-- * 'Just (Just txt)'  — doc text
queryDoc :: Text -> Ghc (Maybe (Maybe Text))
queryDoc nm = do
  names <- getNamesInScope
  let target = T.unpack nm
      matches =
        [ n
        | n <- names
        , occNameString (nameOccName n) == target
        ]
  case matches of
    []    -> pure Nothing
    (n:_) -> do
      result <- getDocs n
      pure . Just $ case result of
        Left _                   -> Nothing
        Right (Nothing, _)       -> Nothing
        Right (Just docStr, _)   -> Just (T.pack (showPprUnsafe docStr))

-- | Source-file fallback for 'queryDoc': locate the binding via its
-- 'SrcSpan', read the file, and extract contiguous @-- |@ / @--@
-- comment lines immediately above the definition (issue #103).
--
-- Returns 'Nothing' when the name has no 'RealSrcSpan' (e.g. it was
-- compiled without debug info), when the file cannot be read, or
-- when there is no Haddock block above the definition.
sourceDoc :: Text -> Ghc (Maybe Text)
sourceDoc nm = do
  names <- getNamesInScope
  let target = T.unpack nm
      matches = [n | n <- names, occNameString (nameOccName n) == target]
  case matches of
    []    -> pure Nothing
    (n:_) -> case nameSrcSpan n of
      RealSrcSpan rspan _ ->
        let file    = unpackFS (srcSpanFile rspan)
            defLine = srcSpanStartLine rspan
        in liftIO (extractHaddockAbove file defLine)
      _ -> pure Nothing

-- | Read @file@, collect the contiguous comment block immediately
-- above @defLine@ (1-indexed), and return the cleaned text.
--
-- Rules (per Haskell Haddock spec):
--   * Scan upward from @defLine - 1@.
--   * Collect lines that start with @--@ (after stripping leading
--     whitespace).
--   * Stop at the first non-comment line.
--   * The block is a Haddock block if its topmost collected line
--     starts with @-- |@.  Otherwise return 'Nothing'.
--   * Strip comment prefixes (@-- | @, @-- @, @--@) before returning.
extractHaddockAbove :: FilePath -> Int -> IO (Maybe Text)
extractHaddockAbove file defLine = do
  eContent <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case eContent of
    Left _        -> pure Nothing
    Right content -> do
      let ls         = T.lines content
          above      = reverse (take (defLine - 1) ls)
          collected  = takeWhile isComment above
      if null collected || not (any isHaddockStart collected)
        then pure Nothing
        else pure (Just (T.unlines (map stripCommentPrefix (reverse collected))))
  where
    isComment ln     = "--" `T.isPrefixOf` T.strip ln
    isHaddockStart ln = "-- |" `T.isPrefixOf` T.strip ln
    stripCommentPrefix ln =
      let s = T.strip ln
      in if "-- | " `T.isPrefixOf` s then T.drop 5 s
         else if "-- |" `T.isPrefixOf` s then T.drop 4 s
         else if "-- " `T.isPrefixOf` s  then T.drop 3 s
         else if "--" `T.isPrefixOf` s   then T.drop 2 s
         else ln


--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Doc-found payload. Issue #90 Phase B: result.{name, hasDoc=true,
-- doc} — same field names as before so consumers continue to work
-- during the dual-shape window.
hasDocPayload :: Text -> Text -> Value
hasDocPayload nm doc = object
  [ "name"   .= nm
  , "hasDoc" .= True
  , "doc"    .= stripLatex (T.strip doc)
  ]

-- | Strip LaTeX delimiters from Haddock strings (F-11). GHC's pretty-
-- printer emits @\\(…\\)@ for math notation; agents receive raw escape
-- sequences they cannot render. Replace with the inner expression.
stripLatex :: Text -> Text
stripLatex = T.replace "\\(" "" . T.replace "\\)" ""

-- | No-doc payload (rides StatusNoMatch). result.{name, hasDoc=false,
-- reason}. Two semantically-distinct cases share this shape — name
-- not in scope vs name in scope but no doc — and the consumer
-- discriminates via the 'reason' string. Both are 'no_match'
-- because in both cases the question was well-formed and the
-- answer is the empty set.
noDocPayload :: Text -> Text -> Value
noDocPayload nm reason = object
  [ "name"   .= nm
  , "hasDoc" .= False
  , "reason" .= reason
  ]
