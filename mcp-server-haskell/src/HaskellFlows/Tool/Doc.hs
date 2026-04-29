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
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import GHC (Ghc, getDocs, getNamesInScope)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
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
      pure $ Env.toolResponseToResult $ case eRes of
        Left (se :: SomeException) ->
          Env.mkFailed
            ((Env.mkErrorEnvelope Env.InternalError
                (T.pack ("GHC API error: " <> show se)))
                  { Env.eeCause = Just (T.pack (show se)) })
        Right Nothing ->
          -- Issue #87 + #90: name not in scope → no_match, NOT a
          -- success-shaped 'hasDoc: false'. The previous response
          -- was indistinguishable from 'name found, no doc' (also
          -- success: true). Now the agent gets a clean
          -- 'status: no_match' with the searched name and a
          -- reason field for diagnostic context.
          Env.mkNoMatch (noDocPayload safe "Name not in scope")
        Right (Just Nothing) ->
          -- Name found, but the package was built without
          -- -haddock or the symbol carries no doc. Same status
          -- as 'not in scope' — the request was well-formed and
          -- the answer is the empty set — but the reason field
          -- discriminates so the agent can pick a different
          -- recovery (build with -haddock vs query a different
          -- name).
          Env.mkNoMatch (noDocPayload safe
            "No Haddock available (package may have been built without -haddock)")
        Right (Just (Just t)) ->
          Env.mkOk (hasDocPayload safe t)

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
  , "doc"    .= T.strip doc
  ]

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
