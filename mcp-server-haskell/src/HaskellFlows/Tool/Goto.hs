-- | @ghc_goto@ — Phase-2 tool (GHC-API migrated).
--
-- Returns the source location where a name is defined. Pre-migration
-- parsed "Defined at" / "Defined in" markers from @:info@ output;
-- post-migration queries the 'Name''s 'SrcSpan' directly.
--
-- Richer jump-to-definition (cross-module re-exports, macro-generated
-- names) still belongs to HLS — a future phase will wrap an
-- @ghc_hls@ tool once that lands.
module HaskellFlows.Tool.Goto
  ( descriptor
  , handle
  , GotoArgs (..)
  , parseDefinedAt
  , Location (..)
    -- * Issue #117 — exposed for unit tests
  , locationPayload
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import GHC
  ( Ghc
  , Name
  , getNamesInScope
  , moduleName
  , nameSrcSpan
  )
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc
  ( SrcSpan (RealSrcSpan, UnhelpfulSpan)
  , srcSpanFile
  , srcSpanStartCol
  , srcSpanStartLine
  )
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcGoto
    , tdDescription =
        "Return the source location where a name is defined, via the "
          <> "GHC API's SrcSpan. For cross-module precision you'll want "
          <> "HLS (future ghc_hls tool)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name to locate. Examples: \"greet\", \"Functor\"."
                       :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype GotoArgs = GotoArgs
  { gaName :: Text
  }
  deriving stock (Show)

instance FromJSON GotoArgs where
  parseJSON = withObject "GotoArgs" $ \o ->
    GotoArgs <$> o .: "name"

-- | A resolved source location. Either a concrete @file:line:col@
-- (project-defined names) or a bare module name (for names resolved
-- to an imported module without a local SrcSpan).
data Location
  = InFile !Text !Int !Int
  | InModule !Text
  deriving stock (Eq, Show)

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right (GotoArgs nm) -> case sanitizeExpression nm of
    Left e ->
      pure (Env.toolResponseToResult (Env.mkRefused (Env.sanitizeRejection "name" e)))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryLocation safe))
      pure $ Env.toolResponseToResult $ case eRes of
        Left (se :: SomeException) ->
          Env.mkFailed
            ((Env.mkErrorEnvelope Env.InternalError
                (T.pack ("GHC API error: " <> show se)))
                  { Env.eeCause = Just (T.pack (show se)) })
        Right Nothing ->
          -- Issue #90 §3 + §4: name not in scope is a 'no_match'
          -- (the question was well-formed, the answer is the
          -- empty set), NOT a 'failed'. The result echoes the
          -- name + the search context so the agent can pivot.
          Env.mkNoMatch (notInScopePayload safe)
        -- Issue #117: file locations → ok (can jump); library/unknown
        -- module locations → no_match (name found but no source to
        -- jump to). The payload still carries module + has_location so
        -- the agent knows *why* there is no file path.
        Right (Just loc) -> case loc of
          InFile {} -> Env.mkOk (locationPayload safe loc)
          InModule {} -> Env.mkNoMatch (locationPayload safe loc)

-- | Discriminate the FromJSON failure shape — same heuristic as
-- the other Phase-B migrations.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Match names in the interactive scope by exact occurrence name,
-- then promote the 'SrcSpan' to a structured 'Location'.
queryLocation :: Text -> Ghc (Maybe Location)
queryLocation nm = do
  names <- getNamesInScope
  let target = T.unpack nm
      matches =
        [ n
        | n <- names
        , occNameString (nameOccName n) == target
        ]
  case matches of
    []    -> pure Nothing
    (n:_) -> pure (Just (nameToLocation n))

nameToLocation :: Name -> Location
nameToLocation n = case nameSrcSpan n of
  RealSrcSpan rspan _ ->
    InFile
      (T.pack (unpackFS (srcSpanFile rspan)))
      (srcSpanStartLine rspan)
      (srcSpanStartCol rspan)
  UnhelpfulSpan _ ->
    case nameModule_maybe n of
      Just m  -> InModule (T.pack (showPprUnsafe (moduleName m)))
      Nothing -> InModule "<unknown>"

--------------------------------------------------------------------------------
-- legacy parser (retained for unit-test back-compat)
--------------------------------------------------------------------------------

-- | Kept for the existing unit tests that validate the pre-migration
-- parser. The live code path no longer calls this — the GHC API
-- returns 'SrcSpan' directly. Retained as a pure parser fixture so
-- the unit tests can pin the text-shape contract without a live
-- session.
parseDefinedAt :: Text -> Maybe Location
parseDefinedAt raw = firstJust tryLine (T.lines raw)
  where
    tryLine ln
      | Just rest <- findMarker "-- Defined at " ln = parseFileLoc rest
      | Just rest <- findMarker "-- Defined in " ln = parseModuleLoc rest
      | otherwise = Nothing

    findMarker marker ln =
      let (_, after) = T.breakOn marker ln
      in if T.null after
           then Nothing
           else Just (T.drop (T.length marker) after)

parseFileLoc :: Text -> Maybe Location
parseFileLoc t =
  case T.splitOn ":" (T.strip t) of
    (file : lnTxt : colTxt : _) -> do
      l <- readMaybe (T.unpack (T.filter (/= ' ') lnTxt))
      c <- readMaybe (T.unpack (T.filter (/= ' ') colTxt))
      pure (InFile file l c)
    _ -> Nothing

parseModuleLoc :: Text -> Maybe Location
parseModuleLoc t =
  let stripped = T.dropAround (`elem` (" '\x2018\x2019" :: String)) (T.strip t)
  in if T.null stripped then Nothing else Just (InModule stripped)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ []     = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Render the resolved location into the same shape the legacy
-- callers consumed (kind=file with file/line/column, OR kind=module
-- with module). Phase B keeps these field names; only the wrapping
-- 'success: bool' moves out of the payload (auto-derived from
-- 'status').
-- | Issue #117: 'InFile' results carry @has_location: true@ (agent can
-- jump to a source file); 'InModule' results carry @has_location: false@
-- plus a remediation hint so the agent understands *why* no file is
-- available (e.g. the name lives in a library with no local source).
locationPayload :: Text -> Location -> Value
locationPayload nm = \case
  InFile f l c ->
    object
      [ "name"         .= nm
      , "kind"         .= ("file" :: Text)
      , "file"         .= f
      , "line"         .= l
      , "column"       .= c
      , "has_location" .= True
      ]
  InModule m ->
    object
      [ "name"         .= nm
      , "kind"         .= ("module" :: Text)
      , "module"       .= m
      , "has_location" .= False
      , "remediation"  .= remediationFor m
      ]
  where
    remediationFor m
      | m == "<unknown>" =
          "Name has no SrcSpan — it may be a built-in or auto-derived \
          \binding. Use ghc_info for type information." :: Text
      | otherwise =
          "Name is defined in library module '" <> m <> "' which has \
          \no local source file. Use ghc_info for its type or \
          \ghc_doc for Haddock documentation."

-- | Result payload for the no-match (name-not-in-scope) path.
-- Carries the searched name + a remediation pointer so the agent
-- can choose to retry via a richer surface.
notInScopePayload :: Text -> Value
notInScopePayload nm = object
  [ "name"        .= nm
  , "searched_in" .= ("interactive scope" :: Text)
  , "remediation" .= ("Name not currently in scope. If it's defined in a \
                      \loaded module, run ghc_load on that module first. \
                      \For external/base names, ghc_info often resolves \
                      \what ghc_goto cannot." :: Text)
  ]
