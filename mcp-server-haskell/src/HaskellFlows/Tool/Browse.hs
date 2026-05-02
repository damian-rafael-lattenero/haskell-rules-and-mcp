-- | @ghc_browse@ — Phase-2 tool (GHC-API migrated).
--
-- Lists names exported by a loaded module and their types. Pre-migration
-- parsed the raw line-per-entry output of @:browse Module@; post-migration
-- queries 'getModuleInfo' + 'modInfoExports' and renders each export's
-- type via 'TyThing'.
module HaskellFlows.Tool.Browse
  ( descriptor
  , handle
  , parseBrowseOutput
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import GHC
  ( Ghc
  , Name
  , TyThing (AnId)
  , getModuleGraph
  , getModuleInfo
  , lookupName
  , mgModSummaries
  , mkModuleName
  , modInfoExports
  , moduleName
  , ms_mod
  )
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (varType)
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcBrowse
    , tdDescription =
        "List names exported by a loaded module + their types. "
          <> "Resolves against the auto-loaded project module graph."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module" .= object [ "type" .= ("string" :: Text) ] ]
          , "required"             .= ["module" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype BrowseArgs = BrowseArgs Text

instance FromJSON BrowseArgs where
  parseJSON = withObject "BrowseArgs" $ \o -> BrowseArgs <$> o .: "module"

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind err)
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) })))
  Right (BrowseArgs m) -> do
    eRes <- try (withGhcSession ghcSess (queryBrowse m))
    pure $ Env.toolResponseToResult $ case eRes of
      Left (se :: SomeException) ->
        Env.mkFailed
          ((Env.mkErrorEnvelope Env.InternalError
              (T.pack ("GHC API error: " <> show se)))
                { Env.eeCause = Just (T.pack (show se)) })
      Right Nothing ->
        -- Issue #72 + #90: 'browse' can only enumerate modules
        -- from the project's compile graph. Phase B re-shapes the
        -- response: the not-found case is now status='no_match'
        -- (the question was well-formed, the answer is "this
        -- module isn't in this graph") with the diagnostic
        -- context inside 'result' and a 'nextStep' pointer at
        -- 'ghc_info' / 'hoogle_search' for the agent's next move.
        Env.withNextStep moduleNotInGraphNextStep
          (Env.mkNoMatch (moduleNotInGraphPayload m))
      Right (Just entries) ->
        Env.mkOk (browsePayload m entries)

-- | Discriminate the FromJSON failure shape — same heuristic as
-- 'HaskellFlows.Tool.Workflow.parseErrorKind'. A missing required
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

-- | Look up the module in the current module graph, pull its exports,
-- render each as "name :: type" (or just the name for non-Id things).
queryBrowse :: Text -> Ghc (Maybe [Text])
queryBrowse nm = do
  let wanted = mkModuleName (T.unpack nm)
  mg <- getModuleGraph
  let matches =
        [ ms_mod ms
        | ms <- mgModSummaries mg
        , moduleName (ms_mod ms) == wanted
        ]
  case matches of
    []      -> pure Nothing
    (m : _) -> do
      minfo <- getModuleInfo m
      case minfo of
        Nothing -> pure (Just [])
        Just mi -> do
          let exports = modInfoExports mi
          entries <- traverse renderExport exports
          pure (Just entries)

-- | Render a single exported 'Name' as @"name :: type"@ when the
-- underlying 'TyThing' carries a type (identifier bindings); fall
-- back to the bare name for datatype / class / etc. entries.
renderExport :: Name -> Ghc Text
renderExport n = do
  let nm = T.pack (occNameString (nameOccName n))
  mTy <- lookupName n
  case mTy of
    Just (AnId i) ->
      pure (nm <> " :: " <> T.pack (showPprUnsafe (varType i)))
    _ ->
      pure nm

--------------------------------------------------------------------------------
-- legacy parser (retained for existing unit tests)
--------------------------------------------------------------------------------

-- | Pre-migration parser kept for the unit-test scaffolding. The live
-- path no longer calls this — the GHC API returns exports as 'Name'
-- directly. Retained as a pure parser fixture so the unit tests can
-- pin the text-shape contract without a live session.
parseBrowseOutput :: Text -> [Text]
parseBrowseOutput = filter (not . T.null) . map T.strip . T.lines

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Browse-success payload. Issue #90 Phase B: status='ok' with
-- the same field names as before ('module', 'count', 'entries')
-- so consumers continue to function during the dual-shape window.
browsePayload :: Text -> [Text] -> Value
browsePayload m entries = object
  [ "module"  .= m
  , "count"   .= length entries
  , "entries" .= entries
  ]

-- | Issue #72 + #90: payload for the no-match path. Carries
-- 'module' echo + a 'remediation' string. The previous shape's
-- 'error' string is replaced by the structured envelope at the
-- top level.
moduleNotInGraphPayload :: Text -> Value
moduleNotInGraphPayload m = object
  [ "module"      .= m
  , "remediation" .= ("Browse only enumerates modules compiled by this project. \
                      \For modules in interactive scope (Prelude, base, external \
                      \deps), look up individual names with ghc_info or query \
                      \with hoogle_search." :: Text)
  ]

-- | NextStep pointer attached to the no-match path: per-name
-- inspection via 'ghc_info', or discovery via 'hoogle_search'.
moduleNotInGraphNextStep :: Value
moduleNotInGraphNextStep = object
  [ "tool"    .= ("ghc_info" :: Text)
  , "why"     .= ("'ghc_browse' only sees modules compiled into this project. \
                  \Use ghc_info(name=\"<symbol>\") for per-name inspection of \
                  \external/base modules, or hoogle_search to discover names." :: Text)
  , "example" .= object
      [ "name" .= ("<symbol you're trying to inspect>" :: Text) ]
  ]
