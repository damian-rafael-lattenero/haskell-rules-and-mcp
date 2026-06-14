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
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T

import GHC
  ( Ghc
  , Module
  , Name
  , TyThing (AnId)
  , getModuleGraph
  , getModuleInfo
  , lookupModule
  , lookupName
  , mgModSummaries
  , mkModuleName
  , modInfoExports
  , moduleName
  , ms_hspp_file
  , ms_mod
  )
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (varType)
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, gsProject, withGhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Types (unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcBrowse
    , tdDescription =
        "PURPOSE: List names exported by a loaded module + their types. "
          <> "WHEN: orienting in an unfamiliar module before touching it; "
          <> "confirming an export was added or surfaced; exploring what "
          <> "a session-preloaded module (Prelude, Data.Map, etc.) exports "
          <> "after ghc_add_import brought it into scope. "
          <> "WHEN NOT: the module cannot be found in the project graph "
          <> "or the package environment — use hoogle_search for discovery, "
          <> "or ghc_info for a single name's details. "
          <> "PREREQUISITES: any prior ghc_load / ghc_check_module / "
          <> "ghc_check_project pulls the target into the compile graph; "
          <> "ghc_add_import makes standard-library modules browseable too. "
          <> "OUTPUT: {module, count, entries:[\"name :: type\"]}; "
          <> "status='no_match' when the module is not in this project or "
          <> "the current package environment. "
          <> "SEE ALSO: ghc_info, hoogle_search, ghc_add_import."
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

handle :: ToolEnv -> Value -> IO ToolResult
handle env rawArgs = do
  ghcSess <- teSession env
  runHandle ghcSess rawArgs

runHandle :: GhcSession -> Value -> IO ToolResult
runHandle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind err)
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) })))
  Right (BrowseArgs m) -> do
    let root = unProjectDir (gsProject ghcSess)
    -- Primary: look in the compile graph (project-own modules).
    eRes <- try (withGhcSession ghcSess (queryBrowseGraph root m))
    case eRes of
      Left (se :: SomeException) ->
        pure $ Env.toolResponseToResult $
          Env.mkFailed
            ((Env.mkErrorEnvelope Env.InternalError
                (T.pack ("GHC API error: " <> show se)))
                  { Env.eeCause = Just (T.pack (show se)) })
      Right (Just entries) ->
        pure $ Env.toolResponseToResult (Env.mkOk (browsePayload m entries))
      Right Nothing -> do
        -- #168 fallback: try the session's package environment.
        -- lookupModule throws when the module is completely unknown,
        -- which we catch at the IO level via try.  If it succeeds,
        -- getModuleInfo gives us the exports just like the graph path.
        eFallback <- try (withGhcSession ghcSess (queryBrowseFallback m))
                       :: IO (Either SomeException (Maybe [Text]))
        pure $ Env.toolResponseToResult $ case eFallback of
          Right (Just entries) ->
            Env.mkOk (browsePayload m entries)
          _ ->
            -- Issue #72 + #90: module not found anywhere — status='no_match'.
            Env.withNextStep moduleNotInGraphNextStep
              (Env.mkNoMatch (moduleNotInGraphPayload m))

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

-- | Primary browse path: look for the module in the compile graph,
-- restricted to source files under the project root.  Filtering by
-- project root prevents browsing stray external-package modules that
-- some CI environments include in the graph (e.g. @Prelude@ from
-- @base@ in GHC-from-source builds).
queryBrowseGraph :: FilePath -> Text -> Ghc (Maybe [Text])
queryBrowseGraph projectRoot nm = do
  let wanted = mkModuleName (T.unpack nm)
  mg <- getModuleGraph
  let matches =
        [ ms_mod ms
        | ms <- mgModSummaries mg
        , moduleName (ms_mod ms) == wanted
        , projectRoot `isPrefixOf` ms_hspp_file ms
        ]
  case matches of
    []      -> pure Nothing
    (m : _) -> browseModuleInfo m

-- | #168 fallback: try the session's loaded package environment via
-- 'lookupModule'.  Called only when 'queryBrowseGraph' returns
-- 'Nothing'. Covers session-preloaded modules (Prelude, Data.Map, …)
-- that exist in the GHC package environment but are not part of the
-- project's own compile graph.
--
-- 'lookupModule' throws a 'SourceError' when the module is completely
-- unknown; the caller catches that at the 'IO' level.
queryBrowseFallback :: Text -> Ghc (Maybe [Text])
queryBrowseFallback nm = do
  let wanted = mkModuleName (T.unpack nm)
  m <- lookupModule wanted Nothing
  browseModuleInfo m

browseModuleInfo :: Module -> Ghc (Maybe [Text])
browseModuleInfo m = do
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
