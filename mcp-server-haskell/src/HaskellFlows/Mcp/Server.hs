-- | Tool dispatch + shared server state.
--
-- Mirrors the role of @mcp-server/src/index.ts@: owns the
-- @projectDir@, owns the GHCi session singleton, and routes
-- JSON-RPC methods to handlers.
--
-- Important invariant ported from the TS audit (finding A1): @projectDir@
-- is held in a 'TVar', not a top-level @let@ binding, so concurrent
-- reads/writes serialise under STM. Tool handlers capture a snapshot of
-- the value under a transaction so a mid-flight @tools/call@ can not see
-- a half-switched project.
module HaskellFlows.Mcp.Server
  ( Server
  , defaultServer
  , serverFor
  , handleRequest
    -- * Dispatch (re-exported so ghc_batch can recurse)
  , dispatchTool
    -- * Canonical tool registry (shared with ghc_workflow's status view)
  , allToolDescriptors
  , allToolNameTexts
    -- * Per-tool timeout envelope (F-12 defence)
  , toolTimeoutMicros
    -- * GHC-API session (Phase-1 scaffolding, docs/GHC-API-rewrite-plan.md)
  , getOrStartGhcSession
  , evictGhcSession
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Foldable (for_, toList)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getCurrentDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.Timeout (timeout)

import HaskellFlows.Data.PropertyStore (Store, openStore)
import HaskellFlows.Mcp.Logging
  ( LogContext (..)
  , logInternalEvent
  , logToolEnd
  , logToolStart
  , newLogContext
  , redactArgs
  )
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , invalidateLoadCache
  , invalidateStanzaFlags
  , killGhcSession
  , startGhcSession
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ErrorKind (ErrorKind (..))
import HaskellFlows.Mcp.Guidance (sessionInstructionsText, workflowRulesMarkdown)
import HaskellFlows.Mcp.NextStep (injectNextStep, suggestNext)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ResourceUri (ResourceUri (..), parseResourceUri)
import HaskellFlows.Mcp.Resources (allResources)
import HaskellFlows.Mcp.RpcMethod
  ( RpcMethod (..)
  , isNotification
  , parseRpcMethod
  )
import HaskellFlows.Mcp.Staleness (checkStaleness)
import HaskellFlows.Mcp.ToolName
  ( ToolName (..)
  , allToolNameTexts
  , parseToolName
  , toolNameText
  , toolVersion
  )
import HaskellFlows.Mcp.WorkflowState
  ( WorkflowStateRef
  , newWorkflowStateRef
  , readState
  , trackTool
  )
import HaskellFlows.Types (ProjectDir, mkProjectDir, unProjectDir)
import qualified HaskellFlows.Tool.AddImport       as AddImportTool
import qualified HaskellFlows.Tool.AddModules      as AddModulesTool
import qualified HaskellFlows.Tool.Modules         as ModulesTool
import qualified HaskellFlows.Tool.ApplyExports    as ApplyExportsTool
import qualified HaskellFlows.Tool.Arbitrary       as ArbitraryTool
import qualified HaskellFlows.Tool.Batch           as BatchTool
import qualified HaskellFlows.Tool.Bootstrap       as BootstrapTool
import qualified HaskellFlows.Tool.Browse          as BrowseTool
import qualified HaskellFlows.Tool.Determinism     as DeterminismTool
import qualified HaskellFlows.Tool.PropertyLifecycle as PropertyLifecycleTool
import qualified HaskellFlows.Tool.ToolchainWarmup as ToolchainWarmupTool
import qualified HaskellFlows.Tool.CheckModule     as CheckModuleTool
import qualified HaskellFlows.Tool.CheckProject    as CheckProjectTool
import qualified HaskellFlows.Tool.Complete        as CompleteTool
import qualified HaskellFlows.Tool.Coverage        as CoverageTool
import qualified HaskellFlows.Tool.CreateProject   as CreateProjectTool
import qualified HaskellFlows.Tool.Deps            as DepsTool
import qualified HaskellFlows.Tool.Doc             as DocTool
import qualified HaskellFlows.Tool.Eval            as EvalTool
import qualified HaskellFlows.Tool.FixWarning      as FixWarningTool
import qualified HaskellFlows.Tool.Format          as FormatTool
import qualified HaskellFlows.Tool.Gate            as GateTool
import qualified HaskellFlows.Tool.Goto            as GotoTool
import qualified HaskellFlows.Tool.Hole            as HoleTool
import qualified HaskellFlows.Tool.Hoogle          as HoogleTool
import qualified HaskellFlows.Tool.Imports         as ImportsTool
import qualified HaskellFlows.Tool.Info            as InfoTool
import qualified HaskellFlows.Tool.Lint            as LintTool
import qualified HaskellFlows.Tool.Load            as Load
import qualified HaskellFlows.Tool.QuickCheck      as QcTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExportTool
import qualified HaskellFlows.Tool.Refactor        as RefactorTool
import qualified HaskellFlows.Tool.Move             as MoveTool
import qualified HaskellFlows.Tool.DepsExplain      as DepsExplainTool
import qualified HaskellFlows.Tool.Lab              as LabTool
import qualified HaskellFlows.Tool.ExplainError     as ExplainErrorTool
import qualified HaskellFlows.Tool.Perf             as PerfTool
import qualified HaskellFlows.Tool.PropertyAudit    as PropertyAuditTool
import qualified HaskellFlows.Tool.Witness          as WitnessTool
import qualified HaskellFlows.Tool.Regression      as RegressionTool
import qualified HaskellFlows.Tool.RemoveModules   as RemoveModulesTool
import qualified HaskellFlows.Tool.Suggest         as SuggestTool
import qualified HaskellFlows.Mcp.PathBootstrap    as PathBootstrap
import qualified HaskellFlows.Tool.SwitchProject   as SwitchProjectTool
import qualified HaskellFlows.Tool.ToolchainStatus as ToolchainStatusTool
import qualified HaskellFlows.Tool.Type            as TypeTool
import qualified HaskellFlows.Tool.ValidateCabal   as ValidateCabalTool
import qualified HaskellFlows.Tool.Workflow        as WorkflowTool

-- | All mutable server state.
--
-- 'srvProjectDir' is an 'IORef' because Phase-1 doesn't yet support
-- runtime project switching through a tool — we'll upgrade it to a TVar
-- the moment we port 'ghc_switch_project'.
--
-- 'srvGhcSession' is held behind an 'MVar' so concurrent handlers
-- cannot race on startup: the first caller wins, everyone else waits
-- on the mutex.
--
-- 'srvBootPosix' + 'srvBinaryPath' wire BUG-07's staleness check:
-- @ghc_workflow(status)@ now compares the on-disk binary mtime
-- with the boot time and flags the user if they rebuilt the MCP
-- but forgot to relaunch the host.
data Server = Server
  { srvProjectDir    :: !(IORef ProjectDir)
  , srvGhcSession    :: !(MVar (Maybe GhcSession))
    -- ^ In-process GHC-API session — the single source of compile
    -- and execute state after Wave 5 landed. All 25 tools route
    -- through this; the legacy subprocess ghci module is gone.
  , srvStore         :: !(IORef Store)
    -- ^ Issue #39: 'IORef Store' (not bare 'Store') because
    -- 'ghc_switch_project' must be able to reopen the property
    -- store against the new project root. Without this, the
    -- previous project's @.haskell-flows/properties.json@ would
    -- still drive 'ghc_check_module' / 'ghc_regression' / gates
    -- after a switch — surfacing ghost regressions for properties
    -- that don't even live in the new project.
  , srvWorkflowState :: !WorkflowStateRef
  , srvBootPosix     :: !Double
  , srvBinaryPath    :: !FilePath
  }

-- | Build a server whose project directory is sourced from
-- @HASKELL_PROJECT_DIR@ or the current working directory (mirrors TS
-- @src/index.ts@). Rejects a relative value up front — no lazy errors.
--
-- Also captures two facts used by BUG-07's staleness check:
-- the server's boot time (POSIX seconds) and the absolute path of
-- the running binary. Both are deployment-level details —
-- 'getExecutablePath' is what @ghc_workflow(status)@ will later
-- stat to detect a rebuild the host hasn't relaunched against.
defaultServer :: IO Server
defaultServer = do
  envVal <- lookupEnv "HASKELL_PROJECT_DIR"
  cwd    <- getCurrentDirectory
  let raw = fromMaybe cwd envVal
  serverForRaw raw

-- | Construct a server explicitly anchored at the given project
-- directory, bypassing the @HASKELL_PROJECT_DIR@ env var. Used by
-- the e2e test harness where multiple in-process Servers run
-- concurrently — mutating the global env var to communicate the
-- target dir is a race (two threads' setEnv calls would interleave
-- and the second 'defaultServer' would read the wrong value).
-- Production callers keep using 'defaultServer'.
serverFor :: FilePath -> IO Server
serverFor = serverForRaw

serverForRaw :: FilePath -> IO Server
serverForRaw raw = do
  -- BUG-04 fix: augment PATH before any subprocess-invoking tool
  -- touches the environment. Hosts launched from macOS Dock /
  -- Finder pass a minimal PATH that omits ~/.ghcup/bin and
  -- ~/.cabal/bin; without this, 'ghc_lint', 'ghc_quickcheck',
  -- 'ghc_regression', 'ghc_gate', 'ghc_coverage',
  -- 'ghc_validate_cabal' all fail with
  -- "posix_spawnp: does not exist".
  _ <- PathBootstrap.augmentPath
  case mkProjectDir raw of
    Left err -> error ("Could not build ProjectDir: " <> show err)
    Right pd -> do
      pdRef    <- newIORef pd
      ghcSess  <- newMVar Nothing
      store    <- openStore pd
      storeRef <- newIORef store
      ws       <- newWorkflowStateRef
      bootPos  <- realToFrac <$> getPOSIXTime
      binPath  <- getExecutablePath
      pure Server
        { srvProjectDir    = pdRef
        , srvGhcSession    = ghcSess
        , srvStore         = storeRef
        , srvWorkflowState = ws
        , srvBootPosix     = bootPos
        , srvBinaryPath    = binPath
        }

-- | Dispatch a single parsed request. 'Nothing' means the input was a
-- notification (e.g. @initialized@) and the caller should not write a
-- reply.
--
-- Method routing goes through 'parseRpcMethod' first: any wire-string
-- outside the closed 'RpcMethod' enumeration short-circuits to either
-- a silent drop (for notifications shaped requests with no id) or
-- @methodNotFoundErr@. After the parse, 'dispatch' is exhaustive over
-- 'RpcMethod' so a new method requires a new handler clause — typos
-- can not silently degrade to "method not found".
handleRequest :: Server -> Request -> IO (Maybe Response)
handleRequest srv req = case parseRpcMethod (reqMethod req) of
  Just m
    | isNotification m, Nothing <- reqId req ->
        -- spec-compliant notification — no reply
        pure Nothing
    | otherwise -> case reqId req of
        Nothing  ->
          -- request shaped as notification (no id) for a non-notification
          -- method; MCP spec says ignore silently
          pure Nothing
        Just rid -> Just <$> dispatch srv m (reqParams req) rid
  Nothing -> case reqId req of
    -- unknown method without an id is a notification we don't understand;
    -- spec says ignore (don't reply with an error)
    Nothing  -> pure Nothing
    Just rid -> pure (Just (err_ rid (methodNotFoundErr (reqMethod req))))

dispatch :: Server -> RpcMethod -> Maybe Value -> RequestId -> IO Response
dispatch _ Initialize _ rid =
  pure $ ok rid $ toJSON $
    InitializeResult
      { irProtocolVersion = "2024-11-05"
      , irServerInfo      =
          ServerInfo
            { siName    = "haskell-flows"
            , siVersion = "0.1.0-haskell"
            }
      , irInstructions    =
          Just (sessionInstructionsText allToolDescriptors)
      }
dispatch _ ToolsList _ rid =
  pure $ ok rid $ object [ "tools" .= allToolDescriptors ]
dispatch _ ResourcesList _ rid =
  pure $ ok rid $ object [ "resources" .= allResources ]
dispatch _ ResourcesRead (Just params) rid =
  case parseEither parseJSON params :: Either String Value of
    Left err -> pure (err_ rid (invalidParamsErr (T.pack err)))
    Right v  -> case v of
      Object o -> case KeyMap.lookup "uri" o of
        Just (String u) -> case parseResourceUri u of
          Just uri ->
            let contents = renderResource uri
            in pure $ ok rid $ object
                 [ "contents" .= [ object
                     [ "uri"      .= u
                     , "mimeType" .= ("text/markdown" :: Text)
                     , "text"     .= contents
                     ] ]
                 ]
          Nothing -> pure (err_ rid (invalidParamsErr ("unknown resource uri: " <> u)))
        _ -> pure (err_ rid (invalidParamsErr "resources/read requires a `uri` string"))
      _ -> pure (err_ rid (invalidParamsErr "resources/read params must be an object"))
  where
    -- Dispatch resource URIs to their dynamically-rendered bodies.
    -- Keeping this inline (not in Resources.hs) avoids a cyclic
    -- import: Resources advertises URI metadata; Server owns the
    -- renderer because it has access to 'allToolDescriptors'.
    --
    -- Exhaustive over 'ResourceUri': adding a new resource here is a
    -- compile-time obligation, not a runtime "unknown URI" surprise.
    renderResource :: ResourceUri -> Text
    renderResource = \case
      WorkflowRules -> workflowRulesMarkdown allToolDescriptors
dispatch _ ResourcesRead Nothing rid =
  pure (err_ rid (invalidParamsErr "resources/read requires params"))
dispatch srv ToolsCall (Just params) rid =
  case parseEither parseJSON params of
    Left err -> pure (err_ rid (invalidParamsErr (T.pack err)))
    Right call -> handleToolCall srv call rid
dispatch _ ToolsCall Nothing rid =
  pure (err_ rid (invalidParamsErr "tools/call requires params"))
-- Notifications should never reach 'dispatch' (filtered by 'handleRequest'
-- before we get an id). If they do, treat as a protocol violation: the
-- client sent an id alongside a notification-shaped method.
dispatch _ Initialized _ rid =
  pure (err_ rid (invalidParamsErr "'initialized' is a notification and must not carry an id"))
dispatch _ NotificationsCancelled _ rid =
  pure (err_ rid (invalidParamsErr "'notifications/cancelled' is a notification and must not carry an id"))

handleToolCall :: Server -> ToolCall -> RequestId -> IO Response
handleToolCall srv call rid = case parseToolName (tcName call) of
  Nothing ->
    -- Wire sent a tool name we don't have a constructor for. Synthesize
    -- the same shape 'dispatchTool' would have emitted via its old
    -- catch-all branch. We skip 'runTool' (no state tracking, no
    -- nextStep injection — there is no canonical ToolName to key by).
    pure (ok rid (toJSON (unknownToolResult (tcName call))))
  Just GhcBatch -> do
    -- Special case: batch has to be routed here (not inside
    -- dispatchTool) because it needs the dispatcher as a callback
    -- and dispatchTool would recurse with no termination on
    -- ghc_batch-in-ghc_batch. The batch tool itself refuses
    -- nesting but we also keep the top-level routing explicit.
    --
    -- Batch owns the slowest envelope: it's a bag of N tool calls,
    -- each of which already has its own per-call budget via
    -- 'dispatchTool' -> 'runTool'. A global 6-minute bound here is
    -- the defence of last resort against a pathological batch, not
    -- the per-action cap.
    --
    -- Issue #98 Phase B: emit tool_call_start / tool_call_end log
    -- events and splice trace_id into the ToolResponse meta.
    ctx  <- newLogContext "ghc_batch"
    logToolStart ctx (redactArgs (tcArguments call))
    t0   <- getPOSIXTime
    resp <- runTool srv GhcBatch rid
              (BatchTool.handle (dispatchTool srv) (tcArguments call))
    t1   <- getPOSIXTime
    logToolEnd ctx (extractResponseStatus resp)
               (round ((t1 - t0) * 1000) :: Int)
    pure (injectTraceId GhcBatch (lcTraceId ctx) resp)
  Just tn -> do
    -- Issue #98 Phase B: emit tool_call_start / tool_call_end log
    -- events and splice trace_id into the ToolResponse meta.
    -- Issue #99 Phase B: also splice meta.tool_version (the per-tool
    -- semver from 'toolVersion').
    ctx  <- newLogContext (toolNameText tn)
    logToolStart ctx (redactArgs (tcArguments call))
    t0   <- getPOSIXTime
    resp <- runTool srv tn rid (dispatchByName srv (tcArguments call) tn)
    t1   <- getPOSIXTime
    logToolEnd ctx (extractResponseStatus resp)
               (round ((t1 - t0) * 1000) :: Int)
    pure (injectTraceId tn (lcTraceId ctx) resp)

-- | Pure (non-response-wrapping) tool dispatcher. Exposed so
-- 'HaskellFlows.Tool.Batch' can recurse without pulling Server's
-- Response envelope. Unknown tool names return a structured error
-- 'ToolResult' rather than raising — that way a ghc_batch run with
-- one bad action still completes the remaining good ones.
dispatchTool :: Server -> ToolCall -> IO ToolResult
dispatchTool srv call = case parseToolName (tcName call) of
  Nothing -> pure (unknownToolResult (tcName call))
  Just tn -> dispatchByName srv (tcArguments call) tn

-- | Exhaustive dispatch from 'ToolName' to the corresponding handler.
-- @-Wincomplete-patterns@ guarantees that adding a constructor to
-- 'ToolName' without wiring its handler is a compile error — pre-ADT
-- the same omission silently fell through to "Unknown tool".
dispatchByName :: Server -> Value -> ToolName -> IO ToolResult
dispatchByName srv args = \case
  GhcLoad -> do
    -- Wave-2 full GhcSession: cabal-aware stanza compile via
    -- loadForTarget, diagnostics captured from the logger hook,
    -- rendered to GHCi-style text so agents still see 'raw' output.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    Load.handle ghcSess pd args
  GhcType -> do
    -- Phase-2 migrated: reads from the in-process GHC API session,
    -- not the legacy subprocess ghci. Auto-load on first call keeps
    -- the FlowExploratory 'type(localBinding)' scenario green.
    ghcSess <- getOrStartGhcSession srv
    TypeTool.handle ghcSess args
  GhcInfo -> do
    -- Phase-2 migrated: getInfo + TyThing classification.
    ghcSess <- getOrStartGhcSession srv
    InfoTool.handle ghcSess args
  GhcEval -> do
    -- Wave-5 full in-process. Fast path: show-wrap + compileExpr.
    -- Fallback: evalIOString (for IO-typed expressions).
    ghcSess <- getOrStartGhcSession srv
    EvalTool.handle ghcSess args
  GhcQuickCheck -> do
    -- Wave-3 full in-process: compileExpr + unsafeCoerce of a
    -- Test.QuickCheck.quickCheckWithResult invocation.
    ghcSess <- getOrStartGhcSession srv
    store   <- readIORef (srvStore srv)
    QcTool.handle store ghcSess args
  GhcHole -> do
    -- Wave-2 full GhcSession: Deferred compile via stanza flags,
    -- diagnostics captured through the logger hook, rendered to
    -- GHCi-style text for parseTypedHoles.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    HoleTool.handle ghcSess pd args
  GhcArbitrary -> do
    -- Wave-4 full GhcSession: parseName + getInfo + showPprUnsafe.
    ghcSess <- getOrStartGhcSession srv
    ArbitraryTool.handle ghcSess args
  HoogleSearch ->
    HoogleTool.handle args
  GhcWorkflow -> do
    ws        <- readState (srvWorkflowState srv)
    staleness <- checkStaleness (srvBinaryPath srv) (srvBootPosix srv)
    WorkflowTool.handle
      (srvProjectDir srv)
      (srvGhcSession srv)
      allToolNameTexts
      ws
      staleness
      args
  GhcRegression -> do
    -- Wave-3 full in-process replay via evalIOString.
    ghcSess <- getOrStartGhcSession srv
    store   <- readIORef (srvStore srv)
    RegressionTool.handle store ghcSess args
  GhcCheckModule -> do
    -- Wave-5 full GhcSession: compile/warnings/holes + in-process
    -- property replay via Regression.runOne.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    store   <- readIORef (srvStore srv)
    CheckModuleTool.handle ghcSess store pd args
  GhcCoverage -> do
    pd <- readIORef (srvProjectDir srv)
    CoverageTool.handle pd args
  GhcComplete -> do
    -- Phase-2 migrated: in-process getNamesInScope + prefix filter.
    ghcSess <- getOrStartGhcSession srv
    CompleteTool.handle ghcSess args
  GhcFormat -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- FormatTool.handle pd args
    invalidateGhcSessionIfPresent srv
    pure r
  GhcDeps -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- DepsTool.handle pd args
    -- Stanza flags hold the resolved package set; ghc_deps just
    -- changed it, so re-bootstrap on next session use.
    invalidateStanzaFlagsIfPresent srv
    pure r
  GhcCreateProject -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- CreateProjectTool.handle pd args
    -- New project = completely different stanza set.
    invalidateStanzaFlagsIfPresent srv
    pure r
  GhcDoc -> do
    -- Phase-2 migrated: GHC.getDocs on the resolved Name.
    ghcSess <- getOrStartGhcSession srv
    DocTool.handle ghcSess args
  GhcGoto -> do
    -- Phase-2 migrated: in-process Name -> nameSrcSpan lookup.
    ghcSess <- getOrStartGhcSession srv
    GotoTool.handle ghcSess args
  GhcRefactor -> do
    -- Wave-5 full GhcSession: compile-verify via loadForTarget.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    RefactorTool.handle ghcSess pd args
  GhcMove -> do
    -- Issue #62 Phase 1: cross-module top-level symbol move with
    -- multi-file snapshot + verify-via-loadForTarget rollback.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    MoveTool.handle ghcSess pd args
  GhcDepsExplain -> do
    -- Issue #63 Phase 1: cabal solver-output translator. Spawns
    -- 'cabal v2-build --dry-run' under Proc.cwd = projectDir;
    -- otherwise pure parsing. No GhcSession needed.
    pd <- readIORef (srvProjectDir srv)
    DepsExplainTool.handle pd args
  GhcLab -> do
    -- Issue #60 Phase 1: module-wide property audit. Composes
    -- Suggest.applyRules + Tool.QuickCheck per top-level binding.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    store   <- readIORef (srvStore srv)
    LabTool.handle ghcSess store pd args
  GhcExplainError -> do
    -- Issue #59 Phase 1: structured explanation-context builder.
    -- The agent's own LLM consumes the response and proposes
    -- candidates; Phase 2 will add a verify endpoint.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    ExplainErrorTool.handle ghcSess pd args
  GhcPerf -> do
    -- Issue #61 Phase 2: wall-clock perf harness with baseline persistence
    -- and regression detection (>10% slower triggers status='refused').
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    PerfTool.handle ghcSess pd args
  GhcPropertyAudit -> do
    -- Issue #64 Phase 1: pairwise contradiction detector over the
    -- persisted property store. Re-uses the QuickCheck cabal-repl
    -- vehicle for each pair-probe.
    ghcSess <- getOrStartGhcSession srv
    store   <- readIORef (srvStore srv)
    PropertyAuditTool.handle store ghcSess args
  GhcWitness -> do
    -- Issue #65 Phase 1: property-witness explorer. Wraps the
    -- property with size-bucket instrumentation and runs it via
    -- the cabal-repl harness so we get QuickCheck's label histogram
    -- in the formatted output.
    ghcSess <- getOrStartGhcSession srv
    WitnessTool.handle ghcSess args
  GhcLint -> do
    pd <- readIORef (srvProjectDir srv)
    LintTool.handle pd args
  GhcToolchainStatus ->
    ToolchainStatusTool.handle args
  GhcValidateCabal -> do
    pd <- readIORef (srvProjectDir srv)
    ValidateCabalTool.handle pd args
  GhcCheckProject -> do
    -- Wave-5 full GhcSession (delegates to check_module per file).
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    store   <- readIORef (srvStore srv)
    CheckProjectTool.handle ghcSess store pd args
  GhcSuggest -> do
    -- Wave-5 full GhcSession: exprType + module-graph walk for siblings.
    ghcSess <- getOrStartGhcSession srv
    SuggestTool.handle ghcSess args
  GhcGate -> do
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    store   <- readIORef (srvStore srv)
    GateTool.handle store ghcSess pd args
  GhcQuickCheckExport -> do
    pd    <- readIORef (srvProjectDir srv)
    store <- readIORef (srvStore srv)
    QcExportTool.handle store pd args
  GhcAddImport -> do
    r <- AddImportTool.handle args
    invalidateGhcSessionIfPresent srv
    pure r
  GhcAddModules -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- AddModulesTool.handle pd args
    -- Changes exposed-modules in .cabal, so stanza flags need
    -- re-bootstrap to capture the new unit-id / include path set.
    invalidateStanzaFlagsIfPresent srv
    pure r
  GhcRemoveModules -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- RemoveModulesTool.handle pd args
    invalidateStanzaFlagsIfPresent srv
    pure r
  GhcModules -> do
    -- #94 Phase B: action-discriminated successor to GhcAddModules /
    -- GhcRemoveModules. ModulesTool.handle dispatches on
    -- args.action ∈ {"add","remove"} and forwards to the legacy
    -- handlers, so behaviour + side-effects (stanza-flag
    -- invalidation) match the legacy path exactly.
    pd <- readIORef (srvProjectDir srv)
    r  <- ModulesTool.handle pd args
    invalidateStanzaFlagsIfPresent srv
    pure r
  GhcApplyExports -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- ApplyExportsTool.handle pd args
    invalidateGhcSessionIfPresent srv
    pure r
  GhcFixWarning -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- FixWarningTool.handle pd args
    invalidateGhcSessionIfPresent srv
    pure r
  GhcImports -> do
    -- Phase-6 migrated: reads from GhcSession's interactive context.
    ghcSess <- getOrStartGhcSession srv
    ImportsTool.handle ghcSess args
  GhcBrowse -> do
    -- Phase-2 migrated: in-process getModuleInfo + modInfoExports.
    ghcSess <- getOrStartGhcSession srv
    BrowseTool.handle ghcSess args
  GhcDeterminism -> do
    -- Wave-3 full in-process via evalIOString.
    ghcSess <- getOrStartGhcSession srv
    DeterminismTool.handle ghcSess args
  GhcPropertyLifecycle -> do
    store <- readIORef (srvStore srv)
    PropertyLifecycleTool.handle store args
  GhcToolchainWarmup ->
    ToolchainWarmupTool.handle args
  GhcBootstrap -> do
    pd <- readIORef (srvProjectDir srv)
    BootstrapTool.handle pd allToolDescriptors args
  GhcSwitchProject ->
    -- SwitchProject is the one tool that mutates the project-dir
    -- ref, the session MVar, AND (issue #39) the property store
    -- ref — it takes those handles directly instead of going
    -- through getOrStartGhcSession, which would boot a fresh
    -- session against the OLD path right before we tear it down.
    SwitchProjectTool.handle
      (srvProjectDir srv)
      (srvGhcSession srv)
      (srvStore srv)
      args
  GhcBatch ->
    -- Reachable only via 'dispatchTool' (e.g. when 'BatchTool.handle'
    -- ever recursed into the dispatcher). 'BatchTool' itself rejects
    -- nesting; this arm exists to keep the case exhaustive.
    BatchTool.handle (dispatchTool srv) args

-- | Synthesize an error 'ToolResult' for an unknown tool name.
-- Pulled out so 'handleToolCall' and 'dispatchTool' produce the
-- exact same shape on the wire.
unknownToolResult :: Text -> ToolResult
unknownToolResult name =
  ToolResult
    { trContent = [ TextContent ("Unknown tool: " <> name) ]
    , trIsError = True
    }

-- ---------------------------------------------------------------------------
-- Logging helpers (Issue #98 Phase B)
-- ---------------------------------------------------------------------------

-- | Navigate the nested JSON path
--   Response → ToolResult JSON → content[0].text → ToolResponse JSON → status
-- and return the @status@ text field.
--
-- Falls back to @"ok"@ if any step of the path is missing or mistyped —
-- for example the unknown-tool result whose content is plain text rather than
-- a JSON-encoded 'ToolResponse'. The fallback is intentionally benign: the
-- log entry is still emitted; it just carries a slightly inaccurate status.
extractResponseStatus :: Response -> Text
extractResponseStatus resp = fromMaybe "ok" $ do
  v <- either (const Nothing) Just (respPayload resp)
  case v of
    Object top -> case KeyMap.lookup "content" top of
      Just (Array arr) -> case toList arr of
        (Object c : _) -> case KeyMap.lookup "text" c of
          Just (String txt) ->
            case decode (BL.fromStrict (TE.encodeUtf8 txt)) of
              Just (Object inner) -> case KeyMap.lookup "status" inner of
                Just (String s) -> Just s
                _               -> Nothing
              _                   -> Nothing
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

-- | Splice @trace_id@ and @tool_version@ into the @meta@ object of the
-- 'ToolResponse' that is encoded as the first 'TextContent' block
-- inside an MCP 'Response'.
--
-- * If the ToolResponse already has a @meta@ object, the keys are
--   inserted (or replaced) there without disturbing any other meta fields.
-- * If there is no @meta@ object yet, a minimal one is created containing
--   only the spliced keys.
-- * The 'Response' is returned unchanged if any step of the JSON path fails
--   (e.g. an error response at the RPC level, or a non-JSON text block).
--
-- Issue #99 Phase B: every tool response now carries
-- @meta.tool_version@ alongside @meta.trace_id@, so consumers can
-- detect which tool semver produced the payload (independently of
-- the binary-level @--version@ surface).
injectTraceId :: ToolName -> Text -> Response -> Response
injectTraceId tn tid resp = case respPayload resp of
  Left _  -> resp
  Right v ->
    let v' = case v of
               Object top ->
                 case KeyMap.lookup "content" top of
                   Just (Array arr) ->
                     let arr' = fmap patchItem arr
                     in Object (KeyMap.insert "content" (Array arr') top)
                   _ -> v
               _ -> v
    in resp { respPayload = Right v' }
  where
    ver = toolVersion tn

    patchItem item = case item of
      Object c ->
        case KeyMap.lookup "text" c of
          Just (String txt) ->
            Object (KeyMap.insert "text" (String (patchText txt)) c)
          _ -> item
      _ -> item

    -- Re-encode the ToolResponse JSON with trace_id + tool_version
    -- spliced into meta.
    patchText txt =
      case decode (BL.fromStrict (TE.encodeUtf8 txt)) of
        Just (Object inner) ->
          let meta = case KeyMap.lookup "meta" inner of
                Just (Object m) ->
                  Object (KeyMap.insert "tool_version" (String ver)
                          (KeyMap.insert "trace_id"    (String tid) m))
                _ ->
                  object [ "trace_id"     .= tid
                         , "tool_version" .= ver
                         ]
              inner' = KeyMap.insert "meta" meta inner
          in TL.toStrict (TLE.decodeUtf8 (encode (Object inner')))
        _ -> txt

--------------------------------------------------------------------------------
-- tool registry — single source of truth for both tools/list and
-- ghc_workflow's status view. Keep additions in sync with the
-- dispatcher branch in 'dispatchTool'.
--------------------------------------------------------------------------------

allToolDescriptors :: [ToolDescriptor]
allToolDescriptors =
  [ Load.descriptor
  , TypeTool.descriptor
  , InfoTool.descriptor
  , EvalTool.descriptor
  , QcTool.descriptor
  , HoleTool.descriptor
  , ArbitraryTool.descriptor
  , HoogleTool.descriptor
  , WorkflowTool.descriptor
  , RegressionTool.descriptor
  , CheckModuleTool.descriptor
  , CoverageTool.descriptor
  , CompleteTool.descriptor
  , FormatTool.descriptor
  , GateTool.descriptor
  , QcExportTool.descriptor
  , DepsTool.descriptor
  , CreateProjectTool.descriptor
  , DocTool.descriptor
  , GotoTool.descriptor
  , RefactorTool.descriptor
  , MoveTool.descriptor
  , DepsExplainTool.descriptor
  , LabTool.descriptor
  , ExplainErrorTool.descriptor
  , PerfTool.descriptor
  , PropertyAuditTool.descriptor
  , WitnessTool.descriptor
  , BatchTool.descriptor
  , LintTool.descriptor
  , ToolchainStatusTool.descriptor
  , ValidateCabalTool.descriptor
  , CheckProjectTool.descriptor
  , SuggestTool.descriptor
  , SwitchProjectTool.descriptor
  , AddImportTool.descriptor
  , AddModulesTool.descriptor
  , ApplyExportsTool.descriptor
  , FixWarningTool.descriptor
  , ImportsTool.descriptor
  , BrowseTool.descriptor
  , DeterminismTool.descriptor
  , RemoveModulesTool.descriptor
  , ModulesTool.descriptor       -- #94 Phase B: action-discriminated successor
  , BootstrapTool.descriptor
  , PropertyLifecycleTool.descriptor
  , ToolchainWarmupTool.descriptor
  ]

-- 'allToolNames :: [ToolName]' / 'allToolNameTexts :: [Text]' both
-- live in 'HaskellFlows.Mcp.ToolName' and are re-exported through
-- this module's export list. That module derives both from
-- @[minBound..maxBound]@ — adding a new constructor automatically
-- adds it to @tools/list@ (vs. the pre-ADT design where the
-- canonical list was a hand-curated literal list of descriptors
-- that could silently drift from the dispatcher case-arms).

-- Dynamic @initialize.instructions@ + @resources/read@ rendering
-- lives in 'HaskellFlows.Mcp.Guidance'; the Server only wires them
-- into the dispatch table above. Having the guidance derived from
-- 'allToolDescriptors' + 'situationTable' means the text can never
-- drift from the real tool registry (BUG-05).

-- | Last-resort hard ceiling for any tool. This is intentionally
-- generous — 10 minutes — and is NOT meant to be the primary time
-- control for a tool call. Each tool already has its own domain
-- timeouts (e.g. @executeNoLock@'s STM-bound budget, @cabal test --
-- enable-coverage@'s 5-minute cap). This envelope exists only so
-- that a completely pathological handler (an unreachable STM retry,
-- a foreign-code infinite loop, a non-interruptible syscall) cannot
-- hold the main loop hostage indefinitely.
--
-- Picking a tight per-tool value here would be a guessing game that
-- falsely fails legitimate long-running work (a 70s compile on a
-- large module, a slow hoogle, a coverage run that needs 4 minutes);
-- the fix for F-12's hang lives at the root in 'Session.hs'
-- (terminal 'Dead' status + honoured command budget).
toolTimeoutMicros :: Int
toolTimeoutMicros = 10 * 60 * 1_000_000

-- | Common exception shield for every tool handler.
--
-- Prevents a handler crash from taking down the server loop and surfaces
-- it as a structured tool-level error to the client. On 'SessionExhausted'
-- (buffer cap from the DoS guard in Session.hs), we additionally evict
-- the dead session from the MVar so 'getOrStartSession' rebuilds it on
-- the next call — otherwise every subsequent tool call would inherit the
-- Overflowed status and fail identically.
--
-- Additionally (F-12 defence-in-depth): wraps the action in a single
-- generous 'System.Timeout.timeout'. If the handler doesn't finish
-- inside the universal ceiling, we evict the GHCi session (so the
-- next call starts fresh) and return a structured timeout error.
-- The primary F-12 fix lives in 'Session.hs'; this envelope catches
-- whatever that fix misses.
runTool :: Server -> ToolName -> RequestId -> IO ToolResult -> IO Response
runTool srv toolName rid action = do
  out <- try (timeout toolTimeoutMicros action)
           :: IO (Either SomeException (Maybe ToolResult))
  case out of
    Left ex -> do
      -- Any exception that escapes the handler: reset the GhcSession
      -- so the next call starts with a fresh HscEnv, then surface as
      -- a structured error.
      evictGhcSession srv
      pure (ok rid (toJSON (toolException ToolException (T.pack (show ex)))))
    Right Nothing -> do
      evictGhcSession srv
      pure (ok rid (toJSON (toolException Timeout (timeoutMsg toolName))))
    Right (Just tr) -> do
      let payload = firstJsonContent tr
      trackTool (srvWorkflowState srv) toolName (not (trIsError tr)) payload
      pure (ok rid (toJSON (enrichWithNextStep toolName tr)))

-- | Attempt to inject a 'NextStep' hint into the tool's payload.
-- The decision is read-only: we peek at the top text-content block
-- of the result as JSON, pass (toolName, success, payload) to
-- 'suggestNext', and — if it returns 'Just' — splice the
-- @nextStep@ object in. On any shape mismatch (non-JSON payload,
-- missing success flag, etc.) the result is returned unchanged.
enrichWithNextStep :: ToolName -> ToolResult -> ToolResult
enrichWithNextStep toolName tr =
  let payload = firstJsonContent tr
      isOk    = not (trIsError tr)
  in case suggestNext toolName isOk payload of
       Just ns -> injectNextStep ns tr
       Nothing -> tr

-- | Peek at the first TextContent block and decode it as JSON. If
-- the first block is not valid JSON we return 'Null' so the
-- 'suggestNext' decision table has a well-typed default to match
-- against.
firstJsonContent :: ToolResult -> Value
firstJsonContent tr = case trContent tr of
  (TextContent t : _) ->
    fromMaybe Null (decode (BL.fromStrict (TE.encodeUtf8 t)))
  _ -> Null

-- | Human-readable timeout error message for agents.
timeoutMsg :: ToolName -> Text
timeoutMsg tool =
  "Tool '" <> toolNameText tool <> "' exceeded the server's 10-minute hard \
  \ceiling. The GHCi session has been evicted; the next call will \
  \spawn a fresh one. This is a defence-in-depth trip, not the \
  \normal timeout surface — most tools have tighter internal \
  \budgets. If this fires, there is probably a deadlock below this \
  \layer."

-- | Reset the in-process GhcSession. Idempotent; catches any
-- failure so an evict from a watchdog path cannot raise.
--
-- Issue #98 Phase C: emits @ghc_session_evict@ at DEBUG level so
-- operators can observe session churn (evictions triggered by
-- exception handlers or hard timeout trips).
evictGhcSession :: Server -> IO ()
evictGhcSession srv = modifyMVar_ (srvGhcSession srv) $ \case
  Nothing -> pure Nothing
  Just s  -> do
    _ <- try (killGhcSession s) :: IO (Either SomeException ())
    ctx <- newLogContext ""
    logInternalEvent ctx "ghc_session_evict" (object [])
    pure Nothing

-- | Phase-1 analogue of 'getOrStartSession' for the in-process GHC
-- API session. Unused by any tool yet — Phase 2 starts calling this
-- when the first read-only tools (type, info) migrate.
--
-- Issue #98 Phase C: emits @ghc_session_hit@ (cache hit, DEBUG) or
-- @ghc_session_start@ (new session, DEBUG + project_dir).
getOrStartGhcSession :: Server -> IO GhcSession
getOrStartGhcSession srv = modifyMVar (srvGhcSession srv) $ \case
  Just s  -> do
    ctx <- newLogContext ""
    logInternalEvent ctx "ghc_session_hit" (object [])
    pure (Just s, s)
  Nothing -> do
    pd  <- readIORef (srvProjectDir srv)
    ctx <- newLogContext ""
    logInternalEvent ctx "ghc_session_start"
      (object ["project_dir" .= unProjectDir pd])
    s   <- startGhcSession pd
    pure (Just s, s)

-- | Drop the GhcSession auto-load cache iff a session has already
-- been booted. Used by file-mutation tools (add_import, add_modules,
-- remove_modules, apply_exports, create_project, deps, fix_warning,
-- format) so the next Phase-2 read re-scans disk and sees their
-- edits. Intentionally DOES NOT boot a session if one doesn't exist —
-- that would burn an HscEnv for a cache-invalidation side-effect.
--
-- Issue #98 Phase C: emits @ghc_cache_invalidate@ at DEBUG level.
invalidateGhcSessionIfPresent :: Server -> IO ()
invalidateGhcSessionIfPresent srv = do
  m <- readMVar (srvGhcSession srv)
  for_ m $ \s -> do
    ctx <- newLogContext ""
    logInternalEvent ctx "ghc_cache_invalidate" (object ["kind" .= ("load_cache" :: Text)])
    invalidateLoadCache s

-- | Heavier-hammer cousin for tools that change the .cabal dep
-- graph or stanza layout. Forces a re-bootstrap of stanza flags
-- and a fresh HscEnv on the next session use.
--
-- Issue #98 Phase C: emits @ghc_cache_invalidate@ (stanza_flags) at DEBUG.
invalidateStanzaFlagsIfPresent :: Server -> IO ()
invalidateStanzaFlagsIfPresent srv = do
  m <- readMVar (srvGhcSession srv)
  for_ m $ \s -> do
    ctx <- newLogContext ""
    logInternalEvent ctx "ghc_cache_invalidate" (object ["kind" .= ("stanza_flags" :: Text)])
    invalidateStanzaFlags s

--------------------------------------------------------------------------------
-- small response helpers
--------------------------------------------------------------------------------

ok :: RequestId -> Value -> Response
ok rid v = Response { respId = rid, respPayload = Right v }

err_ :: RequestId -> RpcError -> Response
err_ rid e = Response { respId = rid, respPayload = Left e }

-- | Structured tool-level error payload used when 'runTool' catches
-- an exception or a timeout elapses. Pre-fix this returned a raw
-- @"Tool threw an exception: X"@ plain string, which broke every
-- client that expected the per-tool JSON envelope
-- (@{"success":false,"error":"…"}@) — a timeout was
-- indistinguishable from a rename-local failure at the schema level.
-- The payload now carries:
--
-- * @success@  — always @false@, matches the per-tool error shape
-- * @error@    — human-readable detail (unchanged text)
-- * @error_kind@ — machine-readable tag: @"session_exhausted"@,
--                  @"timeout"@, or @"tool_exception"@.
--
-- Found by 'Scenarios.FlowTimeoutEnforcement' in the e2e suite.
-- | Issue #90: route exception responses through the same
-- envelope every other tool emits. The legacy ErrorKind (this
-- file's import) has 3 values; map each to the closed-enum
-- 'Env.ErrorKind':
--   * 'Timeout'          → Env.OuterTimeout (status='timeout')
--   * 'SessionExhausted' → Env.SessionExhausted (status='failed')
--   * 'ToolException'    → Env.InternalError (status='failed')
toolException :: ErrorKind -> Text -> ToolResult
toolException kind msg =
  let envKind = case kind of
        Timeout          -> Env.OuterTimeout
        SessionExhausted -> Env.SessionExhausted
        ToolException    -> Env.InternalError
      envErr = (Env.mkErrorEnvelope envKind
                  ("Tool threw an exception: " <> msg))
                    { Env.eeCause = Just msg }
      response = case kind of
        Timeout -> Env.mkTimeout envErr
        _       -> Env.mkFailed  envErr
  in Env.toolResponseToResult response
