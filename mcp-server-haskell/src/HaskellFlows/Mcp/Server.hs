-- | Tool dispatch + shared server state.
--
-- Mirrors the role of @mcp-server/src/index.ts@: owns the @projectDir@,
-- owns the GHCi session singleton, and routes JSON-RPC methods to handlers.
--
-- Important invariant ported from the TS audit (finding A1): @projectDir@
-- is held in a 'TVar', not a top-level @let@ binding, so concurrent
-- reads/writes serialise under STM. Tool handlers capture a snapshot of
-- the value under a transaction so a mid-flight @tools/call@ can not see
-- a half-switched project.
module HaskellFlows.Mcp.Server
  ( Server
  , defaultServer
  , handleRequest
    -- * Dispatch (re-exported so ghci_batch can recurse)
  , dispatchTool
    -- * Canonical tool registry (shared with ghci_workflow's status view)
  , allToolDescriptors
  , allToolNames
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
import Data.Foldable (for_)
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
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , invalidateLoadCache
  , invalidateStanzaFlags
  , killGhcSession
  , startGhcSession
  )
import HaskellFlows.Mcp.Guidance (sessionInstructionsText, workflowRulesMarkdown)
import HaskellFlows.Mcp.NextStep (injectNextStep, suggestNext)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.Resources (allResources)
import HaskellFlows.Mcp.Staleness (checkStaleness)
import HaskellFlows.Mcp.WorkflowState
  ( WorkflowStateRef
  , newWorkflowStateRef
  , readState
  , trackTool
  )
import HaskellFlows.Types (ProjectDir, mkProjectDir)
import qualified HaskellFlows.Tool.AddImport       as AddImportTool
import qualified HaskellFlows.Tool.AddModules      as AddModulesTool
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
import qualified HaskellFlows.Tool.Regression      as RegressionTool
import qualified HaskellFlows.Tool.RemoveModules   as RemoveModulesTool
import qualified HaskellFlows.Tool.Suggest         as SuggestTool
import qualified HaskellFlows.Tool.SwitchProject   as SwitchProjectTool
import qualified HaskellFlows.Tool.ToolchainStatus as ToolchainStatusTool
import qualified HaskellFlows.Tool.Type            as TypeTool
import qualified HaskellFlows.Tool.ValidateCabal   as ValidateCabalTool
import qualified HaskellFlows.Tool.Workflow        as WorkflowTool

-- | All mutable server state.
--
-- 'srvProjectDir' is an 'IORef' because Phase-1 doesn't yet support
-- runtime project switching through a tool — we'll upgrade it to a TVar
-- the moment we port 'ghci_switch_project'.
--
-- 'srvGhcSession' is held behind an 'MVar' so concurrent handlers
-- cannot race on startup: the first caller wins, everyone else waits
-- on the mutex.
--
-- 'srvBootPosix' + 'srvBinaryPath' wire BUG-07's staleness check:
-- @ghci_workflow(status)@ now compares the on-disk binary mtime
-- with the boot time and flags the user if they rebuilt the MCP
-- but forgot to relaunch the host.
data Server = Server
  { srvProjectDir    :: !(IORef ProjectDir)
  , srvGhcSession    :: !(MVar (Maybe GhcSession))
    -- ^ In-process GHC-API session — the single source of compile
    -- and execute state after Wave 5 landed. All 25 tools route
    -- through this; the legacy subprocess ghci module is gone.
  , srvStore         :: !Store
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
-- 'getExecutablePath' is what @ghci_workflow(status)@ will later
-- stat to detect a rebuild the host hasn't relaunched against.
defaultServer :: IO Server
defaultServer = do
  envVal <- lookupEnv "HASKELL_PROJECT_DIR"
  cwd    <- getCurrentDirectory
  let raw = fromMaybe cwd envVal
  case mkProjectDir raw of
    Left err -> error ("Could not build ProjectDir: " <> show err)
    Right pd -> do
      pdRef    <- newIORef pd
      ghcSess  <- newMVar Nothing
      store    <- openStore pd
      ws       <- newWorkflowStateRef
      bootPos  <- realToFrac <$> getPOSIXTime
      binPath  <- getExecutablePath
      pure Server
        { srvProjectDir    = pdRef
        , srvGhcSession    = ghcSess
        , srvStore         = store
        , srvWorkflowState = ws
        , srvBootPosix     = bootPos
        , srvBinaryPath    = binPath
        }

-- | Dispatch a single parsed request. 'Nothing' means the input was a
-- notification (e.g. @initialized@) and the caller should not write a
-- reply.
handleRequest :: Server -> Request -> IO (Maybe Response)
handleRequest srv req = case (reqMethod req, reqId req) of
  -- notifications — no id, no reply
  ("initialized",          Nothing)  -> pure Nothing
  ("notifications/cancelled", Nothing) -> pure Nothing
  -- requests — always have an id
  (_, Nothing) -> pure Nothing  -- notification to an unknown method
  (method, Just rid) -> Just <$> dispatch srv method (reqParams req) rid

dispatch :: Server -> Text -> Maybe Value -> RequestId -> IO Response
dispatch _ "initialize" _ rid =
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
dispatch _ "tools/list" _ rid =
  pure $ ok rid $ object [ "tools" .= allToolDescriptors ]
dispatch _ "resources/list" _ rid =
  pure $ ok rid $ object [ "resources" .= allResources ]
dispatch _ "resources/read" (Just params) rid =
  case parseEither parseJSON params :: Either String Value of
    Left err -> pure (err_ rid (invalidParamsErr (T.pack err)))
    Right v  -> case v of
      Object o -> case KeyMap.lookup "uri" o of
        Just (String u) -> case renderResource u of
          Just contents -> pure $ ok rid $ object
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
    renderResource :: Text -> Maybe Text
    renderResource uri = case uri of
      "haskell-flows://rules/workflow" ->
        Just (workflowRulesMarkdown allToolDescriptors)
      _ -> Nothing
dispatch _ "resources/read" Nothing rid =
  pure (err_ rid (invalidParamsErr "resources/read requires params"))
dispatch srv "tools/call" (Just params) rid =
  case parseEither parseJSON params of
    Left err -> pure (err_ rid (invalidParamsErr (T.pack err)))
    Right call -> handleToolCall srv call rid
dispatch _ "tools/call" Nothing rid =
  pure (err_ rid (invalidParamsErr "tools/call requires params"))
dispatch _ m _ rid =
  pure (err_ rid (methodNotFoundErr m))

handleToolCall :: Server -> ToolCall -> RequestId -> IO Response
handleToolCall srv call rid = case tcName call of
  "ghci_batch" ->
    -- Special case: batch has to be routed here (not inside
    -- dispatchTool) because it needs the dispatcher as a callback
    -- and dispatchTool would recurse with no termination on
    -- ghci_batch-in-ghci_batch. The batch tool itself refuses
    -- nesting but we also keep the top-level routing explicit.
    --
    -- Batch owns the slowest envelope: it's a bag of N tool calls,
    -- each of which already has its own per-call budget via
    -- 'dispatchTool' -> 'runTool'. A global 6-minute bound here is
    -- the defence of last resort against a pathological batch, not
    -- the per-action cap.
    runTool srv (tcName call) rid
      (BatchTool.handle (dispatchTool srv) (tcArguments call))
  _ ->
    runTool srv (tcName call) rid (dispatchTool srv call)

-- | Pure (non-response-wrapping) tool dispatcher. Exposed so
-- 'HaskellFlows.Tool.Batch' can recurse without pulling Server's
-- Response envelope. Unknown tool names return a structured error
-- 'ToolResult' rather than raising — that way a ghci_batch run with
-- one bad action still completes the remaining good ones.
dispatchTool :: Server -> ToolCall -> IO ToolResult
dispatchTool srv call = case tcName call of
  "ghci_load" -> do
    -- Wave-2 full GhcSession: cabal-aware stanza compile via
    -- loadForTarget, diagnostics captured from the logger hook,
    -- rendered to GHCi-style text so agents still see 'raw' output.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    Load.handle ghcSess pd (tcArguments call)
  "ghci_type" -> do
    -- Phase-2 migrated: reads from the in-process GHC API session,
    -- not the legacy subprocess ghci. Auto-load on first call keeps
    -- the FlowExploratory 'type(localBinding)' scenario green.
    ghcSess <- getOrStartGhcSession srv
    TypeTool.handle ghcSess (tcArguments call)
  "ghci_info" -> do
    -- Phase-2 migrated: getInfo + TyThing classification.
    ghcSess <- getOrStartGhcSession srv
    InfoTool.handle ghcSess (tcArguments call)
  "ghci_eval" -> do
    -- Wave-5 full in-process. Fast path: show-wrap + compileExpr.
    -- Fallback: evalIOString (for IO-typed expressions).
    ghcSess <- getOrStartGhcSession srv
    EvalTool.handle ghcSess (tcArguments call)
  "ghci_quickcheck" -> do
    -- Wave-3 full in-process: compileExpr + unsafeCoerce of a
    -- Test.QuickCheck.quickCheckWithResult invocation.
    ghcSess <- getOrStartGhcSession srv
    QcTool.handle (srvStore srv) ghcSess (tcArguments call)
  "ghci_hole" -> do
    -- Wave-2 full GhcSession: Deferred compile via stanza flags,
    -- diagnostics captured through the logger hook, rendered to
    -- GHCi-style text for parseTypedHoles.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    HoleTool.handle ghcSess pd (tcArguments call)
  "ghci_arbitrary" -> do
    -- Wave-4 full GhcSession: parseName + getInfo + showPprUnsafe.
    ghcSess <- getOrStartGhcSession srv
    ArbitraryTool.handle ghcSess (tcArguments call)
  "hoogle_search" ->
    HoogleTool.handle (tcArguments call)
  "ghci_workflow" -> do
    ws        <- readState (srvWorkflowState srv)
    staleness <- checkStaleness (srvBinaryPath srv) (srvBootPosix srv)
    WorkflowTool.handle
      (srvProjectDir srv)
      (srvGhcSession srv)
      allToolNames
      ws
      staleness
      (tcArguments call)
  "ghci_regression" -> do
    -- Wave-3 full in-process replay via evalIOString.
    ghcSess <- getOrStartGhcSession srv
    RegressionTool.handle (srvStore srv) ghcSess (tcArguments call)
  "ghci_check_module" -> do
    -- Wave-5 full GhcSession: compile/warnings/holes + in-process
    -- property replay via Regression.runOne.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    CheckModuleTool.handle ghcSess (srvStore srv) pd (tcArguments call)
  "ghci_coverage" -> do
    pd <- readIORef (srvProjectDir srv)
    CoverageTool.handle pd (tcArguments call)
  "ghci_complete" -> do
    -- Phase-2 migrated: in-process getNamesInScope + prefix filter.
    ghcSess <- getOrStartGhcSession srv
    CompleteTool.handle ghcSess (tcArguments call)
  "ghci_format" -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- FormatTool.handle pd (tcArguments call)
    invalidateGhcSessionIfPresent srv
    pure r
  "ghci_deps" -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- DepsTool.handle pd (tcArguments call)
    -- Stanza flags hold the resolved package set; ghci_deps just
    -- changed it, so re-bootstrap on next session use.
    invalidateStanzaFlagsIfPresent srv
    pure r
  "ghci_create_project" -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- CreateProjectTool.handle pd (tcArguments call)
    -- New project = completely different stanza set.
    invalidateStanzaFlagsIfPresent srv
    pure r
  "ghci_doc" -> do
    -- Phase-2 migrated: GHC.getDocs on the resolved Name.
    ghcSess <- getOrStartGhcSession srv
    DocTool.handle ghcSess (tcArguments call)
  "ghci_goto" -> do
    -- Phase-2 migrated: in-process Name -> nameSrcSpan lookup.
    ghcSess <- getOrStartGhcSession srv
    GotoTool.handle ghcSess (tcArguments call)
  "ghci_refactor" -> do
    -- Wave-5 full GhcSession: compile-verify via loadForTarget.
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    RefactorTool.handle ghcSess pd (tcArguments call)
  "ghci_lint" -> do
    pd <- readIORef (srvProjectDir srv)
    LintTool.handle pd (tcArguments call)
  "ghci_toolchain_status" ->
    ToolchainStatusTool.handle (tcArguments call)
  "ghci_validate_cabal" -> do
    pd <- readIORef (srvProjectDir srv)
    ValidateCabalTool.handle pd (tcArguments call)
  "ghci_check_project" -> do
    -- Wave-5 full GhcSession (delegates to check_module per file).
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    CheckProjectTool.handle ghcSess (srvStore srv) pd (tcArguments call)
  "ghci_suggest" -> do
    -- Wave-5 full GhcSession: exprType + module-graph walk for siblings.
    ghcSess <- getOrStartGhcSession srv
    SuggestTool.handle ghcSess (tcArguments call)
  "ghci_gate" -> do
    ghcSess <- getOrStartGhcSession srv
    pd      <- readIORef (srvProjectDir srv)
    GateTool.handle (srvStore srv) ghcSess pd (tcArguments call)
  "ghci_quickcheck_export" -> do
    pd <- readIORef (srvProjectDir srv)
    QcExportTool.handle (srvStore srv) pd (tcArguments call)
  "ghci_add_import" -> do
    r <- AddImportTool.handle (tcArguments call)
    invalidateGhcSessionIfPresent srv
    pure r
  "ghci_add_modules" -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- AddModulesTool.handle pd (tcArguments call)
    -- Changes exposed-modules in .cabal, so stanza flags need
    -- re-bootstrap to capture the new unit-id / include path set.
    invalidateStanzaFlagsIfPresent srv
    pure r
  "ghci_remove_modules" -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- RemoveModulesTool.handle pd (tcArguments call)
    invalidateStanzaFlagsIfPresent srv
    pure r
  "ghci_apply_exports" -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- ApplyExportsTool.handle pd (tcArguments call)
    invalidateGhcSessionIfPresent srv
    pure r
  "ghci_fix_warning" -> do
    pd <- readIORef (srvProjectDir srv)
    r  <- FixWarningTool.handle pd (tcArguments call)
    invalidateGhcSessionIfPresent srv
    pure r
  "ghci_imports" -> do
    -- Phase-6 migrated: reads from GhcSession's interactive context.
    ghcSess <- getOrStartGhcSession srv
    ImportsTool.handle ghcSess (tcArguments call)
  "ghci_browse" -> do
    -- Phase-2 migrated: in-process getModuleInfo + modInfoExports.
    ghcSess <- getOrStartGhcSession srv
    BrowseTool.handle ghcSess (tcArguments call)
  "ghci_determinism" -> do
    -- Wave-3 full in-process via evalIOString.
    ghcSess <- getOrStartGhcSession srv
    DeterminismTool.handle ghcSess (tcArguments call)
  "ghci_property_lifecycle" ->
    PropertyLifecycleTool.handle (srvStore srv) (tcArguments call)
  "ghci_toolchain_warmup" ->
    ToolchainWarmupTool.handle (tcArguments call)
  "ghci_bootstrap" -> do
    pd <- readIORef (srvProjectDir srv)
    BootstrapTool.handle pd allToolDescriptors (tcArguments call)
  "ghci_switch_project" ->
    -- SwitchProject is the one tool that mutates BOTH the
    -- project-dir ref AND the session MVar — it takes those
    -- handles directly instead of going through
    -- getOrStartGhcSession, which would boot a fresh session
    -- against the OLD path right before we tear it down.
    SwitchProjectTool.handle
      (srvProjectDir srv)
      (srvGhcSession srv)
      (tcArguments call)
  other ->
    pure ToolResult
      { trContent = [ TextContent ("Unknown tool: " <> other) ]
      , trIsError = True
      }

--------------------------------------------------------------------------------
-- tool registry — single source of truth for both tools/list and
-- ghci_workflow's status view. Keep additions in sync with the
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
  , BootstrapTool.descriptor
  , PropertyLifecycleTool.descriptor
  , ToolchainWarmupTool.descriptor
  ]

allToolNames :: [Text]
allToolNames = map tdName allToolDescriptors

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
runTool :: Server -> Text -> RequestId -> IO ToolResult -> IO Response
runTool srv toolName rid action = do
  out <- try (timeout toolTimeoutMicros action)
           :: IO (Either SomeException (Maybe ToolResult))
  case out of
    Left ex -> do
      -- Any exception that escapes the handler: reset the GhcSession
      -- so the next call starts with a fresh HscEnv, then surface as
      -- a structured error.
      evictGhcSession srv
      pure (ok rid (toJSON (toolException "tool_exception" (T.pack (show ex)))))
    Right Nothing -> do
      evictGhcSession srv
      pure (ok rid (toJSON (toolException "timeout" (timeoutMsg toolName))))
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
enrichWithNextStep :: Text -> ToolResult -> ToolResult
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
timeoutMsg :: Text -> Text
timeoutMsg tool =
  "Tool '" <> tool <> "' exceeded the server's 10-minute hard \
  \ceiling. The GHCi session has been evicted; the next call will \
  \spawn a fresh one. This is a defence-in-depth trip, not the \
  \normal timeout surface — most tools have tighter internal \
  \budgets. If this fires, there is probably a deadlock below this \
  \layer."

-- | Reset the in-process GhcSession. Idempotent; catches any
-- failure so an evict from a watchdog path cannot raise.
evictGhcSession :: Server -> IO ()
evictGhcSession srv = modifyMVar_ (srvGhcSession srv) $ \case
  Nothing -> pure Nothing
  Just s  -> do
    _ <- try (killGhcSession s) :: IO (Either SomeException ())
    pure Nothing

-- | Phase-1 analogue of 'getOrStartSession' for the in-process GHC
-- API session. Unused by any tool yet — Phase 2 starts calling this
-- when the first read-only tools (type, info) migrate.
getOrStartGhcSession :: Server -> IO GhcSession
getOrStartGhcSession srv = modifyMVar (srvGhcSession srv) $ \case
  Just s  -> pure (Just s, s)
  Nothing -> do
    pd <- readIORef (srvProjectDir srv)
    s  <- startGhcSession pd
    pure (Just s, s)

-- | Drop the GhcSession auto-load cache iff a session has already
-- been booted. Used by file-mutation tools (add_import, add_modules,
-- remove_modules, apply_exports, create_project, deps, fix_warning,
-- format) so the next Phase-2 read re-scans disk and sees their
-- edits. Intentionally DOES NOT boot a session if one doesn't exist —
-- that would burn an HscEnv for a cache-invalidation side-effect.
invalidateGhcSessionIfPresent :: Server -> IO ()
invalidateGhcSessionIfPresent srv = do
  m <- readMVar (srvGhcSession srv)
  for_ m invalidateLoadCache

-- | Heavier-hammer cousin for tools that change the .cabal dep
-- graph or stanza layout. Forces a re-bootstrap of stanza flags
-- and a fresh HscEnv on the next session use.
invalidateStanzaFlagsIfPresent :: Server -> IO ()
invalidateStanzaFlagsIfPresent srv = do
  m <- readMVar (srvGhcSession srv)
  for_ m invalidateStanzaFlags

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
toolException :: Text -> Text -> ToolResult
toolException kind msg =
  let payload = object
        [ "success"    .= False
        , "error"      .= ("Tool threw an exception: " <> msg)
        , "error_kind" .= kind
        ]
      encoded = TE.decodeUtf8 (BL.toStrict (encode payload))
  in ToolResult
       { trContent = [ TextContent encoded ]
       , trIsError = True
       }
