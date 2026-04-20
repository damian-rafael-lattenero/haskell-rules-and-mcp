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
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (SomeException, fromException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

import HaskellFlows.Data.PropertyStore (Store, openStore)
import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, mkProjectDir)
import qualified HaskellFlows.Tool.Arbitrary       as ArbitraryTool
import qualified HaskellFlows.Tool.Batch           as BatchTool
import qualified HaskellFlows.Tool.CheckModule     as CheckModuleTool
import qualified HaskellFlows.Tool.CheckProject    as CheckProjectTool
import qualified HaskellFlows.Tool.Complete        as CompleteTool
import qualified HaskellFlows.Tool.Coverage        as CoverageTool
import qualified HaskellFlows.Tool.CreateProject   as CreateProjectTool
import qualified HaskellFlows.Tool.Deps            as DepsTool
import qualified HaskellFlows.Tool.Doc             as DocTool
import qualified HaskellFlows.Tool.Eval            as EvalTool
import qualified HaskellFlows.Tool.Format          as FormatTool
import qualified HaskellFlows.Tool.Goto            as GotoTool
import qualified HaskellFlows.Tool.Hole            as HoleTool
import qualified HaskellFlows.Tool.Hoogle          as HoogleTool
import qualified HaskellFlows.Tool.Info            as InfoTool
import qualified HaskellFlows.Tool.Lint            as LintTool
import qualified HaskellFlows.Tool.Load            as Load
import qualified HaskellFlows.Tool.QuickCheck      as QcTool
import qualified HaskellFlows.Tool.Refactor        as RefactorTool
import qualified HaskellFlows.Tool.Regression      as RegressionTool
import qualified HaskellFlows.Tool.Suggest         as SuggestTool
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
-- 'srvSession' is held behind an 'MVar' so concurrent handlers cannot race
-- on startup: the first caller wins, everyone else waits on the mutex.
data Server = Server
  { srvProjectDir :: !(IORef ProjectDir)
  , srvSession    :: !(MVar (Maybe Session))
  , srvStore      :: !Store
  }

-- | Build a server whose project directory is sourced from
-- @HASKELL_PROJECT_DIR@ or the current working directory (mirrors TS
-- @src/index.ts@). Rejects a relative value up front — no lazy errors.
defaultServer :: IO Server
defaultServer = do
  envVal <- lookupEnv "HASKELL_PROJECT_DIR"
  cwd    <- getCurrentDirectory
  let raw = fromMaybe cwd envVal
  case mkProjectDir raw of
    Left err -> error ("Could not build ProjectDir: " <> show err)
    Right pd -> do
      pdRef <- newIORef pd
      sess  <- newMVar Nothing
      store <- openStore pd
      pure Server { srvProjectDir = pdRef, srvSession = sess, srvStore = store }

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
      }
dispatch _ "tools/list" _ rid =
  pure $ ok rid $ object [ "tools" .= allToolDescriptors ]
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
    sess <- getOrStartSession srv
    pd   <- readIORef (srvProjectDir srv)
    Load.handle sess pd (tcArguments call)
  "ghci_type" -> do
    sess <- getOrStartSession srv
    TypeTool.handle sess (tcArguments call)
  "ghci_info" -> do
    sess <- getOrStartSession srv
    InfoTool.handle sess (tcArguments call)
  "ghci_eval" -> do
    sess <- getOrStartSession srv
    EvalTool.handle sess (tcArguments call)
  "ghci_quickcheck" -> do
    sess <- getOrStartSession srv
    QcTool.handle (srvStore srv) sess (tcArguments call)
  "ghci_hole" -> do
    sess <- getOrStartSession srv
    pd   <- readIORef (srvProjectDir srv)
    HoleTool.handle sess pd (tcArguments call)
  "ghci_arbitrary" -> do
    sess <- getOrStartSession srv
    ArbitraryTool.handle sess (tcArguments call)
  "hoogle_search" ->
    HoogleTool.handle (tcArguments call)
  "ghci_workflow" ->
    WorkflowTool.handle
      (srvProjectDir srv)
      (srvSession srv)
      allToolNames
      (tcArguments call)
  "ghci_regression" -> do
    sess <- getOrStartSession srv
    RegressionTool.handle (srvStore srv) sess (tcArguments call)
  "ghci_check_module" -> do
    sess <- getOrStartSession srv
    pd   <- readIORef (srvProjectDir srv)
    CheckModuleTool.handle sess (srvStore srv) pd (tcArguments call)
  "ghci_coverage" -> do
    pd <- readIORef (srvProjectDir srv)
    CoverageTool.handle pd (tcArguments call)
  "ghci_complete" -> do
    sess <- getOrStartSession srv
    CompleteTool.handle sess (tcArguments call)
  "ghci_format" -> do
    pd <- readIORef (srvProjectDir srv)
    FormatTool.handle pd (tcArguments call)
  "ghci_deps" -> do
    pd <- readIORef (srvProjectDir srv)
    DepsTool.handle pd (tcArguments call)
  "ghci_create_project" -> do
    pd <- readIORef (srvProjectDir srv)
    CreateProjectTool.handle pd (tcArguments call)
  "ghci_doc" -> do
    sess <- getOrStartSession srv
    DocTool.handle sess (tcArguments call)
  "ghci_goto" -> do
    sess <- getOrStartSession srv
    GotoTool.handle sess (tcArguments call)
  "ghci_refactor" -> do
    sess <- getOrStartSession srv
    pd   <- readIORef (srvProjectDir srv)
    RefactorTool.handle sess pd (tcArguments call)
  "ghci_lint" -> do
    pd <- readIORef (srvProjectDir srv)
    LintTool.handle pd (tcArguments call)
  "ghci_toolchain_status" ->
    ToolchainStatusTool.handle (tcArguments call)
  "ghci_validate_cabal" -> do
    pd <- readIORef (srvProjectDir srv)
    ValidateCabalTool.handle pd (tcArguments call)
  "ghci_check_project" -> do
    sess <- getOrStartSession srv
    pd   <- readIORef (srvProjectDir srv)
    CheckProjectTool.handle sess (srvStore srv) pd (tcArguments call)
  "ghci_suggest" -> do
    sess <- getOrStartSession srv
    SuggestTool.handle sess (tcArguments call)
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
  ]

allToolNames :: [Text]
allToolNames = map tdName allToolDescriptors

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
      case fromException ex :: Maybe SessionExhausted of
        Just _  -> evictSession srv
        Nothing -> pure ()
      pure (ok rid (toJSON (toolException (T.pack (show ex)))))
    Right Nothing -> do
      evictSession srv
      pure (ok rid (toJSON (toolException (timeoutMsg toolName))))
    Right (Just tr) -> pure (ok rid (toJSON tr))

-- | Human-readable timeout error message for agents.
timeoutMsg :: Text -> Text
timeoutMsg tool =
  "Tool '" <> tool <> "' exceeded the server's 10-minute hard \
  \ceiling. The GHCi session has been evicted; the next call will \
  \spawn a fresh one. This is a defence-in-depth trip, not the \
  \normal timeout surface — most tools have tighter internal \
  \budgets. If this fires, there is probably a deadlock below this \
  \layer."

-- | Remove the current session from the MVar, killing it if present.
-- The next 'getOrStartSession' will boot a fresh child process.
evictSession :: Server -> IO ()
evictSession srv = modifyMVar_ (srvSession srv) $ \case
  Nothing -> pure Nothing
  Just s  -> do
    _ <- try (killSession s) :: IO (Either SomeException ())
    pure Nothing

getOrStartSession :: Server -> IO Session
getOrStartSession srv = modifyMVar (srvSession srv) $ \case
  Just s  -> pure (Just s, s)
  Nothing -> do
    pd <- readIORef (srvProjectDir srv)
    s  <- startSession pd
    pure (Just s, s)

--------------------------------------------------------------------------------
-- small response helpers
--------------------------------------------------------------------------------

ok :: RequestId -> Value -> Response
ok rid v = Response { respId = rid, respPayload = Right v }

err_ :: RequestId -> RpcError -> Response
err_ rid e = Response { respId = rid, respPayload = Left e }

toolException :: Text -> ToolResult
toolException msg =
  ToolResult
    { trContent = [ TextContent ("Tool threw an exception: " <> msg) ]
    , trIsError = True
    }
