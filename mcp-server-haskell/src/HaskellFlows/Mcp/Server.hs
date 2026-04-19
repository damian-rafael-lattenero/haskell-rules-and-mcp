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

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, mkProjectDir)
import qualified HaskellFlows.Tool.Arbitrary  as ArbitraryTool
import qualified HaskellFlows.Tool.Eval       as EvalTool
import qualified HaskellFlows.Tool.Hole       as HoleTool
import qualified HaskellFlows.Tool.Hoogle     as HoogleTool
import qualified HaskellFlows.Tool.Info       as InfoTool
import qualified HaskellFlows.Tool.Load       as Load
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.Type       as TypeTool
import qualified HaskellFlows.Tool.Workflow   as WorkflowTool

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
    Right pd -> Server <$> newIORef pd <*> newMVar Nothing

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
  pure $ ok rid $ object
    [ "tools" .=
        [ Load.descriptor
        , TypeTool.descriptor
        , InfoTool.descriptor
        , EvalTool.descriptor
        , QcTool.descriptor
        , HoleTool.descriptor
        , ArbitraryTool.descriptor
        , HoogleTool.descriptor
        , WorkflowTool.descriptor
        ]
    ]
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
  "ghci_load" -> do
    sess <- getOrStartSession srv
    pd   <- readIORef (srvProjectDir srv)
    runTool srv rid (Load.handle sess pd (tcArguments call))
  "ghci_type" -> do
    sess <- getOrStartSession srv
    runTool srv rid (TypeTool.handle sess (tcArguments call))
  "ghci_info" -> do
    sess <- getOrStartSession srv
    runTool srv rid (InfoTool.handle sess (tcArguments call))
  "ghci_eval" -> do
    sess <- getOrStartSession srv
    runTool srv rid (EvalTool.handle sess (tcArguments call))
  "ghci_quickcheck" -> do
    sess <- getOrStartSession srv
    runTool srv rid (QcTool.handle sess (tcArguments call))
  "ghci_hole" -> do
    sess <- getOrStartSession srv
    pd   <- readIORef (srvProjectDir srv)
    runTool srv rid (HoleTool.handle sess pd (tcArguments call))
  "ghci_arbitrary" -> do
    sess <- getOrStartSession srv
    runTool srv rid (ArbitraryTool.handle sess (tcArguments call))
  "hoogle_search" ->
    -- No GHCi session needed: the tool shells out to the `hoogle`
    -- binary directly. Still runs under the exception shield so the
    -- server stays alive if hoogle is missing or crashes.
    runTool srv rid (HoogleTool.handle (tcArguments call))
  "ghci_workflow" ->
    -- Read-only: uses the server state refs directly, no session boot.
    runTool srv rid (WorkflowTool.handle (srvProjectDir srv)
                                          (srvSession srv)
                                          (tcArguments call))
  other -> pure (err_ rid (methodNotFoundErr ("tool " <> other)))

-- | Common exception shield for every tool handler.
--
-- Prevents a handler crash from taking down the server loop and surfaces
-- it as a structured tool-level error to the client. On 'SessionExhausted'
-- (buffer cap from the DoS guard in Session.hs), we additionally evict
-- the dead session from the MVar so 'getOrStartSession' rebuilds it on
-- the next call — otherwise every subsequent tool call would inherit the
-- Overflowed status and fail identically.
runTool :: Server -> RequestId -> IO ToolResult -> IO Response
runTool srv rid action = do
  out <- try action :: IO (Either SomeException ToolResult)
  case out of
    Left ex -> do
      case fromException ex :: Maybe SessionExhausted of
        Just _  -> evictSession srv
        Nothing -> pure ()
      pure (ok rid (toJSON (toolException (T.pack (show ex)))))
    Right tr -> pure (ok rid (toJSON tr))

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
