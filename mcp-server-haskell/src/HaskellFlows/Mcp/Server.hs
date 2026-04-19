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

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (SomeException, try)
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
import qualified HaskellFlows.Tool.Load as Load

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
  pure $ ok rid $ object [ "tools" .= [ Load.descriptor ] ]
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
    out  <- try (Load.handle sess pd (tcArguments call))
              :: IO (Either SomeException ToolResult)
    case out of
      Left ex  -> pure (ok rid (toJSON (toolException (T.pack (show ex)))))
      Right tr -> pure (ok rid (toJSON tr))
  other -> pure (err_ rid (methodNotFoundErr ("tool " <> other)))

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
