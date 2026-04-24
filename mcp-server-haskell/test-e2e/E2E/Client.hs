-- | In-process MCP client. Preserves the "scripted tool calls
-- with JSON payloads" experience of a subprocess client, but
-- cuts out the transport layer — we hit 'dispatchTool' directly
-- against a live 'Server' pointed at the scenario's temp
-- project dir.
--
-- Why this design:
--
--   * Subprocess-over-pipes-on-macOS-with-GHC-9.12 surfaced
--     non-deterministic flush/read hangs that were burning the
--     whole batch. The scenario cares about tool behaviour, not
--     pipe semantics.
--   * This is what @lsp-test@ does for the majority of HLS's
--     integration tests, and what @cabal-install@ + @stack@
--     use for their command tests — call the dispatch directly,
--     run a single subprocess smoke test on the side to cover
--     the transport itself.
--   * Still "black-box" in the sense the scenario uses only the
--     public tool registry via JSON payloads. Nothing reaches
--     into tool internals; the exact same 'ToolResult' shape
--     the JSON-RPC transport would return is what we see.
--
-- The per-call 'nextStep' injection + 'WorkflowState' tracking
-- that happens inside @Server.runTool@ is replicated here so
-- the scenario's assertions on @nextStep.tool@ / @nextStep.chain@
-- are meaningful.
module E2E.Client
  ( McpClient
  , newClient
  , callTool
  , close
  , findMcpBinaryPath
  ) where

import Control.Exception (SomeException, throwIO, try)
import Data.Foldable (traverse_)
import qualified Data.Aeson as A
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hFlush, hPutStrLn, stdout)
import System.Process (readProcessWithExitCode)
import qualified Data.Vector as V

import HaskellFlows.Mcp.Protocol
  ( Request (..)
  , Response (..)
  , ToolContent (..)
  )
import HaskellFlows.Mcp.Server (Server, defaultServer, handleRequest)

-- | Minimal client handle. Holds a live 'Server' plus the
-- previous value of @HASKELL_PROJECT_DIR@ so 'close' can
-- restore it.
data McpClient = McpClient
  { mcServer    :: !Server
  , mcPrevDir   :: !(Maybe String)
  }

-- | Construct a client. The second argument mirrors the
-- subprocess API shape ('extraEnv') — only @HASKELL_PROJECT_DIR@
-- is honoured because the server reads it at 'defaultServer'
-- time. We set it before 'defaultServer' runs and restore it
-- on 'close'.
newClient :: FilePath -> [(String, String)] -> IO McpClient
newClient _unusedBinary extraEnv = do
  let newProjectDir = lookup "HASKELL_PROJECT_DIR" extraEnv
  prev <- lookupEnv "HASKELL_PROJECT_DIR"
  traverse_ (setEnv "HASKELL_PROJECT_DIR") newProjectDir
  srv <- defaultServer
  pure McpClient { mcServer = srv, mcPrevDir = prev }

-- | Close the client. Restores the prior @HASKELL_PROJECT_DIR@
-- so subsequent clients (or the host process) don't inherit
-- the scenario's tempdir path.
close :: McpClient -> IO ()
close c = case mcPrevDir c of
  Just d  -> setEnv   "HASKELL_PROJECT_DIR" d
  Nothing -> unsetEnv "HASKELL_PROJECT_DIR"

-- | @tools/call@ equivalent. Logs the call + duration so the
-- scenario's progress stream shows each tool as it fires —
-- if a call blocks (unlikely now but possible for e.g.
-- 'ghc_gate' spawning cabal), you still see which one.
--
-- Routes through 'handleRequest' (NOT 'dispatchTool' directly):
-- that's the FULL dispatcher the stdio transport uses, so every
-- tool that has special-case handling at the 'handleToolCall'
-- layer — notably 'ghc_batch' which recurses into the
-- dispatcher via a callback — works end-to-end. Our earlier
-- 'dispatchTool'-only path silently mapped 'ghc_batch' to the
-- "Unknown tool" fallback (it's not in the case-list there).
callTool :: McpClient -> Text -> Value -> IO Value
callTool c name args = do
  putStrLn ("    [mcp] → " <> show name <> " …")
  hFlush stdout
  t0 <- getPOSIXTime
  let req = Request
        { reqJsonrpc = "2.0"
        , reqMethod  = "tools/call"
        , reqParams  = Just (object
            [ "name"      .= name
            , "arguments" .= args
            ])
        , reqId      = Just (Number 0)
        }
  eRes <- try (handleRequest (mcServer c) req)
            :: IO (Either SomeException (Maybe Response))
  t1 <- getPOSIXTime
  let ms = round ((realToFrac (t1 - t0) :: Double) * 1000) :: Int
  case eRes of
    Left ex -> do
      putStrLn ("    [mcp] ✘ " <> show name <> "  EXCEPTION: " <> show ex)
      hFlush stdout
      pure (object
        [ "success" .= False
        , "error"   .= ("exception: " <> show ex)
        ])
    Right Nothing -> do
      putStrLn ("    [mcp] ✘ " <> show name <> "  no response (notification path)")
      hFlush stdout
      pure Null
    Right (Just resp) -> do
      putStrLn ("    [mcp] ← " <> show name <> "  (" <> show ms <> " ms)")
      hFlush stdout
      case respPayload resp of
        Left rpcErr ->
          pure (object
            [ "success" .= False
            , "error"   .= ("rpc error: " <> show rpcErr)
            ])
        Right resultValue ->
          -- The tool's content lives at result.content[0].text.
          -- Decode that nested JSON so scenarios see the tool's
          -- own payload shape, same as the transport layer.
          pure (unwrapResult resultValue)

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Unwrap a @tools/call@ result value into the tool's own
-- JSON payload. The MCP envelope is
-- @{ "content": [ { "type": "text", "text": "<json>" } ], ... }@;
-- we peel both layers so scenarios see the tool-specific shape
-- (with its @nextStep@ already injected by Server.runTool).
unwrapResult :: Value -> Value
unwrapResult v = case v of
  Object o -> case KeyMap.lookup "content" o of
    Just (Array arr) | not (V.null arr) ->
      case V.head arr of
        Object inner -> case KeyMap.lookup "text" inner of
          Just (String t) ->
            fromMaybe v (A.decode (BL.fromStrict (TE.encodeUtf8 t)))
          _ -> v
        _ -> v
    _ -> v
  _ -> v

-- Silence unused warnings when building.
_unused :: KeyMap.KeyMap Value -> V.Vector Value
_unused o = V.fromList (KeyMap.elems o)

--------------------------------------------------------------------------------
-- binary discovery (used by the subprocess smoke test, not by newClient)
--------------------------------------------------------------------------------

-- | Find the @haskell-flows-mcp@ binary for the subprocess smoke
-- test. Preference order:
--   1. @HASKELL_FLOWS_MCP_BIN@ env var (explicit override).
--   2. @cabal list-bin exe:haskell-flows-mcp@.
findMcpBinaryPath :: IO FilePath
findMcpBinaryPath = do
  mEnv <- lookupEnv "HASKELL_FLOWS_MCP_BIN"
  case mEnv of
    Just p  -> pure p
    Nothing -> do
      (_ec, out, _err) <-
        readProcessWithExitCode "cabal"
          ["list-bin", "exe:haskell-flows-mcp"] ""
      case lines out of
        (l : _) | not (null (trim l)) -> pure (trim l)
        _ -> throwIO (userError (
          "Could not locate haskell-flows-mcp binary. Set \
          \HASKELL_FLOWS_MCP_BIN or run under a cabal build with \
          \`build-tool-depends`. Raw `cabal list-bin` output: "
          <> out))
  where
    trim = T.unpack . T.strip . T.pack
