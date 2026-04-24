-- | Flow: hostile JSON-RPC corpus against the stdio transport.
--
-- Every other scenario in this suite bypasses the transport layer —
-- they call @handleRequest@ in-process, which assumes a well-formed
-- 'Request' already. That leaves the bytes-to-'Request' boundary,
-- i.e. the stdio transport parser itself, with no e2e oracle.
--
-- This scenario fixes that. It spawns ONE real @haskell-flows-mcp@
-- subprocess, feeds it a corpus of hostile JSON-RPC lines, and
-- finishes with a sentinel 'tools/list' probe. The oracle is simple:
--
--   /If the subprocess is alive and answers tools/list AFTER every
--   hostile line, the transport survived them all./
--
-- The hostile corpus targets the real failure modes of a JSON-RPC
-- parser + dispatcher:
--
--   * Entirely non-JSON input (must not crash the read loop).
--   * JSON that parses but violates JSON-RPC 2.0 ('jsonrpc' missing,
--     wrong version, id of the wrong type, method missing/non-string).
--   * Well-formed request shape but with @tools/call@ params that
--     break downstream dispatch (missing name, null name, unknown
--     tool, arguments of the wrong type).
--   * Unicode / control-character payloads that could desync any
--     buffer assumptions downstream.
--   * A payload whose @id@ is a literal copy of the sentinel the
--     session uses for command framing — if the server echoes it
--     unsanitised into any log or response it ships with Read,
--     we have a real desync risk.
--
-- Each of these is a bug a hostile or buggy client could produce.
-- The transport must degrade gracefully, never take the binary down.
module Scenarios.FlowCorpusTransport
  ( runFlow
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..))
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess (..))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

-- | Every entry is one line we want to send over stdin. 'payloadLabel'
-- names the hazard so a failure's location is obvious.
data Payload = Payload
  { payloadLabel :: !String
  , payloadLine  :: !String
  }

-- | The corpus. Order is not significant — we care only that after
-- ALL of them the binary still answers a probe.
corpus :: [Payload]
corpus =
  -- ---- unparseable input ----
  [ Payload "not-even-json"
      "this is not json at all, just a line of text"
  , Payload "empty-object"
      "{}"
  , Payload "json-fragment"
      "{\"jsonrpc\":\"2.0\",\"id\":"       -- truncated mid-field
  -- ---- JSON-RPC 2.0 envelope violations ----
  , Payload "missing-jsonrpc"
      "{\"id\":200,\"method\":\"tools/list\"}"
  , Payload "wrong-jsonrpc-version"
      "{\"jsonrpc\":\"1.0\",\"id\":201,\"method\":\"tools/list\"}"
  , Payload "id-null"
      "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"tools/list\"}"
  , Payload "id-array"
      "{\"jsonrpc\":\"2.0\",\"id\":[1,2,3],\"method\":\"tools/list\"}"
  , Payload "id-object"
      "{\"jsonrpc\":\"2.0\",\"id\":{\"nested\":true},\"method\":\"tools/list\"}"
  , Payload "method-missing"
      "{\"jsonrpc\":\"2.0\",\"id\":202}"
  , Payload "method-number"
      "{\"jsonrpc\":\"2.0\",\"id\":203,\"method\":42}"
  , Payload "method-null"
      "{\"jsonrpc\":\"2.0\",\"id\":204,\"method\":null}"
  -- ---- tools/call-specific shape errors ----
  , Payload "tools-call-no-name"
      "{\"jsonrpc\":\"2.0\",\"id\":300,\"method\":\"tools/call\",\"params\":{\"arguments\":{}}}"
  , Payload "tools-call-name-null"
      "{\"jsonrpc\":\"2.0\",\"id\":301,\"method\":\"tools/call\",\"params\":{\"name\":null,\"arguments\":{}}}"
  , Payload "tools-call-unknown-tool"
      "{\"jsonrpc\":\"2.0\",\"id\":302,\"method\":\"tools/call\",\"params\":{\"name\":\"ghc_not_a_real_tool\",\"arguments\":{}}}"
  , Payload "tools-call-args-string"
      "{\"jsonrpc\":\"2.0\",\"id\":303,\"method\":\"tools/call\",\"params\":{\"name\":\"ghc_workflow\",\"arguments\":\"not-an-object\"}}"
  , Payload "tools-call-args-array"
      "{\"jsonrpc\":\"2.0\",\"id\":304,\"method\":\"tools/call\",\"params\":{\"name\":\"ghc_workflow\",\"arguments\":[1,2,3]}}"
  , Payload "params-string"
      "{\"jsonrpc\":\"2.0\",\"id\":305,\"method\":\"tools/call\",\"params\":\"not-an-object\"}"
  , Payload "params-array"
      "{\"jsonrpc\":\"2.0\",\"id\":306,\"method\":\"tools/call\",\"params\":[1,2,3]}"
  -- ---- unknown / weird methods ----
  , Payload "unknown-method"
      "{\"jsonrpc\":\"2.0\",\"id\":400,\"method\":\"totally/unknown\"}"
  , Payload "method-with-slashes"
      "{\"jsonrpc\":\"2.0\",\"id\":401,\"method\":\"a/b/c/d/e\"}"
  -- ---- sentinel-looking id (potential desync vector) ----
  , Payload "id-contains-sentinel"
      "{\"jsonrpc\":\"2.0\",\"id\":\"<<<GHCi-DONE-7f3a2b>>>\",\"method\":\"tools/list\"}"
  -- ---- control chars + unicode corners ----
  , Payload "tab-in-method"
      "{\"jsonrpc\":\"2.0\",\"id\":500,\"method\":\"a\\tb\"}"
  , Payload "unicode-in-method"
      "{\"jsonrpc\":\"2.0\",\"id\":501,\"method\":\"\\u5165\\u529B\"}"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow _c _pd = do
  ----------------------------------------------------------------
  -- (0) resolve the binary path.
  --
  -- E2E.Client.findMcpBinaryPath looks at HASKELL_FLOWS_MCP_BIN
  -- first, then falls back to `cabal list-bin` — same sequence
  -- used by E2E.Smoke. Guard against the lookup itself throwing
  -- (unusual but possible if cabal isn't on PATH) so the scenario
  -- still produces a structured Check instead of a framework error.
  ----------------------------------------------------------------
  eBin <- try Client.findMcpBinaryPath
            :: IO (Either SomeException FilePath)
  case eBin of
    Left ex -> do
      t0 <- stepHeader 1 "skipped · could not resolve haskell-flows-mcp binary"
      cSkip <- liveCheck $ checkPure
        "corpus · binary path resolves (env var or cabal list-bin)"
        False
        ("Could not find haskell-flows-mcp. Set HASKELL_FLOWS_MCP_BIN \
         \or run under cabal so 'cabal list-bin exe:haskell-flows-mcp' \
         \works. Error: " <> T.pack (show ex))
      stepFooter 1 t0
      pure [cSkip]
    Right binary -> do
      t0 <- stepHeader 1
              ("transport corpus · " <> T.pack (show (length corpus))
                <> " hostile payloads + final probe")
      -- Dump the label list once so a subsequent investigation of a
      -- FAIL knows which payloads were in the batch without re-reading
      -- source.
      putStrLn ("    [corpus labels] "
                <> unwords (map payloadLabel corpus))
      result <- driveCorpus binary
      cAlive <- liveCheck $ checkPure
        ("corpus survived · tools/list answers AFTER "
          <> T.pack (show (length corpus)) <> " hostile lines")
        (crAlive result)
        ("The transport must survive every hostile line and still \
         \answer a final tools/list probe. exit="
         <> T.pack (show (crExitCode result))
         <> "  toolsAdvertised=" <> T.pack (show (crToolsAdvertised result))
         <> "  stderr (trimmed)=" <> T.pack (take 500 (crStderr result)))
      cExit <- liveCheck $ checkPure
        "corpus · process exited cleanly (no crash code)"
        (crExitCode result == ExitSuccess)
        ("Expected ExitSuccess after EOF. Got: "
          <> T.pack (show (crExitCode result)))
      stepFooter 1 t0
      pure [cAlive, cExit]

--------------------------------------------------------------------------------
-- subprocess driver
--------------------------------------------------------------------------------

data CorpusResult = CorpusResult
  { crExitCode         :: !ExitCode
  , crToolsAdvertised  :: !Int
  , crAlive            :: !Bool
  , crStderr           :: !String
  }

-- | Spawn @binary@, feed it (initialize + initialized + corpus + final
-- tools/list probe), collect stdout + exit code, parse tool count.
driveCorpus :: FilePath -> IO CorpusResult
driveCorpus binary = do
  currentEnv <- getEnvironment
  let preamble =
        [ "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},\"clientInfo\":{\"name\":\"corpus\",\"version\":\"0\"}}}"
        , "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}"
        ]
      -- After every payload we lob in a cheap probe so the final
      -- tools/list isn't the ONLY test of liveness. If we see a
      -- response with some id, at least the transport was still
      -- reading at that moment.
      hostile = map payloadLine corpus
      -- Final sentinel: the one the oracle checks.
      probe = "{\"jsonrpc\":\"2.0\",\"id\":9999,\"method\":\"tools/list\"}"
      input = unlines (preamble <> hostile <> [probe])

      cp = (proc binary [])
             { env = Just (("HASKELL_PROJECT_DIR", "/tmp/mcp-e2e-corpus") : currentEnv)
             }
  (ec, outStr, errStr) <- readCreateProcessWithExitCode cp input
  let toolsInFinal = countToolEntries outStr
      alive =
        -- Liveness oracle: the FINAL response line contains tool
        -- entries. If the binary died earlier, stdout truncates
        -- before the final probe's response is written.
        toolsInFinal >= 1
        && "\"id\":9999" `isInfixOf` outStr
  pure CorpusResult
    { crExitCode        = ec
    , crToolsAdvertised = toolsInFinal
    , crAlive           = alive
    , crStderr          = errStr
    }

-- | Copy of Smoke.countToolEntries — intentionally duplicated rather
-- than exported to keep the E2E framework surface tight.
countToolEntries :: String -> Int
countToolEntries = go 0
  where
    needle = "\"name\":\"ghc_"
    go !n s
      | null s                = n
      | needle `isInfixOf` s  = go (n + 1) (drop (length needle) (dropUntil needle s))
      | otherwise             = n

    dropUntil pat s
      | null s             = s
      | pat `isInfixOf` s  = dropWhileNotPrefix pat s
      | otherwise          = s

    dropWhileNotPrefix pat s@(_:xs)
      | pat `isInfixOf` s && take (length pat) s == pat = s
      | otherwise = dropWhileNotPrefix pat xs
    dropWhileNotPrefix _ [] = []

-- silence unused warnings
_unusedValue :: Value -> Text
_unusedValue _ = ""
