-- | Subprocess smoke test.
--
-- One job: verify the real @haskell-flows-mcp@ binary can
-- answer @initialize@ + @tools/list@ over stdio. This is the
-- only thing that talks to the binary via a pipe; the rest of
-- the E2E suite runs in-process against the library API
-- ('E2E.Client').
--
-- Rationale: pipe/flush behaviour on macOS + GHC 9.12 proved
-- too flaky for a full scenario to drive dozens of JSON-RPC
-- round-trips reliably. We pay the subprocess cost once, get
-- the transport coverage, and then run the rest of the
-- scenario at library speed.
module E2E.Smoke
  ( runSmoke
  , SmokeResult (..)
  ) where

import Data.List (isInfixOf)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess (..))

data SmokeResult = SmokeResult
  { srPassed       :: !Bool
  , srToolsAdvertised :: !Int
  , srLog          :: !String
  }
  deriving stock (Show)

-- | Drive one @initialize@ + @initialized@ + @tools/list@
-- round-trip over the binary's stdio. Returns pass/fail +
-- the advertised tool count (parsed crudely from the tools/list
-- response body).
--
-- Uses 'readCreateProcessWithExitCode' which feeds stdin and
-- reads stdout to completion — same pattern as the shell smoke
-- test (@printf ... | mcp@) that was shown to work reliably
-- in the dogfood.
runSmoke :: FilePath -> IO SmokeResult
runSmoke binary = do
  currentEnv <- getEnvironment
  let input = unlines
        [ "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},\"clientInfo\":{\"name\":\"e2e-smoke\",\"version\":\"0\"}}}"
        , "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}"
        , "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"
        ]
      cp = (proc binary [])
             { env = Just (("HASKELL_PROJECT_DIR", "/tmp/mcp-e2e-smoke") : currentEnv)
             }
  (ec, outStr, errStr) <- readCreateProcessWithExitCode cp input
  let tools = countToolEntries outStr
      ok    = ec == ExitSuccess
           && "\"protocolVersion\":\"2024-11-05\"" `isInfixOf` outStr
           && tools >= 1
  pure SmokeResult
    { srPassed          = ok
    , srToolsAdvertised = tools
    , srLog             = "exit=" <> show ec <> " tools=" <> show tools
                       <> (if null errStr then "" else "  stderr=" <> errStr)
    }

-- | Count how many tool entries the @tools/list@ response
-- advertises. We look for the @"name":"ghc_@ substring — one
-- per registered tool — which is a simpler and more
-- robust-to-whitespace probe than full JSON parsing here.
countToolEntries :: String -> Int
countToolEntries = go 0
  where
    needle = "\"name\":\"ghc_"
    go !n s
      | null s                 = n
      | needle `isInfixOf` s   = go (n + 1) (drop (length needle) (dropUntil needle s))
      | otherwise              = n

    dropUntil pat s
      | null s              = s
      | pat `isInfixOf` s   = dropWhileNotPrefix pat s
      | otherwise           = s

    dropWhileNotPrefix pat s@(_:xs)
      | pat `isInfixOf` s && take (length pat) s == pat = s
      | otherwise = dropWhileNotPrefix pat xs
    dropWhileNotPrefix _ [] = []
