-- | Subprocess smoke test.
--
-- One job: verify the real @haskell-flows-mcp@ binary correctly
-- handles every JSON-RPC method we advertise, plus the canonical
-- failure modes. This is the only thing that talks to the binary
-- via a pipe; the rest of the E2E suite runs in-process against
-- the library API ('E2E.Client').
--
-- Rationale: pipe/flush behaviour on macOS + GHC 9.12 proved
-- too flaky for a full scenario to drive dozens of JSON-RPC
-- round-trips reliably. We pay the subprocess cost once, get
-- the transport coverage, and then run the rest of the
-- scenario at library speed.
--
-- Coverage matrix (one constructor of 'RpcMethod' per row):
--
--   * 'Initialize'             — handshake, returns protocolVersion
--   * 'Initialized'            — notification, no response expected
--   * 'ToolsList'              — advertises ≥ 1 tool
--   * 'ToolsCall'              — implicitly via the in-process suite,
--                                kept off the smoke path because it
--                                requires a project dir setup
--   * 'ResourcesList'          — advertises the workflow-rules URI
--   * 'ResourcesRead'          — workflow-rules URI returns content
--   * 'NotificationsCancelled' — notification, no response expected
--
-- Plus the negative path:
--
--   * Unknown method — returns JSON-RPC error -32601 (method not
--     found), which is what 'parseRpcMethod' falls through to.
module E2E.Smoke
  ( runSmoke
  , SmokeResult (..)
  ) where

import Data.List (isInfixOf)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess (..))

-- | Aggregate transport coverage. Each boolean pins one wire-format
-- contract. Splitting them out (instead of one global pass/fail)
-- lets the runner pinpoint which method regressed when the smoke
-- goes red.
data SmokeResult = SmokeResult
  { srPassed                :: !Bool       -- ^ legacy aggregate (init+toolslist)
  , srToolsAdvertised       :: !Int        -- ^ count of @"name":"ghc_*"@ in tools/list
  , srInitializeOk          :: !Bool       -- ^ initialize handshake answered
  , srInitializedNoResponse :: !Bool       -- ^ initialized notification produced no response
  , srToolsListOk           :: !Bool       -- ^ tools/list advertises ≥ 1 tool
  , srResourcesListOk       :: !Bool       -- ^ resources/list advertises workflow-rules URI
  , srResourcesReadOk       :: !Bool       -- ^ resources/read on workflow-rules returns markdown
  , srCancelNoResponse      :: !Bool       -- ^ notifications/cancelled produced no response
  , srUnknownMethodErr      :: !Bool       -- ^ unknown method returns -32601
  , srLog                   :: !String     -- ^ debug log (exit code, raw stdout fragment)
  }
  deriving stock (Show)

-- | Drive a full handshake + every advertised JSON-RPC method
-- against the real binary's stdio. Returns a structured result
-- with one boolean per method so the caller can attribute
-- regressions precisely.
--
-- Uses 'readCreateProcessWithExitCode' which feeds stdin and
-- reads stdout to completion — same pattern as the shell smoke
-- test (@printf ... | mcp@) that was shown to work reliably in
-- the dogfood.
runSmoke :: FilePath -> IO SmokeResult
runSmoke binary = do
  currentEnv <- getEnvironment
  -- Each request line is a single JSON object; the server reads
  -- one per line from stdin. Notifications (initialized,
  -- notifications/cancelled) MUST NOT carry an @id@; that's
  -- exactly how the server distinguishes them.
  let input = unlines
        [ "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},\"clientInfo\":{\"name\":\"e2e-smoke\",\"version\":\"0\"}}}"
        , "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}"
        , "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"
        , "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"resources/list\"}"
        , "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"resources/read\",\"params\":{\"uri\":\"haskell-flows://rules/workflow\"}}"
        , "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/cancelled\",\"params\":{\"requestId\":42}}"
        , "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/bogus\"}"
        ]
      cp = (proc binary [])
             { env = Just (("HASKELL_PROJECT_DIR", "/tmp/mcp-e2e-smoke") : currentEnv)
             }
  (ec, outStr, errStr) <- readCreateProcessWithExitCode cp input
  let toolCount  = countToolEntries outStr
      -- The server emits one JSON object per line; counting
      -- @"id":N@ markers tells us how many id-bearing replies
      -- arrived. Notifications produce zero replies, so we
      -- exploit that to verify the notification contract.
      idResponses = countOccurrences "\"id\":0" outStr
                  + countOccurrences "\"id\":1" outStr
                  + countOccurrences "\"id\":2" outStr
                  + countOccurrences "\"id\":3" outStr
                  + countOccurrences "\"id\":4" outStr
      okInit              = "\"protocolVersion\":\"2024-11-05\"" `isInfixOf` outStr
                          && hasResponseForId 0 outStr
      okInitedNoResponse  =
        -- 'initialized' is a notification. If it produced any
        -- reply we'd see one of the strange "id":null patterns
        -- some implementations emit. Easier: count expected
        -- replies (5: ids 0,1,2,3,4) and assert the total id
        -- responses match. That implicitly proves the two
        -- notifications produced none.
        idResponses == 5
      okToolsList         = toolCount >= 1
                          && hasResponseForId 1 outStr
                          && "\"tools\"" `isInfixOf` outStr
      okResourcesList     = hasResponseForId 2 outStr
                          && "haskell-flows://rules/workflow" `isInfixOf` outStr
      okResourcesRead     = hasResponseForId 3 outStr
                          && "\"contents\"" `isInfixOf` outStr
                          && "haskell-flows://rules/workflow" `isInfixOf` outStr
      okCancelNoResponse  = okInitedNoResponse  -- both notifications counted together
      okUnknownMethod     = hasResponseForId 4 outStr
                          && ("\"code\":-32601" `isInfixOf` outStr
                              || "method not found" `isInfixOf` lowerNoSpaces outStr)
      legacyOk = ec == ExitSuccess && okInit && okToolsList
  pure SmokeResult
    { srPassed                = legacyOk
    , srToolsAdvertised       = toolCount
    , srInitializeOk          = okInit
    , srInitializedNoResponse = okInitedNoResponse
    , srToolsListOk           = okToolsList
    , srResourcesListOk       = okResourcesList
    , srResourcesReadOk       = okResourcesRead
    , srCancelNoResponse      = okCancelNoResponse
    , srUnknownMethodErr      = okUnknownMethod
    , srLog                   = "exit=" <> show ec
                              <> " tools=" <> show toolCount
                              <> " idResponses=" <> show idResponses
                              <> (if null errStr then "" else "  stderr=" <> trimLong errStr)
    }
  where
    trimLong s
      | length s > 400 = take 400 s <> "…"
      | otherwise      = s

-- | Count how many tool entries the @tools/list@ response
-- advertises. We look for the @"name":"ghc_@ substring — one
-- per registered tool — which is a simpler and more
-- robust-to-whitespace probe than full JSON parsing here.
countToolEntries :: String -> Int
countToolEntries = countOccurrences "\"name\":\"ghc_"

-- | Substring count. Used both for tool entries and for response
-- id-occurrence checks. O(n*m); fine for the kilobyte-scale
-- responses the smoke test exchanges.
countOccurrences :: String -> String -> Int
countOccurrences needle = go 0
  where
    nLen = length needle
    go !n s
      | null s                = n
      | needle `isInfixOf` s  = go (n + 1) (drop nLen (dropUntilPat s))
      | otherwise             = n

    dropUntilPat s@(_:xs)
      | take (length needle) s == needle = s
      | otherwise                        = dropUntilPat xs
    dropUntilPat [] = []

-- | Did the server respond to a specific request id? The wire
-- format for a JSON-RPC reply embeds the same @id@ value as the
-- request. We compare against the literal @"id":N@ substring,
-- which is robust to whitespace because the server uses Aeson's
-- compact encoding.
hasResponseForId :: Int -> String -> Bool
hasResponseForId i = (("\"id\":" <> show i) `isInfixOf`)

-- | Lowercase + whitespace-stripped copy used for the
-- "method not found" alternative — both the JSON-RPC code AND
-- the human message are accepted, since the server might use
-- either format depending on whether the dispatcher hit the
-- early-return path or constructed a full error object.
lowerNoSpaces :: String -> String
lowerNoSpaces = map toLowerAscii . filter (`notElem` (" \t\n\r" :: String))
  where
    toLowerAscii c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c
