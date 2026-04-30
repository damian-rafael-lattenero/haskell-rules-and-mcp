-- | Structured logging for the haskell-flows-mcp server.
--
-- Issue #98 Phase A — the closed-enum event API.
--
-- Design principles
-- ~~~~~~~~~~~~~~~~~
-- * JSON Lines to stderr (the MCP protocol channel is stdout; stderr is
--   the operator's). One JSON object per line, no trailing comma.
-- * Level-gated: controlled by 'HASKELL_FLOWS_LOG_LEVEL' env var.
--   Default: @info@. Levels: @debug < info < warn < error@.
-- * Redaction: string values longer than 'maxArgStringLen' are
--   truncated to their first 40 chars + "…" to prevent secrets from
--   leaking into log files (a client might pass an API key in an
--   expression argument).
-- * @trace_id@: pseudo-unique 6-char hex derived from POSIX microseconds.
--   Clients can supply their own via the JSON-RPC @params._trace_id@
--   extension; the server echoes it back in the response envelope.
-- * The module is /always-on/ by design (not opt-in via env var), because
--   the cost is one 'hPutStrLn stderr' per tool call — negligible against
--   any real Haskell workload.
module HaskellFlows.Mcp.Logging
  ( -- * Log context
    LogContext (..)
  , newLogContext
  , withTraceId
    -- * Log levels
  , LogLevel (..)
  , configuredLogLevel
    -- * Emit
  , logEvent
  , logToolStart
  , logToolEnd
  , logInternalEvent
    -- * Redaction
  , redactArgs
  , maxArgStringLen
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime)
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- Log context
-- ---------------------------------------------------------------------------

-- | Per-request logging context.
data LogContext = LogContext
  { lcTraceId :: !Text   -- ^ Correlation ID for this tool call
  , lcTool    :: !Text   -- ^ Tool name (empty string for non-tool events)
  } deriving (Show)

-- | Create a fresh 'LogContext' with a generated trace_id.
newLogContext :: Text -> IO LogContext
newLogContext toolName = do
  tid <- generateTraceId
  pure LogContext { lcTraceId = tid, lcTool = toolName }

-- | Override the trace_id on an existing context (client-supplied id).
withTraceId :: Text -> LogContext -> LogContext
withTraceId tid ctx = ctx { lcTraceId = tid }

-- | Generate a pseudo-unique 6-character hex trace_id.
-- Derived from POSIX microseconds — good enough for local session correlation.
generateTraceId :: IO Text
generateTraceId = do
  t <- getPOSIXTime
  let micros = (round (t * 1_000_000) :: Int) `mod` 16_777_216  -- 24-bit fits 6 hex digits
      hex    = showHex micros ""
      padded = replicate (6 - length hex) '0' <> hex
  pure (T.pack padded)

-- ---------------------------------------------------------------------------
-- Log levels
-- ---------------------------------------------------------------------------

-- | Severity levels in increasing order.
data LogLevel
  = LevelDebug  -- ^ Internal events (MVar, cache, subprocess).
  | LevelInfo   -- ^ Every successful tool call start + end.
  | LevelWarn   -- ^ Partial, refused, timeout, unavailable results.
  | LevelError  -- ^ Failed, internal_error results.
  deriving (Eq, Ord, Enum, Bounded, Show)

levelToText :: LogLevel -> Text
levelToText = \case
  LevelDebug -> "debug"
  LevelInfo  -> "info"
  LevelWarn  -> "warn"
  LevelError -> "error"

parseLogLevel :: String -> LogLevel
parseLogLevel s = case s of
  "debug" -> LevelDebug
  "info"  -> LevelInfo
  "warn"  -> LevelWarn
  "error" -> LevelError
  _       -> LevelInfo   -- unknown value → default to info

-- | Read the configured log level from 'HASKELL_FLOWS_LOG_LEVEL'.
-- Returns 'LevelInfo' when the env var is absent or unrecognised.
configuredLogLevel :: IO LogLevel
configuredLogLevel = do
  mLevel <- lookupEnv "HASKELL_FLOWS_LOG_LEVEL"
  pure (maybe LevelInfo parseLogLevel mLevel)

-- ---------------------------------------------------------------------------
-- Emit
-- ---------------------------------------------------------------------------

-- | Emit a single JSON Lines event to stderr, gated by the configured level.
logEvent :: LogLevel -> LogContext -> Text -> Value -> IO ()
logEvent level ctx event payload = do
  threshold <- configuredLogLevel
  if level < threshold
    then pure ()
    else do
      ts <- currentTimestamp
      let obj = object
            [ "ts"       .= ts
            , "level"    .= levelToText level
            , "trace_id" .= lcTraceId ctx
            , "event"    .= event
            , "tool"     .= (if T.null (lcTool ctx) then Null else String (lcTool ctx))
            , "data"     .= payload
            ]
      hPutStrLn stderr (TL.unpack (TLE.decodeUtf8 (encode obj)))

-- | @tool_call_start@: emit before invoking a tool handler.
logToolStart :: LogContext -> Value -> IO ()
logToolStart ctx args =
  logEvent LevelInfo ctx "tool_call_start"
    (object [ "args_summary" .= redactArgs args ])

-- | @tool_call_end@: emit after a tool handler returns.
-- 'status' is the response status string; 'durationMs' is wall-clock ms.
logToolEnd :: LogContext -> Text -> Int -> IO ()
logToolEnd ctx status durationMs =
  logEvent (levelForStatus status) ctx "tool_call_end"
    (object [ "status"      .= status
            , "duration_ms" .= durationMs
            ])

-- | @internal_event@: debug-level event for sub-tool sites.
logInternalEvent :: LogContext -> Text -> Value -> IO ()
logInternalEvent = logEvent LevelDebug

-- | Map a tool result status string to the appropriate log level.
-- ok/partial/no_match → info; refused/timeout/unavailable → warn; failed → error.
levelForStatus :: Text -> LogLevel
levelForStatus s = case s of
  "ok"          -> LevelInfo
  "partial"     -> LevelInfo
  "no_match"    -> LevelInfo
  "refused"     -> LevelWarn
  "timeout"     -> LevelWarn
  "unavailable" -> LevelWarn
  _             -> LevelError   -- failed, internal_error, etc.

-- ---------------------------------------------------------------------------
-- Redaction
-- ---------------------------------------------------------------------------

-- | Maximum verbatim string length in logged arguments.
-- Strings longer than this are truncated + "…" to prevent credential leakage.
maxArgStringLen :: Int
maxArgStringLen = 40

-- | Redact a JSON 'Value' for safe log emission.
-- * 'String' values > 'maxArgStringLen' → truncated with trailing @…@.
-- * 'Number', 'Bool', 'Null' → passed through verbatim.
-- * 'Array' → each element recursively redacted.
-- * 'Object' → each value recursively redacted (keys are kept — they are
--   schema-defined, not user-supplied).
redactArgs :: Value -> Value
redactArgs = \case
  String t
    | T.length t > maxArgStringLen ->
        String (T.take maxArgStringLen t <> "…")
    | otherwise -> String t
  Number n  -> Number n
  Bool b    -> Bool b
  Null      -> Null
  Array xs  -> Array (fmap redactArgs xs)
  Object km -> Object (KeyMap.map redactArgs km)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

currentTimestamp :: IO Text
currentTimestamp =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%.3qZ"
    <$> getCurrentTime
