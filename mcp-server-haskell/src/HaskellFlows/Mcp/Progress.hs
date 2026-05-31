-- | #265: opt-in streaming progress for long-running tools.
--
-- A tool that takes tens of seconds (ghc_gate, ghc_check_project, …) is
-- otherwise silent from invocation to result. This module provides a
-- 'ProgressSink' that a handler can emit step events through; when wired, the
-- events are written to stdout as standard MCP @notifications/progress@
-- messages that Claude Desktop surfaces as a sub-spinner on the tool-call card.
--
-- Safety + opt-in (issue #265 criteria 5 + 6):
--
--   * The sink is a no-op UNLESS both (a) the env flag
--     @HASKELL_FLOWS_STREAM_PROGRESS=1@ is set AND (b) the client supplied a
--     @params._meta.progressToken@ (i.e. it actually subscribed). No token /
--     no flag ⇒ 'noopSink' ⇒ zero overhead, byte-identical behaviour.
--   * The stdio transport loop is single-threaded (read req → run handler to
--     completion → write response → next), so a sink writing to stdout DURING
--     a handler can never interleave with the final response or another call.
--     No lock is needed; the notifications simply precede the response line.
--
-- The notification shape is the spec's @notifications/progress@: a JSON-RPC
-- notification (no @id@) carrying @{progressToken, progress, total, message}@.
-- The issue's @elapsed_ms@ rides along as an extra param field (the spec
-- permits additional fields).
module HaskellFlows.Mcp.Progress
  ( ProgressEvent (..)
  , ProgressSink (..)
  , noopSink
  , stdoutProgressSink
  , progressNotification
  , streamingEnabled
  , mkProgressSink
  , progressTokenFrom
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

-- | One progress step a tool reports. @peCurrentStep@ / @peTotalSteps@ are
-- optional (a tool that can't enumerate its steps passes 'Nothing').
data ProgressEvent = ProgressEvent
  { peStep        :: !Text         -- ^ human-readable step label / status
  , peElapsedMs   :: !Int          -- ^ ms since the tool call began
  , peCurrentStep :: !(Maybe Int)
  , peTotalSteps  :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

-- | Where a tool's progress events go. A plain function wrapper so handlers
-- depend only on @ProgressEvent -> IO ()@, never on the transport.
newtype ProgressSink = ProgressSink { emitProgress :: ProgressEvent -> IO () }

-- | Discards every event. Used when streaming is off or the client did not
-- subscribe — guarantees zero behavioural change on the default path.
noopSink :: ProgressSink
noopSink = ProgressSink (const (pure ()))

-- | Build the JSON-RPC @notifications/progress@ message for an event under the
-- given client progress token. Pure — exposed for unit tests.
progressNotification :: Value -> ProgressEvent -> Value
progressNotification token ev =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method"  .= ("notifications/progress" :: Text)
    , "params"  .= object
        ( [ "progressToken" .= token
          , "message"       .= peStep ev
          , "elapsed_ms"    .= peElapsedMs ev
          ]
            ++ maybe [] (\c -> ["progress" .= c]) (peCurrentStep ev)
            ++ maybe [] (\t -> ["total" .= t]) (peTotalSteps ev)
        )
    ]

-- | Real sink: serialise the @notifications/progress@ message and write it as
-- one newline-delimited JSON line to stdout, flushing immediately. Safe to
-- call mid-handler because the transport loop is single-threaded (see the
-- module header).
stdoutProgressSink :: Value -> ProgressSink
stdoutProgressSink token = ProgressSink $ \ev -> do
  BL.hPutStr stdout (encode (progressNotification token ev))
  BS.hPutStr stdout "\n"
  hFlush stdout

-- | Is the @HASKELL_FLOWS_STREAM_PROGRESS@ feature flag enabled? Accepts
-- @1@ / @true@ / @yes@ / @on@ (case-insensitive).
streamingEnabled :: IO Bool
streamingEnabled = do
  mv <- lookupEnv "HASKELL_FLOWS_STREAM_PROGRESS"
  pure $ case fmap (map toLower) mv of
    Just v  -> v `elem` ["1", "true", "yes", "on"]
    Nothing -> False

-- | Choose a sink: a real stdout sink only when the flag is on AND the client
-- subscribed (sent a progress token); otherwise the no-op sink.
mkProgressSink :: Maybe Value -> IO ProgressSink
mkProgressSink mToken = do
  on <- streamingEnabled
  pure $ case (on, mToken) of
    (True, Just tok) -> stdoutProgressSink tok
    _                -> noopSink

-- | Extract @params._meta.progressToken@ from a @tools/call@ params object.
-- 'Nothing' when absent (the client did not subscribe). Pure — exposed for
-- unit tests.
progressTokenFrom :: Value -> Maybe Value
progressTokenFrom (Object o) = do
  metaV <- KeyMap.lookup "_meta" o
  case metaV of
    Object meta -> KeyMap.lookup "progressToken" meta
    _           -> Nothing
progressTokenFrom _ = Nothing
