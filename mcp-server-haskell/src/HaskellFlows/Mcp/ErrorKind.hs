-- | Reified tool-error discriminator (issue #45).
--
-- The server emits an @error_kind@ tag on every structured tool-error
-- response. Pre-refactor this was a raw 'Text' literal scattered
-- across 11 sites in 'HaskellFlows.Mcp.Server' + 'HaskellFlows.Tool.Eval'
-- and asserted by string equality from the E2E suite (notably
-- 'Scenarios.FlowTimeoutEnforcement'). A rename on either side could
-- silently desync the wire contract.
--
-- 'ErrorKind' anchors the contract. The wire format is unchanged —
-- 'renderErrorKind' produces the exact strings the previous code
-- emitted — but every emitter and asserter now goes through the same
-- single source of truth.
module HaskellFlows.Mcp.ErrorKind
  ( ErrorKind (..)
  , renderErrorKind
  , parseErrorKind
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Kinds of structured tool errors. Order is the order in which they
-- appear on the wire today (so any future enum-derived rendering stays
-- backwards compatible).
data ErrorKind
  = Timeout
    -- ^ Inner-eval budget tripped, or outer 10-min ceiling hit.
  | SessionExhausted
    -- ^ Legacy tag — the buffer-cap DoS guard from the old subprocess
    -- 'Session.hs' module. Retained because the E2E asserter accepts
    -- @session_exhausted | timeout@ for backwards compatibility while
    -- the in-process GHC API session has no equivalent emitter.
  | ToolException
    -- ^ Uncaught exception inside a handler.
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Render an 'ErrorKind' to its wire-format string. Single source of
-- truth — every emitter MUST go through this function.
renderErrorKind :: ErrorKind -> Text
renderErrorKind = \case
  Timeout          -> "timeout"
  SessionExhausted -> "session_exhausted"
  ToolException    -> "tool_exception"

-- | Parse a wire-format @error_kind@ string back to its constructor.
-- Returns 'Nothing' for unknown values; callers (typically test
-- assertions) can treat that as a contract violation.
parseErrorKind :: Text -> Maybe ErrorKind
parseErrorKind = flip Map.lookup reverseMap
  where
    reverseMap :: Map.Map Text ErrorKind
    reverseMap =
      Map.fromList [ (renderErrorKind k, k) | k <- [minBound .. maxBound] ]
