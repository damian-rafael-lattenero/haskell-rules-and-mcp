-- | Pure boundary sanitisation for tool arguments.
--
-- Wave-1 extraction: these three names ('sanitizeExpression',
-- 'CommandError', 'maxExpressionBytes') were historically in the
-- legacy subprocess 'HaskellFlows.Ghci.Session' module. They are
-- pure — no subprocess dependency — so they live here now where the
-- in-process GHC API tools can import them without pulling in the
-- legacy transport.
--
-- The sentinel check still imports the framing sentinel from
-- 'HaskellFlows.Ghci.Sentinel' for backward compatibility with
-- scenarios that verify "input containing the framing sentinel is
-- rejected". When Wave 5 deletes the subprocess layer, the sentinel
-- constant can move here (or be deleted outright).
module HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  , maxEvalBytes
  , maxExpressionBytes
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Ghci.Sentinel (sentinel)

-- | Reasons a tool-input argument was rejected at the boundary.
--
-- Preserves the exact error taxonomy the scenarios rely on —
-- @FlowInjectionGuard@ / @FlowOversizedInput@ pin each constructor
-- to a specific client-visible message.
data CommandError
  = ContainsNewline
    -- ^ Input contained @\\n@ or @\\r@.
  | ContainsSentinel
    -- ^ Input literally contained the framing sentinel.
  | EmptyInput
    -- ^ After stripping whitespace, nothing remained.
  | InputTooLarge !Int !Int
    -- ^ @InputTooLarge observed cap@. Input exceeded
    -- 'maxExpressionBytes'.
  deriving stock (Eq, Show)

-- | Upper bound on an incoming expression. Symmetric with
-- 'maxEvalBytes' so a caller that fits their output under the
-- return cap can always fit their input too.
maxExpressionBytes :: Int
maxExpressionBytes = 64 * 1024

-- | Upper bound on bytes returned from a single evaluation.
maxEvalBytes :: Int
maxEvalBytes = 64 * 1024

-- | Boundary check for anything sent to the compiler as part of a
-- single-line command. Pure — no IO, no Session.
sanitizeExpression :: Text -> Either CommandError Text
sanitizeExpression raw
  | T.null stripped                          = Left EmptyInput
  | T.any (`elem` ("\n\r" :: String)) raw    = Left ContainsNewline
  | sentinel `T.isInfixOf` raw               = Left ContainsSentinel
  | T.length raw > maxExpressionBytes        =
      Left (InputTooLarge (T.length raw) maxExpressionBytes)
  | otherwise                                = Right stripped
  where
    stripped = T.strip raw
