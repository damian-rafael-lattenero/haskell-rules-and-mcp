-- | Pure boundary sanitisation for tool arguments.
--
-- These helpers originally lived in the legacy subprocess
-- 'HaskellFlows.Ghci.Session' module. They are pure — no
-- subprocess dependency — so after Wave 5 retired the subprocess
-- they live here as the single source of boundary-rejection
-- logic.
module HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  , sentinel
  , maxEvalBytes
  , maxExpressionBytes
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | The fixed end-of-output marker from the pre-Wave-5 subprocess
-- framing protocol. Kept as a literal constant so 'sanitizeExpression'
-- can reject user inputs that happen to contain it — defending
-- the one remaining use of the token (boundary rejection) without
-- pulling back the legacy transport.
sentinel :: Text
sentinel = "<<<GHCi-DONE-7f3a2b>>>"

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
