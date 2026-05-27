-- | Pure boundary sanitisation for tool arguments.
--
-- Single source of boundary-rejection logic for every tool that
-- accepts a user-supplied expression (ghc_eval / _type / _info /
-- _complete / _doc / _goto / _arbitrary / _quickcheck / _suggest).
-- Pure — no session or subprocess dependency — so unit tests pin
-- the contract without needing a live GHC API session.
module HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  , sanitizeDeclarations
  , sentinel
  , maxEvalBytes
  , maxExpressionBytes
  ) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T

-- | Historical end-of-output marker from the original subprocess
-- framing protocol (retired). Kept as a literal constant so
-- 'sanitizeExpression' can reject user inputs that happen to contain
-- it — belt-and-suspenders against any future reintroduction of
-- framed transport plus a stable invariant in the injection-guard
-- test suite.
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
  | OversizedIntegerLiteral
    -- ^ #127: expression contains a decimal literal with 20+ digits
    -- (i.e. ≥ 10^19 ≈ 2^63), or a power expression @N ^ E@ with
    -- E ≥ 64. Either form can produce a two-limb GMP 'Integer' that
    -- crashes the in-process GHC evaluator via an RTS segfault.
    --
    -- This is a policy boundary — the caller must simplify the
    -- expression to avoid the large 'Integer' literal.
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
  | hasLargeIntLiteral raw                   = Left OversizedIntegerLiteral
  | hasBigPowExponent raw                    = Left OversizedIntegerLiteral
  | otherwise                                = Right stripped
  where
    stripped = T.strip raw

-- | Boundary check for multi-line declaration blocks (e.g. function
-- definitions with type signatures, guards, or multiple equations).
--
-- Identical to 'sanitizeExpression' except newlines are permitted —
-- they are syntactically meaningful in declaration context.
--
-- Still rejects: empty input, sentinel injection, oversized input,
-- large integer literals.  Newline-injection is not a concern here
-- because the code goes through 'exprType' (type-check only, no
-- execution) wrapped in an explicit @let ... in ()@ block, so any
-- stray @import@ or @:!@ directive is a parse/type error, not an
-- escape hatch.
sanitizeDeclarations :: Text -> Either CommandError Text
sanitizeDeclarations raw
  | T.null stripped                   = Left EmptyInput
  | sentinel `T.isInfixOf` raw        = Left ContainsSentinel
  | T.length raw > maxExpressionBytes =
      Left (InputTooLarge (T.length raw) maxExpressionBytes)
  | hasLargeIntLiteral raw            = Left OversizedIntegerLiteral
  | hasBigPowExponent raw             = Left OversizedIntegerLiteral
  | otherwise                         = Right stripped
  where
    stripped = T.strip raw

-- | #127: 'True' when the text contains a run of 20 or more
-- consecutive decimal digits.
--
-- A 20-digit decimal number is ≥ 10^19 > 2^63, placing it in GMP's
-- two-limb representation on 64-bit systems. Two-limb integers trigger
-- an RTS segfault in the in-process GHC evaluator (GHC 9.12 / 9.10).
--
-- @maxBound :: Word64@ = 18446744073709551615 (20 digits) and
-- @2^64@ = 18446744073709551616 (20 digits) both hit this path.
hasLargeIntLiteral :: Text -> Bool
hasLargeIntLiteral = any (\run -> isDigit (T.head run) && T.length run >= 20)
                   . T.groupBy (\a b -> isDigit a == isDigit b)

-- | #127: 'True' when the text contains @^ N@ (with optional
-- surrounding whitespace) where N ≥ 64.
--
-- The expression @2^64@ computes an 'Integer' >= 2^64 at GHC's
-- constant-folding stage, which triggers the same two-limb GMP crash
-- as a bare large literal, even though neither @2@ nor @64@ alone is
-- a large literal.
hasBigPowExponent :: Text -> Bool
hasBigPowExponent = go
  where
    go txt = case T.breakOn "^" txt of
      (_, "")   -> False
      (_, rest) ->
        let after      = T.dropWhile (\c -> c == ' ' || c == '\t') (T.tail rest)
            (numPart, remainder) = T.span isDigit after
        in bigEnough numPart || go remainder

    bigEnough numTxt
      | T.null numTxt     = False
      | T.length numTxt > 2 = True   -- exponent >= 100 is definitely big
      | T.length numTxt == 2 =        -- two-digit: compare numerically
          case reads (T.unpack numTxt) :: [(Int, String)] of
            [(n, "")] -> n >= 64
            _         -> False
      | otherwise         = False
