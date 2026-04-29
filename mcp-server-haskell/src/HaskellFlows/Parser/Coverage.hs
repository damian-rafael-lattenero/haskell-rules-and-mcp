-- | Parser for @hpc report@ output.
--
-- After @cabal test --enable-coverage@ runs, Cabal emits a summary
-- block like:
--
-- > 100% expressions used (12/12)
-- > 100% boolean coverage (0/0)
-- >      100% guards (0/0)
-- >      100% 'if' conditions (0/0)
-- >      100% qualifiers (0/0)
-- >  66% alternatives used (2/3)
-- >  75% local declarations used (3/4)
-- > 100% top-level declarations used (5/5)
--
-- Note that @hpc@ reports @100%@ for categories with zero applicable
-- program points (e.g. a project with no guards reports
-- @100% guards (0/0)@). Issue #89 — that quirk is mathematically
-- meaningless and inflates any naive average across categories. We
-- normalise it at the parse boundary: @total == 0@ rows surface with
-- @mPercent = Nothing@ and @mStatus = \"not_applicable\"@, mirroring
-- the @status: empty@ discriminator pattern that @ghc_check_project@
-- already uses for its zero-property gate. Consumers that want a
-- pure HPC dump still have @mCovered@ and @mTotal@; the
-- @\"not_applicable\"@ tag tells them to skip the row when computing
-- summary numbers like averages.
--
-- The result is ReDoS-safe — no regex, just a small line-based state
-- machine.
module HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  ) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | Aggregate of every metric line we recognised.
newtype CoverageReport = CoverageReport
  { crMetrics :: [Metric]
  }
  deriving stock (Eq, Show)

-- | One coverage dimension.
--
-- 'mCovered' / 'mTotal' come from the @(a/b)@ suffix on the same
-- line. 'mPercent' is 'Nothing' when @mTotal == 0@ (\"no applicable
-- program points\") and @'Just' p@ otherwise — never trust HPC's
-- leading-column percentage on a @0/0@ row, it always lies as
-- @100%@. 'mStatus' is the categorical discriminator:
--
--   * @\"covered\"@        when @covered == total > 0@
--   * @\"uncovered\"@      when @covered <  total > 0@
--   * @\"not_applicable\"@ when @total == 0@
--
-- See Issue #89 for the bug this shape fixes (the old @!Int@ percent
-- claimed @100%@ for @0/0@ rows and dragged the across-metrics
-- average upward).
data Metric = Metric
  { mLabel   :: !Text
  , mPercent :: !(Maybe Int)
  , mCovered :: !Int
  , mTotal   :: !Int
  , mStatus  :: !Text
  }
  deriving stock (Eq, Show)

-- | Parse the raw @hpc report@ output into a 'CoverageReport'.
--
-- Any lines that don't match the @NN% label (a/b)@ shape are ignored —
-- this keeps us resilient against cabal prefixing banners or footers
-- that change between versions.
parseCoverage :: Text -> CoverageReport
parseCoverage raw =
  CoverageReport { crMetrics = mapMaybe parseLine (T.lines raw) }

parseLine :: Text -> Maybe Metric
parseLine ln = do
  let stripped = T.strip ln
  (pct, rest1) <- takePercent stripped
  (covered, total, label) <- takeFractionAndLabel rest1
  pure (mkMetric (T.strip label) pct covered total)

-- | Smart constructor — collapses the @total == 0@ row to the
-- @\"not_applicable\"@ shape regardless of what HPC's leading-column
-- percent claimed.
mkMetric :: Text -> Int -> Int -> Int -> Metric
mkMetric label pct covered total
  | total <= 0 = Metric
      { mLabel   = label
      , mPercent = Nothing
      , mCovered = covered
      , mTotal   = total
      , mStatus  = "not_applicable"
      }
  | otherwise  = Metric
      { mLabel   = label
      , mPercent = Just pct
      , mCovered = covered
      , mTotal   = total
      , mStatus  = if covered >= total then "covered" else "uncovered"
      }

-- | Consume a leading @NN%@ (with optional leading whitespace) and
-- return the number plus the remainder after the @%@ sign.
takePercent :: Text -> Maybe (Int, Text)
takePercent t =
  let (digits, afterDigits) = T.span isDigit t
  in case (T.null digits, T.uncons afterDigits) of
       (False, Just ('%', rest)) -> do
         n <- readMaybe (T.unpack digits)
         pure (n, T.stripStart rest)
       _ -> Nothing

-- | Given text like @expressions used (12\/12)@ return
-- @(covered, total, label)@. Requires both a parenthesised fraction and
-- a non-empty label before it.
takeFractionAndLabel :: Text -> Maybe (Int, Int, Text)
takeFractionAndLabel t =
  case T.breakOn "(" t of
    (_, parenRest) | T.null parenRest -> Nothing
    (label, parenRest) ->
      let frac = T.takeWhile (/= ')') (T.drop 1 parenRest)
      in case T.breakOn "/" frac of
           (_, afterSlash) | T.null afterSlash -> Nothing
           (leftTxt, rightTxt) -> do
             l <- readMaybe (T.unpack (T.strip leftTxt))
             r <- readMaybe (T.unpack (T.strip (T.drop 1 rightTxt)))
             pure (l, r, label)
