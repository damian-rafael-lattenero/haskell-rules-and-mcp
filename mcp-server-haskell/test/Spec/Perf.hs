-- | Unit tests for the @ghc_perf@ aggregation + baseline maths (#61) — the
-- 'aggregate' statistic shapes, 'regressionPct' delta, and the BaselineEntry
-- JSON roundtrip.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions. Pure
-- 'PerfTool' maths — no shared fixtures. Other ghc_perf tests (pairCombinations,
-- render) stay in the driver, which still imports PerfTool.
module Spec.Perf
  ( testPerfAggregateEmpty
  , testPerfAggregateSingle
  , testPerfAggregateOdd
  , testPerfAggregateEven
  , testPerfRegressionPctPositive
  , testPerfRegressionPctNegative
  , testPerfRegressionPctZeroBaseline
  , testPerfBaselineEntryRoundtrip
  ) where

import qualified Data.Aeson as A
import Data.Maybe (isNothing)

import qualified HaskellFlows.Tool.Perf as PerfTool

-- | Issue #61: 'aggregate' must handle every shape callers will
-- encounter — empty list, single sample, odd count (median is
-- the middle element), even count (median averages the two
-- middle elements).

testPerfAggregateEmpty :: IO Bool
testPerfAggregateEmpty =
  let s = PerfTool.aggregate []
  in pure (PerfTool.sCount s == 0
        && PerfTool.sMean s == 0
        && PerfTool.sMin s == 0
        && PerfTool.sMax s == 0)

testPerfAggregateSingle :: IO Bool
testPerfAggregateSingle =
  let s = PerfTool.aggregate [42]
  in pure (PerfTool.sCount s == 1
        && PerfTool.sMean s == 42
        && PerfTool.sMedian s == 42
        && PerfTool.sMin s == 42
        && PerfTool.sMax s == 42)

testPerfAggregateOdd :: IO Bool
testPerfAggregateOdd =
  let s = PerfTool.aggregate [10, 30, 20, 40, 50]
  in pure (PerfTool.sCount s == 5
        && PerfTool.sMin s == 10
        && PerfTool.sMax s == 50
        && PerfTool.sMedian s == 30
        && PerfTool.sMean s == 30)

-- | Even-count median averages the two middle samples after
-- sorting: [10,20,30,40] → median (20+30)/2 = 25.
testPerfAggregateEven :: IO Bool
testPerfAggregateEven =
  let s = PerfTool.aggregate [10, 30, 20, 40]
  in pure (PerfTool.sCount s == 4
        && PerfTool.sMedian s == 25
        && PerfTool.sMean s == 25)

-- | regressionPct: current 110, baseline 100 → +10% (positive = slower).
testPerfRegressionPctPositive :: IO Bool
testPerfRegressionPctPositive =
  pure (PerfTool.regressionPct 100.0 110.0 == Just 10.0)

-- | regressionPct: current 90, baseline 100 → -10% (negative = faster).
testPerfRegressionPctNegative :: IO Bool
testPerfRegressionPctNegative =
  pure (PerfTool.regressionPct 100.0 90.0 == Just (-10.0))

-- | regressionPct: baseline = 0 → Nothing (avoid divide-by-zero).
testPerfRegressionPctZeroBaseline :: IO Bool
testPerfRegressionPctZeroBaseline =
  pure (isNothing (PerfTool.regressionPct 0.0 100.0))

-- | BaselineEntry ToJSON → FromJSON roundtrip: mean_ns preserved.
testPerfBaselineEntryRoundtrip :: IO Bool
testPerfBaselineEntryRoundtrip =
  let entry   = PerfTool.BaselineEntry { PerfTool.beMeanNs = 12345.6 }
      encoded = A.encode entry
  in case A.decode encoded of
       Just decoded -> pure (PerfTool.beMeanNs decoded == 12345.6)
       Nothing      -> pure False
