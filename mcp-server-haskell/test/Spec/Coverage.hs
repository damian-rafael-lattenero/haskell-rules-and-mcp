-- | Unit tests for the @ghc_coverage@ parser + summariser — HPC row parsing
-- (#89 not-applicable, #176 boolean-parent exclusion, #177 always-True/False
-- annotations) and the #178 verbose/raw payload contract.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions. These are
-- pure parser/summarise/render tests over 'HaskellFlows.Parser.Coverage' +
-- 'HaskellFlows.Tool.Coverage' — no shared fixtures.
module Spec.Coverage
  ( testCoverageFull
  , testCoverageBanner
  , testCoverageMetricNotApplicable
  , testCoverageAverageSkipsNotApplicable
  , testCoverageAllMetricsApplicable
  , testCoverageAllNotApplicable
  , testCoverageSummariseExcludesBooleanParent
  , testCoverageAlwaysTrueParsed
  , testCoverageAlwaysFalseParsed
  , testCoverageAlwaysBothParsed
  , testCoverageRawOmittedByDefault
  , testCoverageRawIncludedWhenVerbose
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Parser.Coverage
import qualified HaskellFlows.Tool.Coverage as CoverageTool

testCoverageFull :: IO Bool
testCoverageFull =
  let raw = T.unlines
        [ "100% expressions used (12/12)"
        , " 66% alternatives used (2/3)"
        , " 75% local declarations used (3/4)"
        , "100% top-level declarations used (5/5)"
        ]
  in pure $ case crMetrics (parseCoverage raw) of
       [a, b, c, d] ->
            mPercent a == Just 100 && mTotal a == 12
         && mStatus  a == "covered"
         && mPercent b == Just 66  && mCovered b == 2 && mTotal b == 3
         && mStatus  b == "uncovered"
         && mPercent c == Just 75 && mStatus c == "uncovered"
         && mPercent d == Just 100 && mLabel d == "top-level declarations used"
         && mStatus  d == "covered"
       _ -> False

testCoverageBanner :: IO Bool
testCoverageBanner =
  let raw = T.unlines
        [ "Cabal version 3.12 — banner without fraction"
        , "100% expressions used (1/1)"
        , ""
        ]
  in pure (length (crMetrics (parseCoverage raw)) == 1)

-- | Issue #89: HPC reports @100%@ for categories with zero applicable
-- program points. The parser must collapse those rows to
-- @mPercent = Nothing, mStatus = "not_applicable"@ regardless of the
-- leading-column percent value HPC emitted.
testCoverageMetricNotApplicable :: IO Bool
testCoverageMetricNotApplicable =
  let raw = T.unlines
        [ "100% expressions used (19/19)"
        , "100% guards (0/0)"
        , "100% qualifiers (0/0)"
        , " 50% alternatives used (2/4)"
        ]
  in pure $ case crMetrics (parseCoverage raw) of
       [expr, guards_, quals, alts] ->
            -- Real-coverage rows untouched.
            mPercent expr   == Just 100 && mStatus expr   == "covered"
         && mPercent alts   == Just 50  && mStatus alts   == "uncovered"
            -- 0/0 rows normalised: percent dropped, status flagged.
         && isNothing (mPercent guards_) && mStatus guards_ == "not_applicable"
         && mTotal   guards_ == 0        && mCovered guards_ == 0
         && isNothing (mPercent quals)   && mStatus quals  == "not_applicable"
         && mTotal   quals  == 0         && mCovered quals  == 0
       _ -> False

-- | Issue #89 + #176: 'summarise' must skip 'not_applicable' rows AND
-- the 'boolean coverage' parent bucket when computing the headline
-- average. Anchor: 8 metrics, 2 are 0/0 (not_applicable) and
-- 'boolean coverage' is the parent of 'if' conditions (both have
-- total > 0). After excluding not_applicable + boolean coverage we
-- have 5 applicable metrics: expressions(89%), 'if' conditions(0%),
-- alternatives(50%), local(100%), top-level(100%).
-- Average = (89+0+50+100+100)/5 = 67%.
testCoverageAverageSkipsNotApplicable :: IO Bool
testCoverageAverageSkipsNotApplicable =
  let raw = T.unlines
        [ " 89% expressions used (17/19)"
        , "  0% boolean coverage (0/2)"
        , "100% guards (0/0)"
        , "  0% 'if' conditions (0/2)"
        , "100% qualifiers (0/0)"
        , " 50% alternatives used (2/4)"
        , "100% local declarations used (1/1)"
        , "100% top-level declarations used (2/2)"
        ]
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "5 applicable metrics" summary
         && T.isInfixOf "67%" summary
         -- Make sure the buggy answers never reappear.
         && not (T.isInfixOf "56%" summary)
         && not (T.isInfixOf "8 metrics" summary)
         && not (T.isInfixOf "6 metrics" summary)

-- | Issue #89 + #176 anchor: when all 8 metrics are applicable
-- (total > 0), the parent 'boolean coverage' is still excluded from
-- the average, leaving 7 leaf metrics. Catches a regression where
-- the exclusion is dropped under the all-applicable case.
testCoverageAllMetricsApplicable :: IO Bool
testCoverageAllMetricsApplicable =
  let raw = T.unlines
        [ "100% expressions used (12/12)"
        , " 50% boolean coverage (1/2)"
        , " 33% guards (1/3)"
        , "100% 'if' conditions (1/1)"
        , " 50% qualifiers (1/2)"
        , " 66% alternatives used (2/3)"
        , " 75% local declarations used (3/4)"
        , "100% top-level declarations used (5/5)"
        ]
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "7 applicable metrics" summary
         && T.isInfixOf "%" summary

-- | Issue #89 edge case: every metric is non-applicable. Don't
-- divide-by-zero; emit a coherent summary that names the count.
testCoverageAllNotApplicable :: IO Bool
testCoverageAllNotApplicable =
  let raw = T.unlines
        [ "100% guards (0/0)"
        , "100% qualifiers (0/0)"
        , "100% boolean coverage (0/0)"
        ]
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "No applicable HPC metrics" summary
         && T.isInfixOf "3 metrics seen" summary

-- | #176: 'summarise' must exclude 'boolean coverage' (parent bucket)
-- from the average even when it has total > 0. Here both boolean
-- coverage and its child 'if' conditions have total=2, so without the
-- fix both would be counted and produce a different average.
testCoverageSummariseExcludesBooleanParent :: IO Bool
testCoverageSummariseExcludesBooleanParent =
  let raw = T.unlines
        [ "100% expressions used (10/10)"
        , "  0% boolean coverage (0/2)"
        , "100% guards (0/0)"
        , "  0% 'if' conditions (0/2)"
        , "100% qualifiers (0/0)"
        ]
      -- Applicable (total > 0) AFTER excluding boolean coverage parent:
      -- expressions(100%) + 'if' conditions(0%) = 2 metrics, avg = 50%
      -- If boolean coverage were included: 3 metrics, avg = 33%
      summary = CoverageTool.summarise (crMetrics (parseCoverage raw))
  in pure $
       T.isInfixOf "2 applicable metrics" summary
         && T.isInfixOf "50%" summary
         && not (T.isInfixOf "33%" summary)
         && not (T.isInfixOf "3 applicable" summary)

-- | #177: 'parseCoverage' must capture the "N always True" HPC
-- annotation on 'if' condition lines.
testCoverageAlwaysTrueParsed :: IO Bool
testCoverageAlwaysTrueParsed =
  let raw = T.unlines
        [ "  0% 'if' conditions (0/2), 2 always True" ]
  in pure $ case crMetrics (parseCoverage raw) of
       [m] -> mAlwaysTrue m == 2 && mAlwaysFalse m == 0
       _   -> False

-- | #177: 'parseCoverage' must capture the "N always False" HPC
-- annotation.
testCoverageAlwaysFalseParsed :: IO Bool
testCoverageAlwaysFalseParsed =
  let raw = T.unlines
        [ "  0% 'if' conditions (0/3), 3 always False" ]
  in pure $ case crMetrics (parseCoverage raw) of
       [m] -> mAlwaysTrue m == 0 && mAlwaysFalse m == 3
       _   -> False

-- | #177: 'parseCoverage' must capture both annotations when HPC
-- emits "N always True, M always False".
testCoverageAlwaysBothParsed :: IO Bool
testCoverageAlwaysBothParsed =
  let raw = T.unlines
        [ "  0% 'if' conditions (0/5), 3 always True, 2 always False" ]
  in pure $ case crMetrics (parseCoverage raw) of
       [m] -> mAlwaysTrue m == 3 && mAlwaysFalse m == 2
       _   -> False

-- | #178: when 'verbose' is not set (default), 'renderResult' must
-- NOT include a 'raw' field in the result payload.
testCoverageRawOmittedByDefault :: IO Bool
testCoverageRawOmittedByDefault =
  let args   = CoverageTool.CoverageArgs
                 { CoverageTool.caTimeoutMinutes = 5
                 , CoverageTool.caVerbose        = False
                 }
      out    = "100% expressions used (5/5)\n"
      result = CoverageTool.renderResult args (CoverageTool.CovSuccess out)
  in pure $ case Env.reResult result of
       Just (A.Object res) -> not (AKM.member "raw" res)
       _                   -> False

-- | #178: when 'verbose=true', 'renderResult' MUST include the raw
-- cabal stdout in the result payload.
testCoverageRawIncludedWhenVerbose :: IO Bool
testCoverageRawIncludedWhenVerbose =
  let args   = CoverageTool.CoverageArgs
                 { CoverageTool.caTimeoutMinutes = 5
                 , CoverageTool.caVerbose        = True
                 }
      out    = "100% expressions used (5/5)\n"
      result = CoverageTool.renderResult args (CoverageTool.CovSuccess out)
  in pure $ case Env.reResult result of
       Just (A.Object res) -> AKM.member "raw" res
       _                   -> False
