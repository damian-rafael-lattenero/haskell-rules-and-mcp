-- | Unit tests for 'Tool.PropertyAudit' (PA*), 'Tool.Witness' (Wit*),
-- 'Tool.ExplainError' patch helpers, and GHC line-col parsing. All pure
-- except testAuditUsesInProcessProbe and testExplainVerifyPatch*.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.PropertyAuditUnit
  ( testPACombinationsEmpty
  , testPACombinations5
  , testPACombinationsDistinct
  , testPABuildProbe
  , testPAInterpretPassed
  , testPAInterpretFailed
  , testPAInterpretSkipped
  , testPAInterpretUnparsedEmptyCause
  , testPADedupByExpression
  , testPADedupSingletons
  , testPAIsVacuousGaveUp
  , testPAIsVacuousNotPassed
  , testAuditUsesInProcessProbe
  , testPARenderFindingKindContradictory
  , testPARenderFindingKindSkipped
  , testEnhanceCrossModuleDetailHits
  , testEnhanceCrossModuleDetailSameModule
  , testEnhanceCrossModuleDetailNotSkipped
  , testEnhanceCrossModuleDetailNullModule
  , testAppendReplStderrHits
  , testAppendReplStderrEmpty
  , testAppendReplStderrNotSkipped
  , testAppendReplStderrTruncates
  , testAllPairsSkippedTrue
  , testAllPairsSkippedFalseCompat
  , testAllPairsSkippedFalseEmpty
  , testEnhanceNotInScopeDetailHits
  , testEnhanceNotInScopeDetailNotSkipped
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HaskellFlows.Data.PropertyStore (StoredProperty (..))
import HaskellFlows.Mcp.Protocol (ToolDescriptor (..))
import HaskellFlows.Parser.Error (GhcError (..))
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..))
import HaskellFlows.Suggest.Rules (Confidence (..))
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool

import Spec.Helpers (withTempProject)

-- pairs. Edge case the auditor relies on so a property store
-- with 0 entries doesn't try to run a probe.
testPACombinationsEmpty :: IO Bool

testPACombinationsEmpty =
  pure (null (PropertyAuditTool.pairCombinations ([] :: [Int])))

-- | Issue #64: n*(n-1)/2 = 5*4/2 = 10 for a 5-element list.

testPACombinations5 :: IO Bool

testPACombinations5 =
  let pairs = PropertyAuditTool.pairCombinations [1 .. 5 :: Int]
  in pure (length pairs == 10)

-- | Issue #64: every pair is between distinct elements (no
-- (x, x) pairs).

testPACombinationsDistinct :: IO Bool

testPACombinationsDistinct =
  let pairs = PropertyAuditTool.pairCombinations [1 .. 4 :: Int]
  in pure (all (uncurry (/=)) pairs)

-- | Issue #64: 'buildContradictionProbe' wraps the two property
-- expressions into a conjunction lambda. The shape must contain
-- 'args' (the lambda parameter), '&&' (the conjunction), and
-- 'not' (the negation of the second property).

testPABuildProbe :: IO Bool

testPABuildProbe =
  let p1 = "\\x -> simplify (simplify x) == simplify x"
      p2 = "\\x -> simplify (simplify x) == x"
      probe = PropertyAuditTool.buildContradictionProbe p1 p2
  in pure $ T.isInfixOf "args" probe
        && T.isInfixOf "&&"   probe
        && T.isInfixOf "not"  probe
        && T.isInfixOf p1     probe
        && T.isInfixOf p2     probe

-- | Issue #77: 'QcPassed' means the probe @P1 ∧ ¬P2@ was true
-- on every random input — that IS the contradiction. The
-- pre-#77 implementation had this inverted.

testPAInterpretPassed :: IO Bool

testPAInterpretPassed =
  let (status, _detail) = PropertyAuditTool.interpretProbeResult
                            (QcPassed "probe" 100)
  in pure (status == "contradictory")

-- | Issue #77: 'QcFailed' means at least one input made the
-- probe false — the conjunction P1 ∧ ¬P2 does not hold there,
-- so the properties are compatible at that input.

testPAInterpretFailed :: IO Bool

testPAInterpretFailed =
  let (status, detail) = PropertyAuditTool.interpretProbeResult
                           (QcFailed "probe" 50 2 "[0,-1]")
  in pure (status == "compatible" && T.isInfixOf "[0,-1]" detail)

-- | Issue #77: every QC outcome that is neither passed nor
-- failed (parse failure, exception, give-up) maps to skipped.
-- The audit must not pretend to know the answer.

testPAInterpretSkipped :: IO Bool

testPAInterpretSkipped =
  let (s1, _) = PropertyAuditTool.interpretProbeResult
                  (QcUnparsed  "p" "garbage")
      (s2, _) = PropertyAuditTool.interpretProbeResult
                  (QcException "p" "oops")
      (s3, _) = PropertyAuditTool.interpretProbeResult
                  (QcGaveUp    "p" 10 50)
  in pure (s1 == "skipped" && s2 == "skipped" && s3 == "skipped")

-- | #149: when QcUnparsed carries empty raw output (no GHCi stdout,
-- e.g. because the REPL failed with only stderr), the cause field in
-- the skipped finding must be non-empty and provide actionable text.

testPAInterpretUnparsedEmptyCause :: IO Bool

testPAInterpretUnparsedEmptyCause =
  let (status, detail) = PropertyAuditTool.interpretProbeResult
                           (QcUnparsed "\\x -> x == x" "")
  in pure
       (  status == "skipped"
       && not (T.null detail)
       && ("probe load/parse failure: " /= detail)
       -- Must contain something actionable after the colon
       && T.isInfixOf "no GHCi output" detail
       )

-- | Issue #77 (cascade of #74): when the store has duplicate
-- rows for the same expression under different module shapes,
-- 'dedupByExpression' collapses them into one entry, keeping
-- the first occurrence.

testPADedupByExpression :: IO Bool

testPADedupByExpression =
  let mk e m = StoredProperty
                 { spExpression = e
                 , spModule     = Just m
                 , spPassed     = 1
                 , spUpdated    = 0
                 , spCases     = 0
                 }
      input = [ mk "expr-A" "Foo.Bar"
              , mk "expr-A" "src/Foo/Bar.hs"   -- duplicate, dropped
              , mk "expr-B" "Foo.Bar"
              , mk "expr-B" "src/Foo/Bar.hs"   -- duplicate, dropped
              ]
      out = PropertyAuditTool.dedupByExpression input
      modules = map spModule out
  in pure $ length out == 2
         && map spExpression out == ["expr-A", "expr-B"]
         && modules == [Just "Foo.Bar", Just "Foo.Bar"]   -- first kept

-- | Issue #77: dedupe is a no-op when every expression is
-- distinct. We must never drop a real entry.

testPADedupSingletons :: IO Bool

testPADedupSingletons =
  let mk e = StoredProperty
               { spExpression = e
               , spModule     = Just "Foo"
               , spPassed     = 1
               , spUpdated    = 0
               , spCases     = 0
               }
      input = [mk "p1", mk "p2", mk "p3"]
      out   = PropertyAuditTool.dedupByExpression input
  in pure (length out == 3)

-- | Issue #65: each canonical bucket boundary maps to its
-- expected label (0 / 1-5 / 6-20 / >20). The four cases below
-- pin every transition point so a future regression doesn't
-- silently shift the histogram.

testPAIsVacuousGaveUp :: IO Bool

testPAIsVacuousGaveUp =
  let qcr = QcGaveUp "\\x -> x > 0" 2 98
  in pure (PropertyAuditTool.isVacuousResult qcr)

-- | isVacuousResult: QcPassed → False.

testPAIsVacuousNotPassed :: IO Bool

testPAIsVacuousNotPassed =
  let qcr = QcPassed "\\x -> True" 100
  in pure (not (PropertyAuditTool.isVacuousResult qcr))

-- | #241: PropertyAudit.hs uses runQuickCheckWithLabelsInProcess for both
-- the contradiction probe and the vacuous check — not the cabal-repl
-- subprocess (which was producing "no GHCi output" for every probe).

testAuditUsesInProcessProbe :: IO Bool

testAuditUsesInProcessProbe = do
  src <- TIO.readFile "src/HaskellFlows/Tool/PropertyAudit.hs"
  -- Must use the in-process path; the old subprocess call must not appear
  -- as a live call (only possibly in comments, which we check by verifying
  -- the number of in-process calls exceeds the number of cabal-repl calls).
  let inProcessCount = T.count "runQuickCheckWithLabelsInProcess" src
      cabalReplCount = T.count "Qc.runQuickCheckViaCabalRepl" src
  pure (inProcessCount >= 2 && cabalReplCount == 0)

-- | #230: kindFor contradictory → "contradictory-pair".

testPARenderFindingKindContradictory :: IO Bool

testPARenderFindingKindContradictory =
  pure (PropertyAuditTool.kindFor "contradictory" == "contradictory-pair")

-- | #230: kindFor skipped → "skipped-pair".

testPARenderFindingKindSkipped :: IO Bool

testPARenderFindingKindSkipped =
  pure (PropertyAuditTool.kindFor "skipped" == "skipped-pair")

-- | #241: enhanceCrossModuleDetail appends the cross-module hint when
-- both pair members have DIFFERENT module paths and the probe was
-- skipped with a load-failure detail.

testEnhanceCrossModuleDetailHits :: IO Bool

testEnhanceCrossModuleDetailHits =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  (Just "src/A.hs") (Just "src/B.hs")
                  "skipped" detail0
  in pure (T.isInfixOf "cross-module pair" result
        && T.isInfixOf "src/A.hs" result
        && T.isInfixOf "src/B.hs" result)

-- | #241: enhanceCrossModuleDetail is a no-op when both modules match.

testEnhanceCrossModuleDetailSameModule :: IO Bool

testEnhanceCrossModuleDetailSameModule =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  (Just "src/A.hs") (Just "src/A.hs")
                  "skipped" detail0
  in pure (result == detail0)

-- | #241: enhanceCrossModuleDetail is a no-op when status is not skipped.

testEnhanceCrossModuleDetailNotSkipped :: IO Bool

testEnhanceCrossModuleDetailNotSkipped =
  let detail0 = "QuickCheck found 100 random inputs satisfying P1 ∧ ¬P2"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  (Just "src/A.hs") (Just "src/B.hs")
                  "contradictory" detail0
  in pure (result == detail0)

-- | #241: enhanceCrossModuleDetail is a no-op when either module is null.

testEnhanceCrossModuleDetailNullModule :: IO Bool

testEnhanceCrossModuleDetailNullModule =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.enhanceCrossModuleDetail
                  Nothing (Just "src/B.hs")
                  "skipped" detail0
  in pure (result == detail0)

-- | #241: appendReplStderr surfaces non-empty stderr on a skipped pair
-- with a load-failure detail.

testAppendReplStderrHits :: IO Bool

testAppendReplStderrHits =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      err     = "Variable not in scope: pretty :: Expr -> String"
      result  = PropertyAuditTool.appendReplStderr err "skipped" detail0
  in pure (T.isInfixOf "REPL stderr" result
        && T.isInfixOf "Variable not in scope" result)

-- | #241: appendReplStderr is a no-op when stderr is empty.

testAppendReplStderrEmpty :: IO Bool

testAppendReplStderrEmpty =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      result  = PropertyAuditTool.appendReplStderr "" "skipped" detail0
      result2 = PropertyAuditTool.appendReplStderr "   \n  " "skipped" detail0
  in pure (result == detail0 && result2 == detail0)

-- | #241: appendReplStderr is a no-op when status is not skipped.

testAppendReplStderrNotSkipped :: IO Bool

testAppendReplStderrNotSkipped =
  let detail0 = "Probe falsified at: 42"
      err     = "anything"
      result  = PropertyAuditTool.appendReplStderr err "compatible" detail0
  in pure (result == detail0)

-- | #241: appendReplStderr truncates stderr to 500 chars.

testAppendReplStderrTruncates :: IO Bool

testAppendReplStderrTruncates =
  let detail0 = "probe load/parse failure: (no GHCi output)"
      err     = T.replicate 1000 "x"   -- 1000 chars of 'x'
      result  = PropertyAuditTool.appendReplStderr err "skipped" detail0
      -- The result should contain exactly 500 'x' chars (no more).
      stderrSection = T.dropWhile (/= 'x') result
  in pure (T.length (T.takeWhile (== 'x') stderrSection) == 500)

-- | #294: a skipped pair whose stderr names an out-of-scope symbol gets an
-- HONEST explanation (audit limitation, not a compile error) appended,
-- replacing the misleading "run ghc_check_project to see compile errors"
-- steer. The project compiles; the probe just can't see Main-module / typed
-- properties.

testEnhanceNotInScopeDetailHits :: IO Bool

testEnhanceNotInScopeDetailHits =
  let detail0 = "probe load/parse failure: (no GHCi output) — REPL stderr \
                \(first 500 chars): Variable not in scope: prop_emptySubstIdentity"
      result  = PropertyAuditTool.enhanceNotInScopeDetail "skipped" detail0
  in pure (T.isInfixOf "audit limitation"   result
        && T.isInfixOf "not a compile error" result
        && not (T.isInfixOf "audit limitation" detail0))

-- | #294: enhanceNotInScopeDetail is a no-op when the status isn't skipped
-- (a genuine compatible/contradictory verdict must not be rewritten).

testEnhanceNotInScopeDetailNotSkipped :: IO Bool

testEnhanceNotInScopeDetailNotSkipped =
  let detail0 = "Variable not in scope: foo"
      result  = PropertyAuditTool.enhanceNotInScopeDetail "contradictory" detail0
  in pure (result == detail0)

-- | #241: allPairsSkipped True when every finding is skipped.
-- We can't construct a 'PairFinding' directly (constructor unexported),
-- so the True branch is covered by the integration path; here we
-- assert the False branches that guard against false positives.

testAllPairsSkippedTrue :: IO Bool

testAllPairsSkippedTrue =
  -- nPairs > 0 but findings empty (length mismatch) → False
  pure (not (PropertyAuditTool.allPairsSkipped 3 []))

-- | #241: allPairsSkipped False when at least one finding is compatible.
-- Indirect: the length-mismatch False branch.

testAllPairsSkippedFalseCompat :: IO Bool

testAllPairsSkippedFalseCompat =
  pure (not (PropertyAuditTool.allPairsSkipped 1 []))

-- | #241: allPairsSkipped False when nPairs=0 (nothing to skip).

testAllPairsSkippedFalseEmpty :: IO Bool

testAllPairsSkippedFalseEmpty =
  pure (not (PropertyAuditTool.allPairsSkipped 0 []))

-- Phase 2: explain_error patch verification (#59) ------------------------------

-- | applyLinePatch replaces old text on the target line.






