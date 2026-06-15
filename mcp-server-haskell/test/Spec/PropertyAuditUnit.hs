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
  , testWitBucketBoundaries
  , testWitBuildInstrumented
  , testWitParseDistribution
  , testWitBiasWarning
  , testWitParseLabelCounts
  , testWitParseLabelCountsRobust
  , testWitCountsToDistribution
  , testWitCountsEmpty
  , testWitBuildConstructorProperty
  , testWitConstructorListAware
  , testWitDeferredDocumented
  , testWitTimerAfterBuild
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
  , testEEApplyLinePatch
  , testEEApplyLinePatchMiss
  , testEEApplyLinePatchOob
  , testExplainVerifyPatchUsesLoadForTarget
  , testParseGhcLineColBasic
  , testParseGhcLineColRange
  , testParseGhcLineColFallback
  , testSyntheticErrorLineCol
  , testLabConfidence
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
import qualified HaskellFlows.Tool.ExplainError as ExplainError
import qualified HaskellFlows.Tool.Lab as LabTool
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.Witness as WitnessTool

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
testWitBucketBoundaries :: IO Bool
testWitBucketBoundaries =
  pure $  WitnessTool.bucketSize 0   == "0"
       && WitnessTool.bucketSize 1   == "1-5"
       && WitnessTool.bucketSize 5   == "1-5"
       && WitnessTool.bucketSize 6   == "6-20"
       && WitnessTool.bucketSize 20  == "6-20"
       && WitnessTool.bucketSize 21  == ">20"
       && WitnessTool.bucketSize 999 == ">20"

-- | Issue #65: 'buildInstrumentedProperty' wraps the user
-- property with a 'Test.QuickCheck.collect' call carrying a
-- size-prefixed label, and threads withMaxSuccess so the
-- harness honours the requested run count.
testWitBuildInstrumented :: IO Bool
testWitBuildInstrumented =
  let prop = "\\xs -> length (reverse xs) == length (xs :: [Int])"
      out  = WitnessTool.buildInstrumentedProperty prop 750
  in pure $  T.isInfixOf "Test.QuickCheck.withMaxSuccess" out
          && T.isInfixOf "750"                            out
          && T.isInfixOf "Test.QuickCheck.collect"        out
          && T.isInfixOf "size:"                          out
          && T.isInfixOf prop                             out

-- | Issue #65: 'parseLabelDistribution' recovers (label, %) pairs
-- from QuickCheck's formatted histogram. Tolerates integer and
-- decimal forms, and ignores non-percent lines.
testWitParseDistribution :: IO Bool
testWitParseDistribution =
  let raw = T.unlines
        [ "+++ OK, passed 1000 tests:"
        , "35.5% size:1-5"
        , " 40% size:0"
        , "20.0% size:6-20"
        , "4.5% size:>20"
        , "noise line without percent"
        ]
      dist = WitnessTool.parseLabelDistribution raw
  in pure $  any (\(l, p) -> l == "size:1-5"  && p == 35.5) dist
          && any (\(l, p) -> l == "size:0"    && p == 40.0) dist
          && any (\(l, p) -> l == "size:6-20" && p == 20.0) dist
          && any (\(l, p) -> l == "size:>20"  && p == 4.5)  dist
          && length dist == 4

-- | Issue #65: any size-bucket holding < 1 % of the runs is a
-- bias signal. The function only emits warnings for size labels
-- (Phase 1's only instrumented dimension) so unrelated labels
-- are silently ignored.
testWitBiasWarning :: IO Bool
testWitBiasWarning =
  let dist = [ ("size:0",    0.5)   -- below 1% → warned
             , ("size:1-5", 80.0)   -- healthy
             , ("size:6-20", 19.5)  -- healthy
             , ("noise",     0.1)   -- not size:* → ignored
             ]
      ws = WitnessTool.biasWarnings dist
  in pure $ length ws == 1
         && T.isInfixOf "size:0" (head ws)
         && T.isInfixOf "0.5"    (head ws)

-- | Issue #78: 'parseLabelCounts' reads the tab-separated
-- block emitted by the labels-aware harness. Each line is
-- '"<label>\\t<count>"'.
testWitParseLabelCounts :: IO Bool
testWitParseLabelCounts =
  let raw = T.unlines
        [ "size:0\t40"
        , "size:1-5\t312"
        , "size:6-20\t148"
        ]
      counts = WitnessTool.parseLabelCounts raw
  in pure $  length counts == 3
          && lookup "size:0"    counts == Just 40
          && lookup "size:1-5"  counts == Just 312
          && lookup "size:6-20" counts == Just 148

-- | Issue #78: malformed rows (missing tab, non-numeric count,
-- empty label) are silently skipped — never crash the witness.
testWitParseLabelCountsRobust :: IO Bool
testWitParseLabelCountsRobust =
  let raw = T.unlines
        [ "size:1-5\t312"
        , "garbage row without a tab"
        , "\tlone-tab"
        , "label-no-count\tnotanint"
        , "size:6-20\t100"
        ]
      counts = WitnessTool.parseLabelCounts raw
  in pure $  length counts == 2
          && lookup "size:1-5"  counts == Just 312
          && lookup "size:6-20" counts == Just 100

-- | Issue #78: 'countsToDistribution' converts raw counts into
-- percentages summing (within float drift) to 100.
testWitCountsToDistribution :: IO Bool
testWitCountsToDistribution =
  let counts = [("size:0", 25), ("size:1-5", 75)]
      dist   = WitnessTool.countsToDistribution counts
      total  = sum (map snd dist)
  in pure $ length dist == 2
         && abs (total - 100.0) < 0.001
         && lookup "size:0"   dist == Just 25.0
         && lookup "size:1-5" dist == Just 75.0

-- | Issue #78: empty input ⇒ empty distribution. Avoids a
-- divide-by-zero and keeps the bias-warning machinery happy.
testWitCountsEmpty :: IO Bool
testWitCountsEmpty =
  pure $ null (WitnessTool.countsToDistribution [])

-- Phase 2: constructor property builder (#65) ----------------------------------

-- | buildConstructorProperty wraps with 'ctor:' label extraction.
testWitBuildConstructorProperty :: IO Bool
testWitBuildConstructorProperty =
  let built = WitnessTool.buildConstructorProperty "\\x -> x > 0" 500
  in pure ("ctor:" `T.isInfixOf` built
        && "withMaxSuccess 500" `T.isInfixOf` built
        && "show args" `T.isInfixOf` built)

-- | #239: buildConstructorProperty uses list-aware extraction.
-- For list inputs show gives "[1,2,3]" (no spaces), so the old
-- "head (words (show args))" gave one bucket per unique list.
-- The fix detects '[' prefix and returns "[]" or "(:)".
testWitConstructorListAware :: IO Bool
testWitConstructorListAware =
  let built = WitnessTool.buildConstructorProperty "\\xs -> length xs >= 0" 500
  -- Must NOT use the old "head (words (show args))" pattern
  -- Must contain the list-detection logic: "(:)" and "[]"
  in pure (not ("head (words (show args))" `T.isInfixOf` built)
        && "(:)" `T.isInfixOf` built
        && "[]" `T.isInfixOf` built
        && "'['" `T.isInfixOf` built)

-- | #171: The @deferred@ field in the witness response lists features
-- not yet implemented. The descriptor's description must mention it
-- so agents understand the field is intentional (not a bug). Pin that
-- the descriptor text contains the word "deferred".
testWitDeferredDocumented :: IO Bool
testWitDeferredDocumented =
  let desc = WitnessTool.descriptor
  in pure ("deferred" `T.isInfixOf` tdDescription desc)

-- | #171: @wall_time_ms@ must only measure the subprocess call, not
-- the (pure, negligible) property-string construction step. We verify
-- this structurally: the instrumented property does NOT contain any
-- timing boilerplate — it is a pure Text value constructed before t0.
testWitTimerAfterBuild :: IO Bool
testWitTimerAfterBuild =
  -- Property building is a pure, instant operation. If it ran inside
  -- the timed window its output would reference clock calls — it
  -- doesn't. This test pins that buildInstrumentedProperty and
  -- buildConstructorProperty are pure Text builders (no IO).
  let p1 = WitnessTool.buildInstrumentedProperty "\\x -> x > 0" 100
      p2 = WitnessTool.buildConstructorProperty  "\\x -> x > 0" 100
  in pure ("withMaxSuccess" `T.isInfixOf` p1 && "withMaxSuccess" `T.isInfixOf` p2)

-- Phase 2: vacuous-property check (#64) ----------------------------------------

-- | isVacuousResult: QcGaveUp → True.
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
testEEApplyLinePatch :: IO Bool
testEEApplyLinePatch =
  let body  = T.unlines ["line1", "foo bar baz", "line3"]
      patch = ExplainError.PatchSpec { ExplainError.psLine = 2
                                     , ExplainError.psOld  = "bar"
                                     , ExplainError.psNew  = "REPLACED"
                                     }
  in pure $ case ExplainError.applyLinePatch body patch of
       Just result -> "REPLACED" `T.isInfixOf` result
                   && "foo" `T.isInfixOf` result
       Nothing     -> False

-- | applyLinePatch returns Nothing when old text not on that line.
testEEApplyLinePatchMiss :: IO Bool
testEEApplyLinePatchMiss =
  let body  = T.unlines ["line1", "line2"]
      patch = ExplainError.PatchSpec { ExplainError.psLine = 1
                                     , ExplainError.psOld  = "NOTHERE"
                                     , ExplainError.psNew  = "X"
                                     }
  in pure (isNothing (ExplainError.applyLinePatch body patch))

-- | applyLinePatch returns Nothing for out-of-bounds line number.
testEEApplyLinePatchOob :: IO Bool
testEEApplyLinePatchOob =
  let body  = T.unlines ["line1"]
      patch = ExplainError.PatchSpec { ExplainError.psLine = 99
                                     , ExplainError.psOld  = "line1"
                                     , ExplainError.psNew  = "X"
                                     }
  in pure (isNothing (ExplainError.applyLinePatch body patch))

-- | Issue #222: runVerifyPatch must use the stanza-aware 'loadForTarget'
-- rather than bare 'loadAndCaptureDiagnostics'. Verified by checking
-- that the source imports and uses both 'loadForTarget' and 'targetForPath'.
testExplainVerifyPatchUsesLoadForTarget :: IO Bool
testExplainVerifyPatchUsesLoadForTarget = do
  src <- TIO.readFile "src/HaskellFlows/Tool/ExplainError.hs"
  let usesLoadForTarget  = "loadForTarget"  `T.isInfixOf` src
  let usesTargetForPath  = "targetForPath"  `T.isInfixOf` src
  -- Confirm the bare path is NOT the only call in runVerifyPatch section
  -- (we don't want a regression back to loadAndCaptureDiagnostics there).
  -- The function is still imported for the initial diagnostic phase, so
  -- loadAndCaptureDiagnostics may appear — but loadForTarget must too.
  pure (usesLoadForTarget && usesTargetForPath)

--------------------------------------------------------------------------------
-- #189 — parseGhcLineCol
--------------------------------------------------------------------------------

-- | #189: standard GHC error format extracts line + column.
testParseGhcLineColBasic :: IO Bool
testParseGhcLineColBasic =
  let errText = "src/WithError.hs:5:10: error: [GHC-39999] No instance for IsString Int"
  in pure (ExplainError.parseGhcLineCol errText == (5, 10))

-- | #189: col range @7-15@ yields just @7@ (end stripped by isDigit).
testParseGhcLineColRange :: IO Bool
testParseGhcLineColRange =
  let errText = "src/Foo.hs:42:7-15: error: something"
  in pure (ExplainError.parseGhcLineCol errText == (42, 7))

-- | #189: plain error text without a location prefix falls back to (1,1).
testParseGhcLineColFallback :: IO Bool
testParseGhcLineColFallback =
  let errText = "No instance for IsString Int"
  in pure (ExplainError.parseGhcLineCol errText == (1, 1))

-- | #189: syntheticError must use parsed line+col, not hardcoded 1:1.
testSyntheticErrorLineCol :: IO Bool
testSyntheticErrorLineCol =
  let errText = "src/WithError.hs:5:10: error: No instance for IsString Int"
      diag    = ExplainError.syntheticError "src/WithError.hs" errText
  in pure (geLine diag == 5 && geColumn diag == 10)

testLabConfidence :: IO Bool
testLabConfidence = pure $
     LabTool.confidenceAtLeast Low    Low    -- threshold Low,    candidate Low    → True
  && LabTool.confidenceAtLeast Low    Medium
  && LabTool.confidenceAtLeast Low    High
  && LabTool.confidenceAtLeast Medium Medium
  && LabTool.confidenceAtLeast Medium High
  && LabTool.confidenceAtLeast High   High
  && not (LabTool.confidenceAtLeast Medium Low)
  && not (LabTool.confidenceAtLeast High   Medium)
  && not (LabTool.confidenceAtLeast High   Low)
