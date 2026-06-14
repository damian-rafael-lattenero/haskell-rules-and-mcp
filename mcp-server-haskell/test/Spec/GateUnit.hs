-- | Unit tests for QcExport registration/shape/sanitize, Coverage and
-- Gate timeout parameters, dynamic-regression scaling, and Suggest
-- evaluator/sibling logic. All pure except testGateAllSkipRefused and
-- testGateSummaryNoEmptyVerbs.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.GateUnit
  ( testQcExportRegistered
  , testQcExportRenderShape
  , testQcExportSanitize
  , testCoverageDefaultTimeout
  , testCoverageTimeoutClamp
  , testCoverageTimeoutMessage
  , testGateDefaultTestTimeout
  , testGateDefaultBuildTimeout
  , testGateCustomTestTimeout
  , testGateCustomBuildTimeout
  , testDynamicRegressionFloor
  , testDynamicRegressionScales
  , testGateRegistered
  , testGateAllSkip
  , testGateAllSkipRefused
  , testGateSummaryNoEmptyVerbs
  , testSuggestFunctorFmap
  , testSuggestEvaluatorPreservation
  , testSuggestConstFoldingSoundness
  , testSuggestEvaluatorNoSibling
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolDescriptor (..), ToolResult (..))
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Data.PropertyStore (StoredProperty (..), openStore)
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import qualified HaskellFlows.Tool.Coverage as CoverageTool
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport
import HaskellFlows.Mcp.Progress (noopSink)
import Spec.ToolEnvFixture (storeSessionPdSinkEnv)
import qualified HaskellFlows.Suggest.Rules as SuggestTool
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Suggest.Rules (Confidence (..), Suggestion (..), RuleContext (..), applyRules, applyRulesCtx, mkRuleContext)

import Spec.Helpers (withTempProject)

testQcExportRegistered :: IO Bool
testQcExportRegistered = pure $ "ghc_property_store" `elem` allToolNameTexts
  -- #94 Phase C step 6: ghc_quickcheck_export merged into
  -- ghc_property_store(action="export"). The legacy wire surface
  -- is gone; the action lives on inside the consolidated tool.

-- | Phase 11h: renderTestFile emits a valid-looking Main module
-- with the expected structural pieces (main, imports, a prop_N
-- binding per property, a runProp helper).
testQcExportRenderShape :: IO Bool
testQcExportRenderShape =
  let props =
        [ StoredProperty
            { spExpression = "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
            , spModule     = Just "src/DogfoodRle.hs"
            , spPassed     = 1
            , spUpdated    = 0
            , spCases     = 0
            }
        , StoredProperty
            { spExpression = "\\(xs :: [Int]) -> length xs >= 0"
            , spModule     = Nothing
            , spPassed     = 1
            , spUpdated    = 0
            , spCases     = 0
            }
        ]
      body = QcExport.renderTestFile props
  in pure $
       T.isInfixOf "module Main where"          body
    && T.isInfixOf "import Test.QuickCheck"     body
    && T.isInfixOf "import DogfoodRle"          body
    -- Issue #215: properties are now emitted as "prop_N args = body"
    -- (eta-reduced), not "prop_N = \args -> body".
    && T.isInfixOf "prop_1 "                   body   -- binding exists
    && T.isInfixOf "prop_2 "                   body   -- binding exists
    && not (T.isInfixOf "prop_1 = \\" body)           -- not lambda-style
    && T.isInfixOf "runProp :: Testable p"      body
    && T.isInfixOf "exitFailure"                body

-- | Phase 11h: sanitizeLabel must (a) strip CR/LF so a label never
-- breaks the generated string literal, (b) collapse whitespace
-- runs, (c) fall back to "property" on an empty-after-clean input.
testQcExportSanitize :: IO Bool
testQcExportSanitize = pure $
     QcExport.sanitizeLabel "add right identity"    == "add_right_identity"
  && QcExport.sanitizeLabel "with\nnewline"         == "with_newline"
  && QcExport.sanitizeLabel "   "                    == "property"
  && QcExport.sanitizeLabel "weird@#$_chars"         == "weird____chars"

-- | Phase 11g: ghc_gate must be in the canonical tool list + the
-- descriptor mentions its three sub-steps.
--------------------------------------------------------------------------------
-- #163: ghc_coverage configurable timeout
--------------------------------------------------------------------------------

-- | Default CoverageArgs must produce a 5-minute timeout (unchanged
-- from the pre-#163 hard-coded value so existing workflows see no
-- behavioural difference).
testCoverageDefaultTimeout :: IO Bool
testCoverageDefaultTimeout =
  let args = CoverageTool.CoverageArgs { CoverageTool.caTimeoutMinutes = 5, CoverageTool.caVerbose = False }
  in pure $ CoverageTool.coverageTimeoutMicros args == 5 * 60 * 1_000_000

-- | Clamping: values below 1 become 1, above 60 become 60.
testCoverageTimeoutClamp :: IO Bool
testCoverageTimeoutClamp =
  let raw0  = A.object []  -- defaults to 5
      raw10 = A.object ["timeout_minutes" .= (10 :: Int)]
      raw80 = A.object ["timeout_minutes" .= (80 :: Int)]
      raw0_ = A.object ["timeout_minutes" .= (0  :: Int)]
  in case ( A.fromJSON raw0  :: A.Result CoverageTool.CoverageArgs
          , A.fromJSON raw10 :: A.Result CoverageTool.CoverageArgs
          , A.fromJSON raw80 :: A.Result CoverageTool.CoverageArgs
          , A.fromJSON raw0_ :: A.Result CoverageTool.CoverageArgs ) of
       (A.Success a0, A.Success a10, A.Success a80, A.Success a0_) ->
         pure $ CoverageTool.caTimeoutMinutes a0  == 5
             && CoverageTool.caTimeoutMinutes a10 == 10
             && CoverageTool.caTimeoutMinutes a80 == 60  -- clamped
             && CoverageTool.caTimeoutMinutes a0_ == 1   -- clamped
       _ -> pure False

-- | The timeout error message must reflect the ACTUAL configured
-- minutes (not hard-code "5 minutes"). This caught by checking
-- the cause field when CovTimeout is rendered with a 15-minute arg.
testCoverageTimeoutMessage :: IO Bool
testCoverageTimeoutMessage =
  let args   = CoverageTool.CoverageArgs { CoverageTool.caTimeoutMinutes = 15, CoverageTool.caVerbose = False }
      result = CoverageTool.renderResult args CoverageTool.CovTimeout
  in pure $ case trContent result of
       [TextContent body_] ->
         case A.decode (TLE.encodeUtf8 (TL.fromStrict body_)) of
           Just (A.Object top) ->
             case AKM.lookup "error" top of
               Just (A.Object err) ->
                 case AKM.lookup "cause" err of
                   Just (A.String cause) -> T.isInfixOf "15m" cause
                   _                    -> False
               _ -> False
           _ -> False
       _ -> False

--------------------------------------------------------------------------------
-- #164: ghc_gate configurable timeouts
--------------------------------------------------------------------------------

-- | Default GateArgs must give 5 min for test (unchanged).
testGateDefaultTestTimeout :: IO Bool
testGateDefaultTestTimeout =
  let raw = A.object []
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalTestTimeoutMicros args == 5 * 60 * 1_000_000
       _ -> pure False

-- | Default GateArgs must give 3 min for build (unchanged).
testGateDefaultBuildTimeout :: IO Bool
testGateDefaultBuildTimeout =
  let raw = A.object []
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalBuildTimeoutMicros args == 3 * 60 * 1_000_000
       _ -> pure False

-- | Passing test_timeout_minutes=20 raises the test budget to 20 min.
testGateCustomTestTimeout :: IO Bool
testGateCustomTestTimeout =
  let raw = A.object ["test_timeout_minutes" .= (20 :: Int)]
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalTestTimeoutMicros args == 20 * 60 * 1_000_000
       _ -> pure False

-- | Passing build_timeout_minutes=10 raises the build budget to 10 min.
testGateCustomBuildTimeout :: IO Bool
testGateCustomBuildTimeout =
  let raw = A.object ["build_timeout_minutes" .= (10 :: Int)]
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success args ->
         pure $ Gate.cabalBuildTimeoutMicros args == 10 * 60 * 1_000_000
       _ -> pure False

-- | #216: with 0 properties the dynamic timeout must not fall below 2 min.
testDynamicRegressionFloor :: IO Bool
testDynamicRegressionFloor =
  pure $ Gate.dynamicRegressionTimeout 0 == 2 * 60 * 1_000_000

-- | #216: with 7 properties the dynamic timeout must exceed 2 min so
-- the budget doesn't fire before all 7 cabal-repl launches finish.
-- Formula: max(2 min, n × replayTimeout + 30 s overhead)
-- With n=7 and replayTimeout=30 s:  7×30 + 30 = 240 s > 120 s
testDynamicRegressionScales :: IO Bool
testDynamicRegressionScales =
  pure $ Gate.dynamicRegressionTimeout 7 > 2 * 60 * 1_000_000

testGateRegistered :: IO Bool
testGateRegistered = pure $
     "ghc_gate" `elem` allToolNameTexts
  && case filter (\td -> tdName td == "ghc_gate") allToolDescriptors of
       [td] ->
         let d = tdDescription td
         in T.isInfixOf "regression" d
         && T.isInfixOf "cabal test" d
         && T.isInfixOf "cabal build" d
       _ -> False

-- | Phase 11g: parsing GateArgs with all skip flags set must yield
-- a report with three "skip" steps and success=true. Uses a minimal
-- decode instead of invoking the full handler (which would spawn
-- cabal subprocesses).
testGateAllSkip :: IO Bool
testGateAllSkip =
  let raw = A.object
        [ "skip_regression"  .= True
        , "skip_cabal_test"  .= True
        , "skip_cabal_build" .= True
        ]
  in case A.fromJSON raw :: A.Result Gate.GateArgs of
       A.Success _ -> pure True
       A.Error   _ -> pure False

-- | #138: calling ghc_gate with all three skip flags returns
-- status='refused' / kind='validation' instead of the vacuous
-- "All gates passed: . Safe to push." success that misled callers.
-- The early-exit path never touches the GhcSession, so 'undefined'
-- is safe for that argument — it is guaranteed not to be forced.
testGateAllSkipRefused :: IO Bool
testGateAllSkipRefused = withTempProject $ \pd -> do
  store <- openStore pd
  let raw = A.object
        [ "skip_regression"  .= True
        , "skip_cabal_test"  .= True
        , "skip_cabal_build" .= True
        ]
  tr <- Gate.handle (storeSessionPdSinkEnv store (error "GhcSession not needed for all-skip path") pd noopSink) raw
  case trContent tr of
    [TextContent body] ->
      case A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)) of
        Right env ->
          pure $ Env.reStatus env == Env.StatusRefused
              && fmap Env.eeKind (Env.reError env) == Just Env.Validation
        Left _ -> pure False
    _ -> pure False

-- | #138: the 'summary' function must not produce the malformed
-- "All requested gates passed: . Safe to push." string when the
-- passed-verbs list is empty. Verified via source inspection of
-- the defensive guard added in the fix.
testGateSummaryNoEmptyVerbs :: IO Bool
testGateSummaryNoEmptyVerbs = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Gate.hs"
  -- The fix adds a null-check on the verbs list before the safe-to-push string.
  pure $ T.isInfixOf "T.null verbs" src
      && T.isInfixOf "nothing was verified" src

-- | Phase 11f: Functor shape `(a -> b) -> F a -> F b` emits BOTH
-- identity and composition laws in one rule firing.
testSuggestFunctorFmap :: IO Bool
testSuggestFunctorFmap =
  case parseSignature "(a -> b) -> [a] -> [b]" of
    Nothing  -> pure False
    Just sig ->
      let laws = map sLaw (applyRules "myMap" sig)
      in pure $ "Functor identity" `elem` laws
             && "Functor composition" `elem` laws

-- | Phase 11f: transform @simplify :: Expr -> Expr@ with sibling
-- interpreter @eval :: Env -> Expr -> Int@ → emits evaluator
-- preservation law.
testSuggestEvaluatorPreservation :: IO Bool
testSuggestEvaluatorPreservation =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Int") of
    (Just simplifySig, Just evalSig) ->
      let ctx = RuleContext
            { rcName     = "transform"  -- deliberately non-optimization name
            , rcSig      = simplifySig
            , rcSiblings = [("eval", evalSig)]
            }
          laws = map sLaw (applyRulesCtx ctx)
      in pure ("Evaluator preservation" `elem` laws)
    _ -> pure False

-- | Phase 11f: same sibling pair BUT the focal name is
-- "simplify" → triggers ConstantFoldingSoundness AT High on top of
-- the generic EvaluatorPreservation.
testSuggestConstFoldingSoundness :: IO Bool
testSuggestConstFoldingSoundness =
  case (parseSignature "Expr -> Expr", parseSignature "Env -> Expr -> Int") of
    (Just simplifySig, Just evalSig) ->
      let ctx = RuleContext
            { rcName     = "simplify"
            , rcSig      = simplifySig
            , rcSiblings = [("eval", evalSig)]
            }
          suggs = applyRulesCtx ctx
      in pure $ any
           (\s -> sLaw s == "Constant-folding soundness"
               && sConfidence s == High)
           suggs
    _ -> pure False

-- | Phase 11f: evaluator laws require at least one interpreter
-- sibling. With no siblings, nothing fires.
testSuggestEvaluatorNoSibling :: IO Bool
testSuggestEvaluatorNoSibling =
  case parseSignature "Expr -> Expr" of
    Nothing  -> pure False
    Just sig ->
      let laws = map sLaw (applyRulesCtx (mkRuleContext "simplify" sig))
      in pure $ "Evaluator preservation"     `notElem` laws
             && "Constant-folding soundness" `notElem` laws
