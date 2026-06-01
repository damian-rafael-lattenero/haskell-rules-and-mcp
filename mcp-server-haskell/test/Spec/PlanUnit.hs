-- | Unit tests for 'Tool.Workflow' plan action (#284): multi-module
-- decomposition, confidence capping, and nextStep module resolution.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.PlanUnit
  ( testPlanMatchesModuleQc
  , testPlanLowConfidenceListsAlternatives
  , testPlanMultiModule
  , testPlanComplexGoalCapped
  , testNextStepResolvesSameModule
  , testNextStepModulesCreatedScratch
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Vector as Vector

import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Tool.Workflow as WorkflowTool

-- | #264: a concrete goal matches the right template + yields a chain.
testPlanMatchesModuleQc :: IO Bool
testPlanMatchesModuleQc =
  pure $ case WorkflowTool.planPayload "set up Expr.Foo with a QC roundtrip property" of
    A.Object o ->
      AKM.lookup "matched_template" o == Just (A.String "module-with-qc-property")
        && case AKM.lookup "chain" o of
             Just (A.Array a) -> not (null a)
             _                -> False
    _ -> False

-- | #264: a vague goal yields no match but lists alternatives.
testPlanLowConfidenceListsAlternatives :: IO Bool
testPlanLowConfidenceListsAlternatives =
  pure $ case WorkflowTool.planPayload "do the thing with stuff zzz" of
    A.Object o ->
      AKM.lookup "matched_template" o == Just A.Null
        && case AKM.lookup "alternative_templates" o of
             Just (A.Array a) -> not (null a)
             _                -> False
    _ -> False

-- | #284: a goal naming several modules scaffolds them ALL in one ghc_modules
-- step (matched_template = multi-module-scaffold) instead of collapsing to the
-- first, and carries a clarifying note.
testPlanMultiModule :: IO Bool
testPlanMultiModule =
  pure $ case WorkflowTool.planPayload
               "build modules Expr.Syntax, Expr.Eval, Expr.Pretty with QC" of
    A.Object o ->
      AKM.lookup "matched_template" o == Just (A.String "multi-module-scaffold")
        && AKM.member "note" o
        && case AKM.lookup "chain" o of
             Just (A.Array a) -> case Vector.toList a of
               (step1 : _) -> case step1 of
                 A.Object s -> case AKM.lookup "args" s of
                   Just (A.Object args) -> case AKM.lookup "modules" args of
                     Just (A.Array ms) -> length ms == 3
                     _                  -> False
                   _ -> False
                 _ -> False
               _ -> False
             _ -> False
    _ -> False

-- | #284: a long / multi-faceted goal that still maps to a single template has
-- its confidence capped (<= 0.5) and gains a note, so the agent treats the
-- chain as a starting slice rather than a complete plan.
testPlanComplexGoalCapped :: IO Bool
testPlanComplexGoalCapped =
  pure $ case WorkflowTool.planPayload
               "build an arithmetic expression evaluator with eval, algebraic \
               \simplify, and pretty-print parse roundtrip, all property-tested" of
    A.Object o ->
      case AKM.lookup "confidence" o of
        Just (A.Number n) -> n <= 0.5 && AKM.member "note" o
        _                 -> False
    _ -> False

-- | #270: a nextStep example's module_path is resolved to the payload's
-- concrete module (not a "<same module>" placeholder) when the tool
-- echoes module_path (ghc_refactor does) — so the chain is ghc_batch-ready.
testNextStepResolvesSameModule :: IO Bool
testNextStepResolvesSameModule =
  let payload = A.object
        [ "status"      A..= ("ok" :: T.Text)
        , "action"      A..= ("rename_local" :: T.Text)
        , "module_path" A..= ("src/Calc.hs" :: T.Text)
        ]
  in pure $ case suggestNext GhcRefactor True payload of
       Just ns -> case nsExample ns of
         Just (A.Object o) ->
           AKM.lookup "module_path" o == Just (A.String "src/Calc.hs")
         _ -> False
       Nothing -> False

-- | #262: ghc_modules(add) with created_files routes nextStep to a
-- design-first ghc_scratch chain (one scratch per created file + a
-- closing ghc_load on the first, with concrete paths).
testNextStepModulesCreatedScratch :: IO Bool
testNextStepModulesCreatedScratch =
  let payload = A.object
        [ "action"        A..= ("add" :: T.Text)
        , "created_files" A..= (["src/Expr/Pretty.hs", "src/Expr/Eval.hs"] :: [T.Text])
        ]
  in pure $ case suggestNext GhcModules True payload of
       Just ns -> nsTool ns == GhcScratch
                    && fmap length (nsChain ns) == Just 3
       Nothing -> False
