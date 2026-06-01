-- | Unit tests for 'ghc_workflow(discover)' and 'ghc_workflow(post-mortem)'
-- (#263, #266): unused-tool ranking, phase relevance, and missed-opportunity
-- detection.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Discover
  ( testDiscoverExcludesCalled
  , testDiscoverAtMostFive
  , testDiscoverPhaseRelevance
  , testPostMortemMissedScratch
  , testPostMortemCounts
  , testCodeToolsRegistered
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import HaskellFlows.Mcp.Server (allToolNameTexts)
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Mcp.WorkflowState as WS
import qualified HaskellFlows.Tool.Workflow as WorkflowTool

-- | #263: discover excludes tools already called this session.
testDiscoverExcludesCalled :: IO Bool
testDiscoverExcludesCalled = do
  ref <- WS.newWorkflowStateRef
  WS.trackTool ref GhcScratch True (A.object [])
  s <- WS.readState ref
  pure (GhcScratch `notElem` WorkflowTool.discoverRanked s WS.PhaseDeveloping)

-- | #263: discover returns at most 5 suggestions (fresh session).
testDiscoverAtMostFive :: IO Bool
testDiscoverAtMostFive = do
  ref <- WS.newWorkflowStateRef
  s <- WS.readState ref
  pure (length (WorkflowTool.discoverRanked s WS.PhaseDeveloping) == 5)

-- | #263: phase-relevant tools rank in — ghc_gate in PhaseReadyToPush.
testDiscoverPhaseRelevance :: IO Bool
testDiscoverPhaseRelevance = do
  ref <- WS.newWorkflowStateRef
  s <- WS.readState ref
  pure (GhcGate `elem` WorkflowTool.discoverRanked s WS.PhaseReadyToPush)

-- | #266: post-mortem flags "never used ghc_scratch" after enough calls.
testPostMortemMissedScratch :: IO Bool
testPostMortemMissedScratch = do
  base <- WS.readState =<< WS.newWorkflowStateRef
  let ws = base { WS.wsToolCalls  = 10
                , WS.wsEverCalled = Set.fromList [GhcLoad, GhcType] }
  pure (any (T.isInfixOf "ghc_scratch") (WS.sessionMissedOpportunities ws))

-- | #266: post-mortem payload reports cumulative counts (unique + unused).
testPostMortemCounts :: IO Bool
testPostMortemCounts = do
  base <- WS.readState =<< WS.newWorkflowStateRef
  let now = posixSecondsToUTCTime 0
      ws  = base { WS.wsToolCalls  = 7
                 , WS.wsEverCalled = Set.fromList [GhcLoad, GhcSuggest, GhcQuickCheck] }
  case WorkflowTool.postMortemPayload ws Map.empty now of
    A.Object o ->
      pure $ AKM.lookup "tools_called" o == Just (A.Number 7)
          && AKM.lookup "tools_unique" o == Just (A.Number 3)
          && AKM.lookup "tools_unused" o == Just (A.Number 33)
    _ -> pure False

-- | Phase 11j: all 5 Code tools registered in the inventory.
testCodeToolsRegistered :: IO Bool
testCodeToolsRegistered = pure $
  all (`elem` allToolNameTexts)
    [ "ghc_add_import"
    , "ghc_modules"
    , "ghc_apply_exports"
    , "ghc_fix_warning"
    , "ghc_imports"
    ]
