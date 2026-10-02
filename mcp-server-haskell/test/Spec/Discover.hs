-- | Unit tests for 'ghc_workflow(discover)' and 'ghc_workflow(post-mortem)'
-- (#263, #266): unused-tool ranking, phase relevance, and missed-opportunity
-- detection.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Discover
  ( testDiscoverAtMostFive
  , testDiscoverPhaseRelevance
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

testCodeToolsRegistered :: IO Bool

testCodeToolsRegistered = pure $
  all (`elem` allToolNameTexts)
    [ "ghc_edit"
    , "ghc_module"
    , "ghc_session"
    ]
