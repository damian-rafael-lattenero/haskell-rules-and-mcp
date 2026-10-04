-- | Unit tests for the full nextStep routing suite: Create/Deps/Load/Suggest/
-- Qc/Regression/Refactor/Hole/ExplainError/CheckModule/CheckProject hints,
-- suggestOnError routing (#259), session ledger, and scratch-target resolution.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepFull
  ( testNextStepCreateProject
  , testSessionLedgerEmpty
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import qualified Data.Map.Strict as Map
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Mcp.WorkflowState as WS

import Spec.Helpers (withTempProject)

-- | The core happy-path chain: new scaffold → add deps.

testNextStepCreateProject :: IO Bool

testNextStepCreateProject =
  let payload = A.object [ "success" .= True, "files_written" .= ([] :: [Text]) ]
  in pure $ case suggestNext GhcProject True payload of
       Just ns -> nsTool ns == GhcDeps
       Nothing -> False

-- | After ghc_deps(add), reload.



testSessionLedgerEmpty :: IO Bool

testSessionLedgerEmpty = withTempProject $ \pd -> do
  m <- WS.loadLifetime pd
  pure (Map.null m)

-- | #A4 residual: ghc_info's nextStep example resolves the name from the
-- payload it echoes (ghc_batch-ready) instead of a "<placeholder>".

