-- | Unit tests for logging audit-path, Bench.Budget parsing, Runner
-- warmup-discard, and nextStep Gate chain quality/length/golden dispatch.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.BudgetGate
  ( testLoggingAuditPathPresentWhenEnabled
  , testBudgetParsesCleanly
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import System.Environment (setEnv, unsetEnv)

import qualified HaskellFlows.Bench.Budget as Budget
import qualified HaskellFlows.Mcp.Logging as Logging
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..), allToolNames)

import Data.Aeson (Value, object, (.=))
import Data.Maybe (catMaybes, isJust)
import qualified Data.List as List
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

testLoggingAuditPathPresentWhenEnabled :: IO Bool

testLoggingAuditPathPresentWhenEnabled = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hf-audit-test"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  setEnv "HASKELL_FLOWS_AUDIT" "1"
  setEnv "HASKELL_PROJECT_DIR" dir
  ctx <- Logging.newLogContext "ghc_test"
  unsetEnv "HASKELL_FLOWS_AUDIT"
  unsetEnv "HASKELL_PROJECT_DIR"
  removePathForcibly dir
  pure $ case Logging.lcAuditPath ctx of
    Nothing   -> False
    Just path -> ".haskell-flows/audit.jsonl" `List.isSuffixOf` path

------------------------------------------------------------------------
-- Issue #96 Phase A — performance budget scaffold
------------------------------------------------------------------------

-- | Every constructor in 'ToolName' must have an entry in 'Budget.allBudgets'.
-- Catches gaps introduced when a new tool is added to 'ToolName' without
-- a corresponding budget row.

testBudgetParsesCleanly :: IO Bool

testBudgetParsesCleanly =
  pure $ all (isJust . Budget.lookupBudget) allToolNames

-- | No budget value is 0 ms — a zero p50 or p95 would always pass and
-- would be useless as a regression gate.

type GoldenRow = (String, ToolName, Value, Maybe ToolName)
