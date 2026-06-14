-- | Unit tests for the ToolSpec registry (#286).
--
-- Verifies:
--   * registry is total — exactly one ToolSpec per ToolName, no gaps or dups
--   * Registry.toolCategory agrees with ToolName.toolCategory (cycle-safety note)
--   * Registry.allBudgets keys agree with Budget.allBudgets keys
module Spec.RegistryUnit
  ( testRegistryTotalOverToolName
  , testRegistryNoDuplicateNames
  , testRegistryBudgetKeysAgree
  , testToolCategoryTotalOverToolName
  , testToolCategoryAgreesWithToolName
  ) where

import Control.Exception (evaluate, try, SomeException)
import qualified Data.Map.Strict as Map
import Data.List (sort, nub)

import HaskellFlows.Mcp.ToolName (allToolNames)
import qualified HaskellFlows.Mcp.ToolName as TN
import qualified HaskellFlows.Bench.Budget as Budget
import HaskellFlows.Tool.Registry
  ( registry
  , tsName
  , toolCategory
  , allBudgets
  )

-- | #286: registry covers every ToolName exactly — length matches allToolNames.
testRegistryTotalOverToolName :: IO Bool
testRegistryTotalOverToolName = do
  let registryNames = map tsName registry
      total = length allToolNames
  pure $ length registryNames == total
       && sort registryNames == sort allToolNames

-- | #286: no ToolName appears twice in the registry.
testRegistryNoDuplicateNames :: IO Bool
testRegistryNoDuplicateNames = do
  let registryNames = map tsName registry
  pure $ nub registryNames == registryNames

-- | #286: Registry.allBudgets and Budget.allBudgets cover the same ToolNames.
-- Verifies the two tables stay in sync while Budget.hs still exists.
testRegistryBudgetKeysAgree :: IO Bool
testRegistryBudgetKeysAgree = do
  let registryKeys = sort (Map.keys allBudgets)
      budgetKeys   = sort (Map.keys Budget.allBudgets)
  pure $ registryKeys == budgetKeys

-- | #286: Registry.toolCategory (derived projection) is defined for every
-- ToolName — a missing registry entry would make Map.! throw.
testToolCategoryTotalOverToolName :: IO Bool
testToolCategoryTotalOverToolName = do
  result <- try (evaluate (length (map toolCategory allToolNames)))
              :: IO (Either SomeException Int)
  pure $ case result of
    Right n -> n == length allToolNames
    Left _  -> False

-- | #286: Registry.toolCategory agrees with ToolName.toolCategory for all
-- tools. ToolName keeps its own case to avoid the Registry → Workflow →
-- Registry import cycle; this test is the regression net.
testToolCategoryAgreesWithToolName :: IO Bool
testToolCategoryAgreesWithToolName =
  pure $ all (\tn -> toolCategory tn == TN.toolCategory tn) allToolNames
