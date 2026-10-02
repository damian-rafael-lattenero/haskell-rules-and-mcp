-- | Per-tool latency budget table for @haskell-flows-mcp@ (#96 Phase A).
--
-- Every tool gets two latency budgets measured against the
-- reference project in @benchmarks\/Reference\/@:
--
--   * 'tbP50Ms'  — the typical latency (half of calls should be ≤ this).
--   * 'tbP95Ms'  — the upper-bound (sustained p95 violation = regression).
--
-- A 'tbColdStartMs' threshold is included for the five tools that pay a
-- one-time cabal v2-repl bootstrap cost on their first call; all others
-- carry @Nothing@.
--
-- Phase A ships the table with *initial-proposal* values from the
-- dogfood-pass measurements in issue #96. Phase B will replace every
-- entry with actual measured p50\/p95 from the timing harness, and only
-- then does the gate enforcement begin (Phase C).
--
-- Invariants checked by unit tests:
--   * 'allBudgets' covers every 'ToolName' constructor exactly once.
--   * No budget value is 0 ms (useless — would never fail).
module HaskellFlows.Bench.Budget
  ( ToolBudget (..)
  , BudgetTable
  , allBudgets
  , lookupBudget
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | Latency budget for a single tool. All times are in milliseconds.
data ToolBudget = ToolBudget
  { tbP50Ms       :: !Int        -- ^ p50 budget (ms); half of warm calls must be ≤ this
  , tbP95Ms       :: !Int        -- ^ p95 budget (ms); sustained violation = regression
  , tbColdStartMs :: !(Maybe Int) -- ^ cold-start surcharge (ms); 'Nothing' when n/a
  , tbNotes       :: !String     -- ^ rationale for this budget (for docs/Budget.md)
  }
  deriving stock (Show)

-- | Lookup table mapping every tool to its latency budget.
type BudgetTable = Map ToolName ToolBudget

-- | All tool budgets.  One entry per 'ToolName' constructor.
--
-- Phase A values are *initial proposals* from the dogfood-pass in
-- issue #96 §1. Phase B replaces each with a measured value.
allBudgets :: BudgetTable
allBudgets = Map.fromList
  --  Tool                   p50    p95   cold-start   notes
  [ ( GhcEval
    , ToolBudget 100  500   Nothing     "cached GHCi env; simple expression eval")
  , ( GhcCheck
    , ToolBudget  500 3000  Nothing
        "wave-2b composite: load/module/project/lint gates (max of the merged verbs)")
  , ( GhcProperty
    , ToolBudget  200 5000  Nothing
        "wave-2b composite: QC + store replay + arbitrary (max of merged verbs)")
  , ( GhcSession
    , ToolBudget   50  500  Nothing
        "wave-2b composite: workflow/toolchain/imports views")
  , ( GhcEdit
    , ToolBudget  200 2000  Nothing
        "wave-2b composite: refactor + imports + exports + fix + format")
  , ( GhcModule
    , ToolBudget  100  600  Nothing
        "wave-2b composite: cabal registration + scratchpad canvas")
  , ( GhcInspect
    , ToolBudget   50  300  Nothing
        "wave-2b composite: read-only introspection actions")
  , ( GhcGate
    , ToolBudget 8000 15000 Nothing
        "cabal test + cabal build; scales with project size")
  , ( GhcDeps
    , ToolBudget 1500 3000  Nothing
        "cabal solver invocation; version-constraint resolution")
  , ( GhcBatch
    , ToolBudget 500 2000   Nothing
        "per-child average; actual budget = sum of included tools")
  , ( GhcSuggest
    , ToolBudget 100  400   Nothing
        "signature-driven property proposal; pure computation")
  , ( GhcProject
    , ToolBudget 200  500   Nothing
        "#94 Phase C step 5: action-discriminated successor to \
        \ghc_create_project + ghc_switch_project + \
        \ghc_validate_cabal + ghc_bootstrap. The 200 ms / 500 ms \
        \budget is the upper envelope of the four legacy budgets \
        \(create / validate were 200/500; switch was 100/300; \
        \bootstrap was 50/200) — all four share a 'no GHCi, light \
        \subprocess' profile so we keep the worst-case bound rather \
        \than per-action thresholds.")
  , ( GhcExplainError
    , ToolBudget 200  500   Nothing
        "diagnostic evidence package + optional patch verify roundtrip")
  ]

-- | Look up the budget for a specific tool.
-- Returns 'Nothing' when the tool has no entry (should not happen
-- after Phase A — the unit test 'testBudgetParsesCleanly' catches gaps).
lookupBudget :: ToolName -> Maybe ToolBudget
lookupBudget t = Map.lookup t allBudgets
