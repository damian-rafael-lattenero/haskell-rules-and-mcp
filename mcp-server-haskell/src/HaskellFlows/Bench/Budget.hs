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
  [ ( GhcLoad
    , ToolBudget 300  800   (Just 6000)
        "warm path; first call boots cabal v2-repl (cold-start: ~5s)")
  , ( GhcType
    , ToolBudget  50  200   Nothing
        "cached GHCi env; near-zero marginal cost")
  , ( GhcInfo
    , ToolBudget 100  300   Nothing
        "cached GHCi env; :i lookup only")
  , ( GhcEval
    , ToolBudget 100  500   Nothing
        "cached GHCi env; simple expression eval")
  , ( GhcQuickCheck
    , ToolBudget 500 1500   Nothing
        "100 QC runs + property persist; cabal-repl harness")
  , ( GhcHole
    , ToolBudget 200  600   Nothing
        "typed-hole query over loaded module")
  , ( GhcArbitrary
    , ToolBudget 200  500   Nothing
        "template generation from :i output; pure")
  , ( HoogleSearch
    , ToolBudget 200 1000   Nothing
        "hoogle subprocess; includes fork+exec overhead")
  , ( GhcWorkflow
    , ToolBudget  50  200   Nothing
        "inventory scan; no GHCi interaction")
  , ( GhcRegression
    , ToolBudget 300 1000   Nothing
        "replay stored property set; size-dependent on store")
  , ( GhcCheckModule
    , ToolBudget 500 1500   Nothing
        "strict load + warning gate + property replay")
  , ( GhcCoverage
    , ToolBudget 5000 10000 Nothing
        "full test rebuild with HPC instrumentation; subprocess-heavy")
  , ( GhcComplete
    , ToolBudget  50  200   Nothing
        ":complete repl; cached env")
  , ( GhcFormat
    , ToolBudget 300 1000   Nothing
        "fourmolu/ormolu subprocess; per-file read+write I/O")
  , ( GhcGate
    , ToolBudget 8000 15000 Nothing
        "cabal test + cabal build; scales with project size")
  , ( GhcQuickCheckExport
    , ToolBudget 200  500   Nothing
        "materialise test/Spec.hs from property store; I/O only")
  , ( GhcDeps
    , ToolBudget 1500 3000  Nothing
        "cabal solver invocation; version-constraint resolution")
  , ( GhcCreateProject
    , ToolBudget 200  500   Nothing
        "scaffold cabal + modules + test stub; no GHCi")
  , ( GhcDoc
    , ToolBudget 100  300   Nothing
        ":doc lookup; cached env + optional haddock data")
  , ( GhcGoto
    , ToolBudget  50  200   Nothing
        "parse Defined-at marker; pure text scan")
  , ( GhcRefactor
    , ToolBudget 800 2000   Nothing
        "rename/extract + snapshot-and-compile-verify roundtrip")
  , ( GhcBatch
    , ToolBudget 500 2000   Nothing
        "per-child average; actual budget = sum of included tools")
  , ( GhcLint
    , ToolBudget 2500 5000  Nothing
        "hlint subprocess; recursive project scan")
  , ( GhcToolchain
    , ToolBudget 200  500   Nothing
        "#94 Phase C: action-discriminated successor to \
        \ghc_toolchain_status + ghc_toolchain_warmup; budget covers \
        \binary-probe (status) and PATH warm-up (warmup) — both are \
        \subprocess-bound but cheap")
  , ( GhcValidateCabal
    , ToolBudget 200  500   Nothing
        "cabal check + duplicate-dep heuristic scan")
  , ( GhcCheckProject
    , ToolBudget 1500 4000  Nothing
        "check_module over every exposed + other-module in .cabal")
  , ( GhcSuggest
    , ToolBudget 100  400   Nothing
        "signature-driven property proposal; pure computation")
  , ( GhcSwitchProject
    , ToolBudget 100  300   Nothing
        "project dir swap; no recompile triggered")
  , ( GhcAddImport
    , ToolBudget 200  800   Nothing
        "AST-free import line injection; optional hoogle subprocess")
  , ( GhcApplyExports
    , ToolBudget 300  800   Nothing
        "export list insertion + compile-verify roundtrip")
  , ( GhcFixWarning
    , ToolBudget 500 1500   Nothing
        "warning-driven text rewrite + compile-verify roundtrip")
  , ( GhcImports
    , ToolBudget  50  200   Nothing
        "list live GHCi imports; cached env query")
  , ( GhcBrowse
    , ToolBudget 100  300   Nothing
        "module member listing; cached env query")
  , ( GhcBootstrap
    , ToolBudget  50  200   Nothing
        "preview/apply cabal.project bootstrap; no GHCi")
  , ( GhcPropertyLifecycle
    , ToolBudget 100  300   Nothing
        "property store list/drop; file I/O only")
  , ( GhcMove
    , ToolBudget 1500 4000  Nothing
        "multi-file rename + export fixup + compile-verify roundtrip")
  , ( GhcLab
    , ToolBudget 5000 15000 Nothing
        "per-binding suggest + QC across whole module; scales with module size")
  , ( GhcExplainError
    , ToolBudget 200  500   Nothing
        "diagnostic evidence package + optional patch verify roundtrip")
  , ( GhcPerf
    , ToolBudget 3000 8000  Nothing
        "expression eval x30 samples via cabal-repl harness")
  , ( GhcPropertyAudit
    , ToolBudget 300 1000   Nothing
        "pair-wise contradiction probe over property store")
  , ( GhcWitness
    , ToolBudget 4000 10000 Nothing
        "property eval x1000 with distribution labelling; cabal-repl harness")
  , ( GhcModules
    , ToolBudget 100  300   Nothing
        "#94 Phase B: action-discriminated successor to ghc_add_modules / \
        \ghc_remove_modules; budgets match the underlying handlers")
  ]

-- | Look up the budget for a specific tool.
-- Returns 'Nothing' when the tool has no entry (should not happen
-- after Phase A — the unit test 'testBudgetParsesCleanly' catches gaps).
lookupBudget :: ToolName -> Maybe ToolBudget
lookupBudget t = Map.lookup t allBudgets
