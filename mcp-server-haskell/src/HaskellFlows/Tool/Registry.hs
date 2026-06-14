-- | Single source of truth for per-tool metadata (#286).
--
-- Every tool has exactly one 'ToolSpec' entry here.  The following
-- functions are /derived projections/ of 'registry' — adding a new
-- tool is two edits: one 'ToolName' constructor in
-- "HaskellFlows.Mcp.ToolName" (compiler-enforced) and one 'ToolSpec'
-- here:
--
--   * 'toolCategory'       — replaces the @ToolName.toolCategory@ case
--   * 'allToolDescriptors' — replaces the @Server.allToolDescriptors@ list
--   * 'allBudgets'         — replaces the @Budget.allBudgets@ map
--   * 'handlerFor'         — replaces the @Server.handlerFor@ case
--
-- @toolVersion@ stays in "HaskellFlows.Mcp.ToolName" (Protocol
-- dependency; see note on 'tsVersion').
module HaskellFlows.Tool.Registry
  ( ToolSpec (..)
  , registry
  , byName
    -- * Derived projections
  , toolCategory
  , allToolDescriptors
  , allBudgets
  , handlerFor
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import HaskellFlows.Bench.Budget (ToolBudget (..), BudgetTable)
import HaskellFlows.Mcp.Protocol (ToolDescriptor)
import HaskellFlows.Mcp.ToolName
  ( ToolName (..)
  , ToolCategory (..)
  , toolVersion
  )
import HaskellFlows.Tool.Env (ToolHandler)

import qualified HaskellFlows.Tool.AddImport       as AddImportTool
import qualified HaskellFlows.Tool.ApplyExports    as ApplyExportsTool
import qualified HaskellFlows.Tool.Arbitrary       as ArbitraryTool
import qualified HaskellFlows.Tool.Batch           as BatchTool
import qualified HaskellFlows.Tool.Browse          as BrowseTool
import qualified HaskellFlows.Tool.CheckModule     as CheckModuleTool
import qualified HaskellFlows.Tool.CheckProject    as CheckProjectTool
import qualified HaskellFlows.Tool.Complete        as CompleteTool
import qualified HaskellFlows.Tool.Coverage        as CoverageTool
import qualified HaskellFlows.Tool.Deps            as DepsTool
import qualified HaskellFlows.Tool.Doc             as DocTool
import qualified HaskellFlows.Tool.Eval            as EvalTool
import qualified HaskellFlows.Tool.ExplainError    as ExplainErrorTool
import qualified HaskellFlows.Tool.FixWarning      as FixWarningTool
import qualified HaskellFlows.Tool.Format          as FormatTool
import qualified HaskellFlows.Tool.Gate            as GateTool
import qualified HaskellFlows.Tool.Goto            as GotoTool
import qualified HaskellFlows.Tool.Hole            as HoleTool
import qualified HaskellFlows.Tool.Hoogle          as HoogleTool
import qualified HaskellFlows.Tool.Imports         as ImportsTool
import qualified HaskellFlows.Tool.Info            as InfoTool
import qualified HaskellFlows.Tool.Lab             as LabTool
import qualified HaskellFlows.Tool.Lint            as LintTool
import qualified HaskellFlows.Tool.Load            as Load
import qualified HaskellFlows.Tool.Modules         as ModulesTool
import qualified HaskellFlows.Tool.Perf            as PerfTool
import qualified HaskellFlows.Tool.Project         as ProjectTool
import qualified HaskellFlows.Tool.PropertyStore   as PropertyStoreTool
import qualified HaskellFlows.Tool.QuickCheck      as QcTool
import qualified HaskellFlows.Tool.Refactor        as RefactorTool
import qualified HaskellFlows.Tool.Scratch         as ScratchTool
import qualified HaskellFlows.Tool.Suggest         as SuggestTool
import qualified HaskellFlows.Tool.Toolchain       as ToolchainTool
import qualified HaskellFlows.Tool.Type            as TypeTool
import qualified HaskellFlows.Tool.Witness         as WitnessTool
import qualified HaskellFlows.Tool.Workflow        as WorkflowTool

------------------------------------------------------------------------
-- ToolSpec
------------------------------------------------------------------------

-- | All metadata for one tool, bundled in a single record.
--
-- === 'tsVersion' note
-- 'tsVersion' matches 'toolVersion' from "HaskellFlows.Mcp.ToolName"
-- by construction (see 'registry').  'toolVersion' stays in that
-- module because "HaskellFlows.Mcp.Protocol" imports it, and Protocol
-- is upstream of Registry in the module graph.  A test in
-- @Spec.RegistryUnit@ verifies they're always equal.
data ToolSpec = ToolSpec
  { tsName       :: ToolName
  , tsCategory   :: ToolCategory
  , tsVersion    :: Text
  , tsDescriptor :: ToolDescriptor
  , tsBudget     :: Maybe ToolBudget
  , tsHandler    :: ToolHandler
  }

------------------------------------------------------------------------
-- Registry
------------------------------------------------------------------------

-- | The canonical registry — one 'ToolSpec' per tool.
-- Order matches the current @allToolDescriptors@ list so diffs are
-- minimal; the derived projections re-sort as needed.
registry :: [ToolSpec]
registry =
  [ ToolSpec GhcLoad          CatPrimitive     (toolVersion GhcLoad)
      Load.descriptor
      (Just (ToolBudget 300  800  (Just 6000) "warm path; first call boots cabal v2-repl (cold-start: ~5s)"))
      Load.handle

  , ToolSpec GhcType          CatPrimitive     (toolVersion GhcType)
      TypeTool.descriptor
      (Just (ToolBudget  50  200   Nothing     "cached GHCi env; near-zero marginal cost"))
      TypeTool.handle

  , ToolSpec GhcInfo          CatPrimitive     (toolVersion GhcInfo)
      InfoTool.descriptor
      (Just (ToolBudget 100  300   Nothing     "cached GHCi env; :i lookup only"))
      InfoTool.handle

  , ToolSpec GhcEval          CatPrimitive     (toolVersion GhcEval)
      EvalTool.descriptor
      (Just (ToolBudget 100  500   Nothing     "cached GHCi env; simple expression eval"))
      EvalTool.handle

  , ToolSpec GhcQuickCheck    CatPrimitive     (toolVersion GhcQuickCheck)
      QcTool.descriptor
      (Just (ToolBudget 500 1500   Nothing     "100 QC runs + property persist; cabal-repl harness"))
      QcTool.handle

  , ToolSpec GhcHole          CatPrimitive     (toolVersion GhcHole)
      HoleTool.descriptor
      (Just (ToolBudget 200  600   Nothing     "typed-hole query over loaded module"))
      HoleTool.handle

  , ToolSpec GhcArbitrary     CatPrimitive     (toolVersion GhcArbitrary)
      ArbitraryTool.descriptor
      (Just (ToolBudget 200  500   Nothing     "template generation from :i output; pure"))
      ArbitraryTool.handle

  , ToolSpec HoogleSearch     CatPrimitive     (toolVersion HoogleSearch)
      HoogleTool.descriptor
      (Just (ToolBudget 200 1000   Nothing     "hoogle subprocess; includes fork+exec overhead"))
      HoogleTool.handle

  , ToolSpec GhcWorkflow      CatControlPlane  (toolVersion GhcWorkflow)
      WorkflowTool.descriptor
      (Just (ToolBudget  50  200   Nothing     "inventory scan; no GHCi interaction"))
      WorkflowTool.handle

  , ToolSpec GhcCheckModule   CatGate          (toolVersion GhcCheckModule)
      CheckModuleTool.descriptor
      (Just (ToolBudget 500 1500   Nothing     "strict load + warning gate + property replay"))
      CheckModuleTool.handle

  , ToolSpec GhcCoverage      CatComposite     (toolVersion GhcCoverage)
      CoverageTool.descriptor
      (Just (ToolBudget 5000 10000 Nothing     "full test rebuild with HPC instrumentation; subprocess-heavy"))
      CoverageTool.handle

  , ToolSpec GhcComplete      CatPrimitive     (toolVersion GhcComplete)
      CompleteTool.descriptor
      (Just (ToolBudget  50  200   Nothing     ":complete repl; cached env"))
      CompleteTool.handle

  , ToolSpec GhcFormat        CatPrimitive     (toolVersion GhcFormat)
      FormatTool.descriptor
      (Just (ToolBudget 300 1000   Nothing     "fourmolu/ormolu subprocess; per-file read+write I/O"))
      FormatTool.handle

  , ToolSpec GhcGate          CatComposite     (toolVersion GhcGate)
      GateTool.descriptor
      (Just (ToolBudget 8000 15000 Nothing     "cabal test + cabal build; scales with project size"))
      GateTool.handle

  , ToolSpec GhcDeps          CatPrimitive     (toolVersion GhcDeps)
      DepsTool.descriptor
      (Just (ToolBudget 1500 3000  Nothing     "cabal solver invocation; version-constraint resolution"))
      DepsTool.handle

  , ToolSpec GhcDoc           CatPrimitive     (toolVersion GhcDoc)
      DocTool.descriptor
      (Just (ToolBudget 100  300   Nothing     ":doc lookup; cached env + optional haddock data"))
      DocTool.handle

  , ToolSpec GhcGoto          CatPrimitive     (toolVersion GhcGoto)
      GotoTool.descriptor
      (Just (ToolBudget  50  200   Nothing     "parse Defined-at marker; pure text scan"))
      GotoTool.handle

  , ToolSpec GhcRefactor      CatPrimitive     (toolVersion GhcRefactor)
      RefactorTool.descriptor
      (Just (ToolBudget 1500 4000  Nothing
        "rename/extract/move_symbol + snapshot-and-compile-verify roundtrip; \
        \#94 Phase C bumped the budget when ghc_move was merged in \
        \(multi-file moves dominate the upper bound)"))
      RefactorTool.handle

  , ToolSpec GhcLab           CatComposite     (toolVersion GhcLab)
      LabTool.descriptor
      (Just (ToolBudget 5000 15000 Nothing     "per-binding suggest + QC across whole module; scales with module size"))
      LabTool.handle

  , ToolSpec GhcExplainError  CatPrimitive     (toolVersion GhcExplainError)
      ExplainErrorTool.descriptor
      (Just (ToolBudget 200  500   Nothing     "diagnostic evidence package + optional patch verify roundtrip"))
      ExplainErrorTool.handle

  , ToolSpec GhcPerf          CatPrimitive     (toolVersion GhcPerf)
      PerfTool.descriptor
      (Just (ToolBudget 3000 8000  Nothing     "expression eval x30 samples via cabal-repl harness"))
      PerfTool.handle

  , ToolSpec GhcWitness       CatPrimitive     (toolVersion GhcWitness)
      WitnessTool.descriptor
      (Just (ToolBudget 4000 10000 Nothing     "property eval x1000 with distribution labelling; cabal-repl harness"))
      WitnessTool.handle

  , ToolSpec GhcBatch         CatComposite     (toolVersion GhcBatch)
      BatchTool.descriptor
      (Just (ToolBudget 500 2000   Nothing     "per-child average; actual budget = sum of included tools"))
      BatchTool.handle

  , ToolSpec GhcLint          CatGate          (toolVersion GhcLint)
      LintTool.descriptor
      (Just (ToolBudget 2500 5000  Nothing     "hlint subprocess; recursive project scan"))
      LintTool.handle

  , ToolSpec GhcToolchain     CatControlPlane  (toolVersion GhcToolchain)
      ToolchainTool.descriptor
      (Just (ToolBudget 200  500   Nothing
        "#94 Phase C: action-discriminated successor to \
        \ghc_toolchain_status + ghc_toolchain_warmup; budget covers \
        \binary-probe (status) and PATH warm-up (warmup)"))
      ToolchainTool.handle

  , ToolSpec GhcCheckProject  CatGate          (toolVersion GhcCheckProject)
      CheckProjectTool.descriptor
      (Just (ToolBudget 1500 4000  Nothing     "check_module over every exposed + other-module in .cabal"))
      CheckProjectTool.handle

  , ToolSpec GhcSuggest       CatPrimitive     (toolVersion GhcSuggest)
      SuggestTool.descriptor
      (Just (ToolBudget 100  400   Nothing     "signature-driven property proposal; pure computation"))
      SuggestTool.handle

  , ToolSpec GhcAddImport     CatPrimitive     (toolVersion GhcAddImport)
      AddImportTool.descriptor
      (Just (ToolBudget 200  800   Nothing     "AST-free import line injection; optional hoogle subprocess"))
      AddImportTool.handle

  , ToolSpec GhcApplyExports  CatPrimitive     (toolVersion GhcApplyExports)
      ApplyExportsTool.descriptor
      (Just (ToolBudget 300  800   Nothing     "export list insertion + compile-verify roundtrip"))
      ApplyExportsTool.handle

  , ToolSpec GhcFixWarning    CatPrimitive     (toolVersion GhcFixWarning)
      FixWarningTool.descriptor
      (Just (ToolBudget 500 1500   Nothing     "warning-driven text rewrite + compile-verify roundtrip"))
      FixWarningTool.handle

  , ToolSpec GhcImports       CatPrimitive     (toolVersion GhcImports)
      ImportsTool.descriptor
      (Just (ToolBudget  50  200   Nothing     "list live GHCi imports; cached env query"))
      ImportsTool.handle

  , ToolSpec GhcBrowse        CatPrimitive     (toolVersion GhcBrowse)
      BrowseTool.descriptor
      (Just (ToolBudget 100  300   Nothing     "module member listing; cached env query"))
      BrowseTool.handle

  , ToolSpec GhcModules       CatPrimitive     (toolVersion GhcModules)
      ModulesTool.descriptor
      (Just (ToolBudget 100  300   Nothing
        "#94 Phase B: action-discriminated successor to \
        \ghc_add_modules / ghc_remove_modules; budgets match the \
        \underlying handlers"))
      ModulesTool.handle

  , ToolSpec GhcProject       CatPrimitive     (toolVersion GhcProject)
      ProjectTool.descriptor
      (Just (ToolBudget 200  500   Nothing
        "#94 Phase C step 5: action-discriminated successor to \
        \ghc_create_project + ghc_switch_project + \
        \ghc_validate_cabal + ghc_bootstrap"))
      ProjectTool.handle

  , ToolSpec GhcPropertyStore CatPrimitive     (toolVersion GhcPropertyStore)
      PropertyStoreTool.descriptor
      (Just (ToolBudget 1000 3000  Nothing
        "#94 Phase C step 6: action-discriminated successor to \
        \ghc_property_lifecycle + ghc_regression + \
        \ghc_quickcheck_export + ghc_property_audit"))
      PropertyStoreTool.handle

  , ToolSpec GhcScratch       CatPrimitive     (toolVersion GhcScratch)
      ScratchTool.descriptor
      (Just (ToolBudget 100  300   Nothing
        "#253: persistent LLM code canvas. Phase 1 ships data-bound \
        \actions only (write / list / show / clear) — pure JSON I/O \
        \through the scratchpad store"))
      ScratchTool.handle
  ]

-- | Fast lookup by 'ToolName'.
byName :: Map ToolName ToolSpec
byName = Map.fromList [ (tsName s, s) | s <- registry ]

------------------------------------------------------------------------
-- Derived projections
------------------------------------------------------------------------

-- | Tool category for dispatch and @tools/list@ grouping.
-- Replaces the @toolCategory@ case expression in
-- "HaskellFlows.Mcp.ToolName"; callers should import from here.
toolCategory :: ToolName -> ToolCategory
toolCategory tn = tsCategory (byName Map.! tn)

-- | Ordered descriptor list for @tools/list@ responses.
-- Preserves the registry order (matches the former hard-coded list in
-- @Server.allToolDescriptors@).
allToolDescriptors :: [ToolDescriptor]
allToolDescriptors = map tsDescriptor registry

-- | Full latency budget table, derived from 'registry'.
-- Replaces @HaskellFlows.Bench.Budget.allBudgets@.
allBudgets :: BudgetTable
allBudgets = Map.fromList
  [ (tsName s, b)
  | s <- registry
  , Just b <- [tsBudget s]
  ]

-- | Pure dispatch table — look up the 'ToolHandler' for a 'ToolName'.
-- Replaces the @handlerFor@ case expression in
-- "HaskellFlows.Mcp.Server".
handlerFor :: ToolName -> ToolHandler
handlerFor tn = tsHandler (byName Map.! tn)
