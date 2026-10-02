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

import qualified HaskellFlows.Tool.Check           as Check
import qualified HaskellFlows.Tool.Edit            as Edit
import qualified HaskellFlows.Tool.Inspect        as Inspect
import qualified HaskellFlows.Tool.ModuleMgmt     as ModuleMgmt
import qualified HaskellFlows.Tool.Property       as Property
import qualified HaskellFlows.Tool.Session        as Session
import qualified HaskellFlows.Tool.Batch           as BatchTool
import qualified HaskellFlows.Tool.Deps            as DepsTool
import qualified HaskellFlows.Tool.Eval            as EvalTool
import qualified HaskellFlows.Tool.ExplainError    as ExplainErrorTool
import qualified HaskellFlows.Tool.Gate            as GateTool
import qualified HaskellFlows.Tool.Hoogle          as HoogleTool
import qualified HaskellFlows.Tool.Load            as Load
import qualified HaskellFlows.Tool.Project         as ProjectTool
import qualified HaskellFlows.Tool.Suggest         as SuggestTool

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
  [ ToolSpec GhcCheck         CatGate          (toolVersion GhcCheck)
      Check.descriptor
      (Just (ToolBudget 500 3000 Nothing "wave-2b composite: load/module/project/lint gates"))
      Check.handle

  , ToolSpec GhcEval          CatPrimitive     (toolVersion GhcEval)
      EvalTool.descriptor
      (Just (ToolBudget 100  500   Nothing     "cached GHCi env; simple expression eval"))
      EvalTool.handle

  , ToolSpec GhcProperty      CatComposite     (toolVersion GhcProperty)
      Property.descriptor
      (Just (ToolBudget 200 5000 Nothing "wave-2b composite: QC + store + arbitrary"))
      Property.handle

  , ToolSpec GhcSession       CatControlPlane  (toolVersion GhcSession)
      Session.descriptor
      (Just (ToolBudget 50 500 Nothing "wave-2b composite: workflow/toolchain/imports"))
      Session.handle

  , ToolSpec GhcEdit          CatComposite     (toolVersion GhcEdit)
      Edit.descriptor
      (Just (ToolBudget 200 2000 Nothing "wave-2b composite: refactor/imports/exports/fix/format"))
      Edit.handle

  , ToolSpec GhcModule        CatComposite     (toolVersion GhcModule)
      ModuleMgmt.descriptor
      (Just (ToolBudget 100 600 Nothing "wave-2b composite: cabal registration + scratchpad"))
      ModuleMgmt.handle

  , ToolSpec GhcInspect       CatPrimitive     (toolVersion GhcInspect)
      Inspect.descriptor
      (Just (ToolBudget 50 300 Nothing "wave-2b composite: read-only introspection"))
      Inspect.handle

  , ToolSpec GhcGate          CatComposite     (toolVersion GhcGate)
      GateTool.descriptor
      (Just (ToolBudget 8000 15000 Nothing     "cabal test + cabal build; scales with project size"))
      GateTool.handle

  , ToolSpec GhcDeps          CatPrimitive     (toolVersion GhcDeps)
      DepsTool.descriptor
      (Just (ToolBudget 1500 3000  Nothing     "cabal solver invocation; version-constraint resolution"))
      DepsTool.handle


  , ToolSpec GhcExplainError  CatPrimitive     (toolVersion GhcExplainError)
      ExplainErrorTool.descriptor
      (Just (ToolBudget 200  500   Nothing     "diagnostic evidence package + optional patch verify roundtrip"))
      ExplainErrorTool.handle

  , ToolSpec GhcBatch         CatComposite     (toolVersion GhcBatch)
      BatchTool.descriptor
      (Just (ToolBudget 500 2000   Nothing     "per-child average; actual budget = sum of included tools"))
      BatchTool.handle

  , ToolSpec GhcSuggest       CatPrimitive     (toolVersion GhcSuggest)
      SuggestTool.descriptor
      (Just (ToolBudget 100  400   Nothing     "signature-driven property proposal; pure computation"))
      SuggestTool.handle


  , ToolSpec GhcProject       CatPrimitive     (toolVersion GhcProject)
      ProjectTool.descriptor
      (Just (ToolBudget 200  500   Nothing
        "#94 Phase C step 5: action-discriminated successor to \
        \ghc_create_project + ghc_switch_project + \
        \ghc_validate_cabal + ghc_bootstrap"))
      ProjectTool.handle
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
