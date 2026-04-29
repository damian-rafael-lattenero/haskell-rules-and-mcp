-- | Reified tool names (issue #44).
--
-- Before this module, the 39 tool names exposed by the MCP server were
-- carried as raw 'Text' literals: 645 sites across the dispatcher,
-- @nextStep@ table, workflow state, guidance + situation table, the
-- E2E client, and every scenario. A single rename round (e.g.
-- @ghci_X -> ghc_X@ on 2026-04-24) touched 111 files and 1549 lines
-- because the compiler had no anchor to chase.
--
-- 'ToolName' is the anchor. Every constructor lists exactly once in
-- 'toolNameText'; a future rename is a one-line edit to a single arm,
-- and the @-Wincomplete-patterns@ warning surfaces every dispatch
-- arm that has not yet been adapted. The wire format (JSON-RPC tool
-- name) is unchanged — 'toolNameText' produces the same strings the
-- previous code emitted, so clients see no difference.
--
-- Adding a new tool is now:
--
--   1. Add a constructor to 'ToolName'.
--   2. Add an arm to 'toolNameText' (compiler complains until you do).
--   3. Add an arm to 'HaskellFlows.Mcp.Server.dispatchTool' (compiler
--      complains until you do).
--
-- Forgetting any one of those is a compile error, not a silent
-- runtime "Unknown tool" reply.
module HaskellFlows.Mcp.ToolName
  ( ToolName (..)
  , toolNameText
  , parseToolName
  , allToolNames
  , allToolNameTexts
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | The reified tool registry. Constructors are listed in the same
-- order as the previous 'Text' list in 'allToolDescriptors' so the
-- @tools/list@ wire output is unchanged.
data ToolName
  = GhcLoad
  | GhcType
  | GhcInfo
  | GhcEval
  | GhcQuickCheck
  | GhcHole
  | GhcArbitrary
  | HoogleSearch
  | GhcWorkflow
  | GhcRegression
  | GhcCheckModule
  | GhcCoverage
  | GhcComplete
  | GhcFormat
  | GhcGate
  | GhcQuickCheckExport
  | GhcDeps
  | GhcCreateProject
  | GhcDoc
  | GhcGoto
  | GhcRefactor
  | GhcBatch
  | GhcLint
  | GhcToolchainStatus
  | GhcValidateCabal
  | GhcCheckProject
  | GhcSuggest
  | GhcSwitchProject
  | GhcAddImport
  | GhcAddModules
  | GhcApplyExports
  | GhcFixWarning
  | GhcImports
  | GhcBrowse
  | GhcDeterminism
  | GhcRemoveModules
  | GhcBootstrap
  | GhcPropertyLifecycle
  | GhcToolchainWarmup
  | GhcMove
  | GhcDepsExplain
  | GhcLab
  | GhcExplainError
  | GhcPerf
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Render a 'ToolName' as the wire-format string the MCP clients
-- already speak. This is the ONE place in the codebase where the
-- string form lives; every other site must go through this function.
toolNameText :: ToolName -> Text
toolNameText = \case
  GhcLoad              -> "ghc_load"
  GhcType              -> "ghc_type"
  GhcInfo              -> "ghc_info"
  GhcEval              -> "ghc_eval"
  GhcQuickCheck        -> "ghc_quickcheck"
  GhcHole              -> "ghc_hole"
  GhcArbitrary         -> "ghc_arbitrary"
  HoogleSearch         -> "hoogle_search"
  GhcWorkflow          -> "ghc_workflow"
  GhcRegression        -> "ghc_regression"
  GhcCheckModule       -> "ghc_check_module"
  GhcCoverage          -> "ghc_coverage"
  GhcComplete          -> "ghc_complete"
  GhcFormat            -> "ghc_format"
  GhcGate              -> "ghc_gate"
  GhcQuickCheckExport  -> "ghc_quickcheck_export"
  GhcDeps              -> "ghc_deps"
  GhcCreateProject     -> "ghc_create_project"
  GhcDoc               -> "ghc_doc"
  GhcGoto              -> "ghc_goto"
  GhcRefactor          -> "ghc_refactor"
  GhcBatch             -> "ghc_batch"
  GhcLint              -> "ghc_lint"
  GhcToolchainStatus   -> "ghc_toolchain_status"
  GhcValidateCabal     -> "ghc_validate_cabal"
  GhcCheckProject      -> "ghc_check_project"
  GhcSuggest           -> "ghc_suggest"
  GhcSwitchProject     -> "ghc_switch_project"
  GhcAddImport         -> "ghc_add_import"
  GhcAddModules        -> "ghc_add_modules"
  GhcApplyExports      -> "ghc_apply_exports"
  GhcFixWarning        -> "ghc_fix_warning"
  GhcImports           -> "ghc_imports"
  GhcBrowse            -> "ghc_browse"
  GhcDeterminism       -> "ghc_determinism"
  GhcRemoveModules     -> "ghc_remove_modules"
  GhcBootstrap         -> "ghc_bootstrap"
  GhcPropertyLifecycle -> "ghc_property_lifecycle"
  GhcToolchainWarmup   -> "ghc_toolchain_warmup"
  GhcMove              -> "ghc_move"
  GhcDepsExplain       -> "ghc_deps_explain"
  GhcLab               -> "ghc_lab"
  GhcExplainError      -> "ghc_explain_error"
  GhcPerf              -> "ghc_perf"

-- | Parse a wire-format tool name back to its constructor. Returns
-- 'Nothing' for any unknown string — used by the dispatcher to emit
-- a structured \"Unknown tool\" error rather than to silently match.
parseToolName :: Text -> Maybe ToolName
parseToolName = flip Map.lookup reverseMap
  where
    -- Built once at module load. 39 entries; cheap.
    reverseMap :: Map.Map Text ToolName
    reverseMap = Map.fromList [ (toolNameText t, t) | t <- allToolNames ]

-- | Every registered tool. Derived from 'Bounded'+'Enum' so adding
-- a constructor automatically adds it to @tools/list@. Forgetting
-- to register a new tool is impossible by construction.
allToolNames :: [ToolName]
allToolNames = [minBound .. maxBound]

-- | Convenience: the wire-format string list, used by callers that
-- need to compare against parsed JSON. Always in sync with
-- 'allToolNames'.
allToolNameTexts :: [Text]
allToolNameTexts = map toolNameText allToolNames
