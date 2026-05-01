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
    -- * Tool taxonomy (issue #94 Phase A)
  , ToolCategory (..)
  , toolCategoryText
  , toolCategory
    -- * Tool versioning (issue #99 Phase B)
  , toolVersion
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
  | GhcValidateCabal
  | GhcCheckProject
  | GhcSuggest
  | GhcSwitchProject
  | GhcAddImport
  | GhcApplyExports
  | GhcFixWarning
  | GhcImports
  | GhcBrowse
  | GhcBootstrap
  | GhcPropertyLifecycle
  | GhcMove
  | GhcLab
  | GhcExplainError
  | GhcPerf
  | GhcPropertyAudit
  | GhcWitness
  | GhcModules
  | GhcToolchain
    -- ^ #94 Phase C: action-discriminated 'toolchain' primitive
    -- (action: "status" \| "warmup"). Replaced the per-verb
    -- 'GhcToolchainStatus' + 'GhcToolchainWarmup' constructors
    -- outright (no deprecation period — single internal consumer).
    -- ^ #94 Phase B: action-discriminated 'modules' primitive
    -- (action: "add" \| "remove"). Replaced the per-verb
    -- 'GhcAddModules' + 'GhcRemoveModules' constructors outright —
    -- the project has a single internal consumer, so deprecation
    -- was unnecessary. The dispatcher in 'Tool.Modules' delegates
    -- to the same handlers the per-verb tools used; only the wire
    -- surface changed.
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
  GhcValidateCabal     -> "ghc_validate_cabal"
  GhcCheckProject      -> "ghc_check_project"
  GhcSuggest           -> "ghc_suggest"
  GhcSwitchProject     -> "ghc_switch_project"
  GhcAddImport         -> "ghc_add_import"
  GhcApplyExports      -> "ghc_apply_exports"
  GhcFixWarning        -> "ghc_fix_warning"
  GhcImports           -> "ghc_imports"
  GhcBrowse            -> "ghc_browse"
  GhcBootstrap         -> "ghc_bootstrap"
  GhcPropertyLifecycle -> "ghc_property_lifecycle"
  GhcToolchain         -> "ghc_toolchain"
  GhcMove              -> "ghc_move"
  GhcLab               -> "ghc_lab"
  GhcExplainError      -> "ghc_explain_error"
  GhcPerf              -> "ghc_perf"
  GhcPropertyAudit     -> "ghc_property_audit"
  GhcWitness           -> "ghc_witness"
  GhcModules           -> "ghc_modules"

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

------------------------------------------------------------------------
-- Tool taxonomy (issue #94 Phase A)
------------------------------------------------------------------------

-- | Four-way classification of every MCP tool (issue #94).
--
-- * 'CatPrimitive'    — atomic operation the agent can compose; no
--   other tool in this list provides the same capability.
-- * 'CatComposite'    — internally chains ≥2 primitives; exposed as a
--   single surface point for round-trip convenience.
-- * 'CatGate'         — zero-argument (or single-flavour) composite
--   that returns a binary green\/red decision; used as a pre-push hook.
-- * 'CatControlPlane' — talks *about* the MCP or toolchain, not about
--   Haskell source; used for orientation and recovery.
data ToolCategory
  = CatPrimitive
  | CatComposite
  | CatGate
  | CatControlPlane
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Render a 'ToolCategory' as a lowercase text label suitable for
-- JSON or log output.
toolCategoryText :: ToolCategory -> Text
toolCategoryText = \case
  CatPrimitive    -> "primitive"
  CatComposite    -> "composite"
  CatGate         -> "gate"
  CatControlPlane -> "control_plane"

-- | Classify every registered tool into the four-category taxonomy.
-- This is the authoritative mapping; 'docs/TOOL_TAXONOMY.md' is
-- generated from it.  Adding a new constructor to 'ToolName' without
-- adding an arm here is a compile error.
toolCategory :: ToolName -> ToolCategory
toolCategory = \case
  -- ── Primitives ──────────────────────────────────────────────────
  -- Read / inspect
  GhcLoad              -> CatPrimitive
  GhcType              -> CatPrimitive
  GhcInfo              -> CatPrimitive
  GhcEval              -> CatPrimitive
  GhcHole              -> CatPrimitive
  GhcComplete          -> CatPrimitive
  GhcGoto              -> CatPrimitive
  GhcBrowse            -> CatPrimitive
  GhcImports           -> CatPrimitive
  GhcDoc               -> CatPrimitive
  HoogleSearch         -> CatPrimitive
  -- Write / refactor
  GhcRefactor          -> CatPrimitive
  GhcMove              -> CatPrimitive   -- future: refactor action=move_symbol
  GhcFormat            -> CatPrimitive
  GhcApplyExports      -> CatPrimitive
  GhcFixWarning        -> CatPrimitive
  GhcAddImport         -> CatPrimitive
  GhcArbitrary         -> CatPrimitive
  -- Dependency + project management
  GhcDeps              -> CatPrimitive
  GhcModules           -> CatPrimitive   -- #94 Phase B: action-discriminated successor
  GhcCreateProject     -> CatPrimitive   -- future: project action=create
  GhcSwitchProject     -> CatPrimitive   -- future: project action=switch
  GhcValidateCabal     -> CatPrimitive   -- future: project action=validate
  GhcBootstrap         -> CatPrimitive   -- future: project action=bootstrap
  -- Property-first testing
  GhcQuickCheck        -> CatPrimitive
  GhcSuggest           -> CatPrimitive
  GhcPropertyLifecycle -> CatPrimitive   -- future: property_store action=list|drop
  GhcRegression        -> CatPrimitive   -- future: property_store action=run
  GhcQuickCheckExport  -> CatPrimitive   -- future: property_store action=export
  GhcPropertyAudit     -> CatPrimitive   -- future: property_store action=audit
  -- Phase-2 advanced
  GhcPerf              -> CatPrimitive
  GhcWitness           -> CatPrimitive
  GhcExplainError      -> CatPrimitive
  -- ── Composites ──────────────────────────────────────────────────
  GhcGate              -> CatComposite  -- regression + cabal-test + cabal-build
  GhcLab               -> CatComposite  -- browse + suggest + quickcheck per binding
  GhcCoverage          -> CatComposite  -- cabal-test --enable-coverage + HPC parse
  GhcBatch             -> CatComposite  -- N sequential tool calls
  -- ── Gates ───────────────────────────────────────────────────────
  GhcCheckModule       -> CatGate       -- per-file compile + warnings + holes + props
  GhcCheckProject      -> CatGate       -- whole-project compile + warnings + holes + props
  GhcLint              -> CatGate       -- hlint over the project
  -- ── Control-plane ───────────────────────────────────────────────
  GhcWorkflow          -> CatControlPlane
  GhcToolchain         -> CatControlPlane

------------------------------------------------------------------------
-- Tool versioning (issue #99 Phase B)
------------------------------------------------------------------------

-- | Per-tool semver string surfaced in @tools/list@ and in every
-- tool response's @meta.tool_version@. Independent of the binary
-- and protocol versions: a tool can bump its own version when its
-- input or output shape changes incompatibly, even within the same
-- binary release.
--
-- Bump rules (per issue #99):
--
-- * MAJOR — tool's input or output shape changes incompatibly.
-- * MINOR — new args or output fields added (additive).
-- * PATCH — bug fix within the existing shape.
--
-- All tools start at \"1.0.0\" at this issue's landing; phase-2
-- features that *expanded* a response (\#93's @ghc_perf@ /
-- @ghc_witness@) were additive, so they remain at MAJOR=1. The
-- next bump will be tied to a real shape change and recorded in
-- @CHANGELOG.md@ at issue #99 Phase D landing.
--
-- Adding a constructor to 'ToolName' without an arm here is a
-- compile error — exhaustive case is the regression net.
toolVersion :: ToolName -> Text
toolVersion = \case
  -- ── Read / inspect ──────────────────────────────────────────────
  GhcLoad              -> "1.0.0"
  GhcType              -> "1.0.0"
  GhcInfo              -> "1.0.0"
  GhcEval              -> "1.0.0"
  GhcHole              -> "1.0.0"
  GhcComplete          -> "1.0.0"
  GhcGoto              -> "1.0.0"
  GhcBrowse            -> "1.0.0"
  GhcImports           -> "1.0.0"
  GhcDoc               -> "1.0.0"
  HoogleSearch         -> "1.0.0"
  -- ── Write / refactor ────────────────────────────────────────────
  GhcRefactor          -> "1.0.0"
  GhcMove              -> "1.0.0"
  GhcFormat            -> "1.0.0"
  GhcApplyExports      -> "1.0.0"
  GhcFixWarning        -> "1.0.0"
  GhcAddImport         -> "1.0.0"
  GhcArbitrary         -> "1.0.0"
  -- ── Dependency + project management ─────────────────────────────
  GhcDeps              -> "1.0.0"
  GhcCreateProject     -> "1.0.0"
  GhcSwitchProject     -> "1.0.0"
  GhcValidateCabal     -> "1.0.0"
  GhcBootstrap         -> "1.0.0"
  -- ── Property-first testing ──────────────────────────────────────
  GhcQuickCheck        -> "1.0.0"
  GhcSuggest           -> "1.0.0"
  GhcPropertyLifecycle -> "1.0.0"
  GhcRegression        -> "1.0.0"
  GhcQuickCheckExport  -> "1.0.0"
  GhcPropertyAudit     -> "1.0.0"
  -- ── Phase-2 advanced ────────────────────────────────────────────
  GhcPerf              -> "1.0.0"
  GhcWitness           -> "1.0.0"
  GhcExplainError      -> "1.0.0"
  -- ── Composites ──────────────────────────────────────────────────
  GhcGate              -> "1.0.0"
  GhcLab               -> "1.0.0"
  GhcCoverage          -> "1.0.0"
  GhcBatch             -> "1.0.0"
  -- ── Gates ───────────────────────────────────────────────────────
  GhcCheckModule       -> "1.0.0"
  GhcCheckProject      -> "1.0.0"
  GhcLint              -> "1.0.0"
  -- ── Control-plane ───────────────────────────────────────────────
  GhcWorkflow          -> "1.0.0"
  GhcToolchain         -> "1.0.0"
  -- ── #94 Phase B: action-discriminated successors ────────────────
  GhcModules           -> "1.0.0"
