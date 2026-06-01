-- | Unit tests for logging audit-path, Bench.Budget parsing, Runner
-- warmup-discard, and nextStep Gate chain quality/length/golden dispatch.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.BudgetGate
  ( testLoggingAuditPathPresentWhenEnabled
  , testBudgetParsesCleanly
  , testBudgetNoZeroValues
  , testRunnerDiscardFirstSample
  , testNextStepGateDWhyQuality
  , testNextStepGateEChainLength
  , testNextStepGoldenDispatch
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import System.Environment (setEnv, unsetEnv)

import qualified HaskellFlows.Bench.Budget as Budget
import qualified HaskellFlows.Bench.Runner as Runner
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
testBudgetNoZeroValues :: IO Bool
testBudgetNoZeroValues = pure $ all okBudget allToolNames
  where
    okBudget t = case Budget.lookupBudget t of
      Nothing -> False
      Just b  ->
        Budget.tbP50Ms b > 0
        && Budget.tbP95Ms b > 0
        && Budget.tbP50Ms b <= Budget.tbP95Ms b

-- | 'Runner.discardFirst' removes exactly the first element.
-- Simulates discarding the cold-start sample before computing p50\/p95.
testRunnerDiscardFirstSample :: IO Bool
testRunnerDiscardFirstSample = pure $
  -- non-empty list: first element gone
  Runner.discardFirst ([4800, 310, 290] :: [Int]) == [310, 290]
  -- singleton: result is empty
  && null (Runner.discardFirst ([42] :: [Int]))
  -- empty list: still empty (no error)
  && null (Runner.discardFirst ([] :: [Int]))
  -- computeStats picks up warm samples correctly
  && let r = Runner.computeStats [5000, 100, 200, 300]
     in Runner.prSamples r == [100, 200, 300]
        && Runner.prP50 r == 200
        && Runner.prP95 r == 300

------------------------------------------------------------------------
-- Issue #95 Phase D — nextStep quality gate: why string + chain length
------------------------------------------------------------------------

-- | Collect every 'NextStep' hint the dispatch table can emit, across
-- all tools and the meaningful payload shapes that select alternate
-- dispatch branches.  Used by Gate D and Gate E property tests.
allDispatchedHints :: [NextStep]
allDispatchedHints = catMaybes $
  -- Default payload — covers the bulk of tools (most have a single
  -- dispatch branch that ignores payload content).
  [ suggestNext t True (object []) | t <- allToolNames ]
  ++
  -- Per-tool payload variants that select alternate branches:
  [ suggestNext GhcGate         True (object ["status" .= ("ok"     :: Text)])
  -- #94 Phase C: determinism (runs>=2) merged into quickcheck.
  , suggestNext GhcQuickCheck   True (object ["runs"   .= (3 :: Int), "status" .= ("ok" :: Text)])
  , suggestNext GhcPropertyStore True (object ["action" .= ("run"    :: Text)])
  , suggestNext GhcPropertyStore True (object ["action" .= ("list"   :: Text)])
  , suggestNext GhcPropertyStore True (object ["files_written" .= (["test/Spec.hs"] :: [Text])])
  , suggestNext GhcPropertyStore True (object ["findings" .= ([] :: [Value])])
  , suggestNext GhcDeps         True (object ["action" .= ("add"    :: Text)])
  , suggestNext GhcDeps         True (object ["action" .= ("remove" :: Text)])
  , suggestNext GhcAddImport    True (object ["count"  .= (3 :: Int)])
  , suggestNext GhcQuickCheck   True (object ["state"  .= ("passed" :: Text)])
  , suggestNext GhcQuickCheck   True (object ["state"  .= ("failed" :: Text)])
  -- GhcLoad: typed-hole path
  , suggestNext GhcLoad True
      (object ["warnings" .=
        [object ["message" .= ("typed hole: _ :: Int" :: Text)]]])
  -- GhcLoad: fixable-warning path (non-hole warning)
  , suggestNext GhcLoad True
      (object ["warnings" .=
        [object ["message" .= ("unused import" :: Text)]]])
  -- GhcProject(action=switch): empty directory (no cabal file → scaffold)
  , suggestNext GhcProject True (object ["scaffolded" .= False])
  -- GhcProject(action=validate): cabal errors present
  , suggestNext GhcProject True (object ["errors" .= (3 :: Int)])
  ]

-- | Gate D: every 'nsWhy' string must be at least 10 characters long
-- and must end with a period ".".  A short or unpunctuated 'why' string
-- is not actionable — it gives the agent too little context to act on.
testNextStepGateDWhyQuality :: IO Bool
testNextStepGateDWhyQuality = pure $
  all checkWhy allDispatchedHints
  where
    checkWhy ns =
      T.length (nsWhy ns) >= 10
      && T.isSuffixOf "." (T.strip (nsWhy ns))

-- | Gate E: every 'nsChain' list (when present) must contain at most 4
-- steps.  Chains longer than 4 steps are overwhelming — the agent should
-- batch large workflows rather than prescribe them up front.
testNextStepGateEChainLength :: IO Bool
testNextStepGateEChainLength = pure $
  all checkChain allDispatchedHints
  where
    checkChain ns = case nsChain ns of
      Nothing -> True
      Just c  -> length c <= 4

------------------------------------------------------------------------
-- Issue #95 Phase C — golden dispatch snapshot
------------------------------------------------------------------------

-- | Golden table: @(description, source tool, payload, expected next tool)@.
-- Captures the dispatch table's behaviour for every meaningful (tool, payload)
-- combination.  A diff against this table signals a deliberate suppression-rule
-- change and must be reviewed before landing.
type GoldenRow = (String, ToolName, Value, Maybe ToolName)

goldenDispatchTable :: [GoldenRow]
goldenDispatchTable =
  -- ── Default payload (object []) ──────────────────────────────────
  [ ("project(create default) → deps chain", GhcProject,         object [],    Just GhcDeps)
  , ("deps(no-action) → suppressed",     GhcDeps,               object [],    Nothing)
  , ("load(clean) → suggest",            GhcLoad,               object [],    Just GhcSuggest)
  , ("hole → scratch(write) chain",      GhcHole,               object [],    Just GhcScratch)
  , ("arbitrary → load",                 GhcArbitrary,          object [],    Just GhcLoad)
  , ("suggest → scratch(write) chain",   GhcSuggest,            object [],    Just GhcScratch)
  , ("quickcheck(no-state) → suppress",  GhcQuickCheck,         object [],    Nothing)
  , ("property_store(no-action) → suppress", GhcPropertyStore,  object [],    Nothing)
  , ("refactor → load",                  GhcRefactor,           object [],    Just GhcLoad)
  , ("check_module → check_project",     GhcCheckModule,        object [],    Just GhcCheckProject)
  , ("check_project → gate chain",       GhcCheckProject,       object [],    Just GhcGate)
  , ("toolchain status → workflow",     GhcToolchain,          object [ "action" .= ("status" :: T.Text) ], Just GhcWorkflow)
  , ("project(validate clean) → suppress", GhcProject,          object [ "errors" .= (0 :: Int) ], Nothing)
  , ("lint → suppress",                  GhcLint,               object [],    Nothing)
  , ("format → load",                    GhcFormat,             object [],    Just GhcLoad)
  , ("batch → suppress",                 GhcBatch,              object [],    Nothing)
  , ("gate(fail) → check_project",       GhcGate,               object [],    Just GhcCheckProject)
  , ("property_store(export) → gate",   GhcPropertyStore,      object [ "files_written" .= (["test/Spec.hs"] :: [Text]) ], Just GhcGate)
  , ("quickcheck(runs=3,fail) → quickcheck",  GhcQuickCheck,    object [ "runs" .= (3 :: Int), "success" .= False ],  Just GhcQuickCheck)
  , ("property_store(audit) → list",    GhcPropertyStore,      object [ "findings" .= ([] :: [Value]) ],   Just GhcPropertyStore)
  , ("perf → perf",                      GhcPerf,               object [],    Just GhcPerf)
  , ("explain_error → scratch(write) chain", GhcExplainError,   object [],    Just GhcScratch)
  -- PR-3: lab promoted to a chain whose primary is property_store(audit),
  -- followed by check_project. Catches contradictions before replay.
  , ("lab → property_store(audit) chain", GhcLab,               object [],    Just GhcPropertyStore)
  , ("deps explain → deps add",         GhcDeps,               object [ "action" .= ("explain" :: T.Text) ], Just GhcDeps)
  , ("witness → quickcheck",            GhcWitness,            object [],    Just GhcQuickCheck)
  , ("refactor move_symbol → check_project", GhcRefactor,        object [ "action" .= ("move_symbol" :: T.Text) ], Just GhcCheckProject)
  , ("add_import(0) → suppress",        GhcAddImport,          object [],    Nothing)
  , ("modules add → check_project",     GhcModules,            object [ "action" .= ("add" :: T.Text) ],    Just GhcCheckProject)
  , ("modules remove → check_project",  GhcModules,            object [ "action" .= ("remove" :: T.Text) ], Just GhcCheckProject)
  , ("apply_exports → load",            GhcApplyExports,       object [],    Just GhcLoad)
  , ("fix_warning → load",              GhcFixWarning,         object [],    Just GhcLoad)
  , ("browse → suggest",                GhcBrowse,             object [],    Just GhcSuggest)
  -- PR-3: imports listed → browse the most-used module to find a
  -- candidate binding for laws.
  , ("imports → browse",                GhcImports,            object [],    Just GhcBrowse)
  -- #94 Phase C step 6: property_lifecycle merged into property_store. The
  -- "(no-action) → suppress" row above already covers the default-payload path.
  , ("toolchain warmup → workflow",     GhcToolchain,          object [ "action" .= ("warmup" :: T.Text) ], Just GhcWorkflow)
  , ("project(bootstrap) → workflow",   GhcProject,            object [ "host" .= ("claude-code" :: T.Text) ], Just GhcWorkflow)
  , ("workflow → suppress",             GhcWorkflow,           object [],    Nothing)
  -- PR-3: type/info/goto/doc fire on bare success — exploratory tools
  -- now carry forward-chaining hints. The new contract.
  , ("type → suggest",                  GhcType,               object [],    Just GhcSuggest)
  , ("info → doc",                      GhcInfo,               object [],    Just GhcDoc)
  -- PR-3: eval suppresses on degraded status; bare success (no status)
  -- counts as degraded via the statusOk_ fallback → suppressed.
  , ("eval (no status) → suppress",     GhcEval,               object [],    Nothing)
  , ("goto → browse",                   GhcGoto,               object [],    Just GhcBrowse)
  , ("doc → browse",                    GhcDoc,                object [],    Just GhcBrowse)
  -- PR-3: count-gated suggestions suppress when count is absent or zero
  -- (no candidates to act on).
  , ("complete (no count) → suppress",  GhcComplete,           object [],    Nothing)
  , ("hoogle_search (no count) → suppress", HoogleSearch,      object [],    Nothing)
  -- PR-3: coverage suppresses on degraded status (same shape as eval).
  , ("coverage (no status) → suppress", GhcCoverage,           object [],    Nothing)
  , ("project(switch scaffolded) → workflow", GhcProject,    object [ "scaffolded" .= True ], Just GhcWorkflow)
  -- ── Variant payloads ─────────────────────────────────────────────
  , ("deps(add) → load",
        GhcDeps,
        object ["action" .= ("add" :: Text)],
        Just GhcLoad)
  , ("deps(remove) → load",
        GhcDeps,
        object ["action" .= ("remove" :: Text)],
        Just GhcLoad)
  , ("load(errors) → suppress",
        GhcLoad,
        object ["errors" .= [object [] :: Value]],
        Nothing)
  , ("load(typed-holes) → hole",
        GhcLoad,
        object ["warnings" .= [object ["message" .= ("typed hole: _ :: Int" :: Text)] :: Value]],
        Just GhcHole)
  , ("load(fixable-warning) → fix_warning",
        GhcLoad,
        object ["warnings" .= [object ["message" .= ("unused import" :: Text)] :: Value]],
        Just GhcFixWarning)
  , ("quickcheck(passed) → check_module",
        GhcQuickCheck,
        object ["state" .= ("passed" :: Text)],
        Just GhcCheckModule)
  , ("quickcheck(failed) → eval",
        GhcQuickCheck,
        object ["state" .= ("failed" :: Text)],
        Just GhcEval)
  , ("property_store(list) → property_store(run)",
        GhcPropertyStore,
        object ["action" .= ("list" :: Text)],
        Just GhcPropertyStore)
  , ("property_store(run) → check_project",
        GhcPropertyStore,
        object ["action" .= ("run" :: Text)],
        Just GhcCheckProject)
  , ("gate(pass) → coverage",
        GhcGate,
        object ["status" .= ("ok" :: Text)],
        Just GhcCoverage)
  , ("quickcheck(runs=3,pass) → property_store",
        GhcQuickCheck,
        object ["runs" .= (3 :: Int), "status" .= ("ok" :: Text)],
        Just GhcPropertyStore)
  -- #94 Phase C step 6: ghc_property_lifecycle merged into
  -- ghc_property_store(action=list); covered by the
  -- "property_store(list) → property_store(run)" row above.
  , ("add_import(count>0) → load",
        GhcAddImport,
        object ["count" .= (3 :: Int)],
        Just GhcLoad)
  , ("project(switch empty) → project(create)",
        GhcProject,
        object ["scaffolded" .= False],
        Just GhcProject)
  , ("project(validate errors>0) → deps",
        GhcProject,
        object ["errors" .= (5 :: Int)],
        Just GhcDeps)
  ]

-- | Golden snapshot test: verify the dispatch table emits the expected
-- next-tool for every @(tool, payload)@ pair in 'goldenDispatchTable'.
-- A failure here means a dispatch-table change altered the recommendation
-- for a named case — review the diff before merging.
testNextStepGoldenDispatch :: IO Bool
testNextStepGoldenDispatch = do
  let failures = [ desc
                 | (desc, t, payload, expected) <- goldenDispatchTable
                 , let actual = fmap nsTool (suggestNext t True payload)
                 , actual /= expected
                 ]
  mapM_ (\d -> putStrLn ("  GOLDEN MISMATCH: " ++ d)) failures
  pure (null failures)
