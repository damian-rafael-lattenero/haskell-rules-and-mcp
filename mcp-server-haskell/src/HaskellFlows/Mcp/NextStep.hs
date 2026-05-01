-- | @nextStep@ — structured "what to do next" hint injected
-- into every successful tool response.
--
-- The MCP protocol already carries tool descriptors (static,
-- 'tools/list') and a session-level 'instructions' field (one-shot,
-- 'initialize'). What those do not tell the agent is which tool to
-- reach for *after* the current one succeeded. That decision was
-- implicit — the agent had to re-read the descriptors and infer a
-- chain. F-14 from the Phase 11d/e dogfood surfaced the gap: even
-- with F-13's richer 'instructions', a fresh agent burned several
-- turns on "ok, I created a project, now what?" questions that a
-- per-response hint would have closed in one round-trip.
--
-- This module provides a tiny decision table: given a tool name + a
-- success flag + the tool's JSON payload, it returns an optional
-- 'NextStep' that the server layer injects into the outgoing
-- payload. The agent sees a structured @nextStep@ alongside the
-- tool's data:
--
-- > {
-- >   "files_written": [ … ],
-- >   "success": true,
-- >   "nextStep": {
-- >     "tool": "ghc_deps",
-- >     "why":  "scaffold only has `base`; add the deps you need before wiring up modules.",
-- >     "example": { "action": "add", "package": "QuickCheck", "stanza": "test-suite" }
-- >   }
-- > }
--
-- The hint is informational — it never executes anything, never
-- leaks secrets (only tool names + canonical example args, all
-- internal). The agent is free to ignore it.
module HaskellFlows.Mcp.NextStep
  ( NextStep (..)
  , ChainStep (..)
  , suggestNext
  , injectNextStep
    -- * Issue #95 Phase A: suppression rule API
  , RecommendCtx (..)
  , suppressIf
  , suppressOnZero
  , suppressOnDegraded
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

-- | Structured next-step hint. 'nsExample' is an optional sample
-- arguments object the agent can use verbatim. 'nsChain' (BUG-22)
-- is an optional multi-step plan — the agent can execute it as a
-- single @ghc_batch@ call, or walk the steps one by one. The
-- primary @tool@ + @why@ are always the first step's intent, so
-- an agent that ignores @chain@ still gets the right first call.
--
-- 'nsTool' / 'csTool' carry the 'ToolName' ADT (issue #44). The
-- on-the-wire string is produced by 'toolNameText' inside the
-- 'ToJSON' instances below — so renaming a tool's wire string is
-- a single-site edit in 'HaskellFlows.Mcp.ToolName' that ripples
-- here automatically.
data NextStep = NextStep
  { nsTool    :: !ToolName
  , nsWhy     :: !Text
  , nsExample :: !(Maybe Value)
  , nsChain   :: !(Maybe [ChainStep])
  }
  deriving stock (Eq, Show)

-- | One step in a multi-step plan. The fields mirror the shape
-- @ghc_batch@ accepts (@{tool, args}@) so the agent can pass
-- @chain@ straight to @ghc_batch(actions=chain)@.
data ChainStep = ChainStep
  { csTool :: !ToolName
  , csArgs :: !Value
  }
  deriving stock (Eq, Show)

instance ToJSON ChainStep where
  toJSON cs = object
    [ "tool" .= toolNameText (csTool cs)
    , "args" .= csArgs cs
    ]

instance ToJSON NextStep where
  toJSON ns =
    object $
      [ "tool" .= toolNameText (nsTool ns)
      , "why"  .= nsWhy ns
      ]
      <> maybe [] (\e -> ["example" .= e]) (nsExample ns)
      <> maybe [] (\c -> ["chain"   .= c]) (nsChain ns)

--------------------------------------------------------------------------------
-- Issue #95 Phase A: suppression rule API
--------------------------------------------------------------------------------

-- | Per-call context the suppression rules inspect. Built once from
-- the tool name, response status, and payload.
data RecommendCtx = RecommendCtx
  { rcTool    :: !ToolName
  , rcStatus  :: !Text    -- ^ wire-format status: "ok" | "partial" | …
  , rcPayload :: !Value
  }
  deriving stock (Show)

-- | Apply a predicate to a 'NextStep'; return 'Nothing' (suppressed)
-- when the predicate holds, otherwise 'Just' the original hint.
-- Compose with @(>>= suppressIf p)@ for multiple rules.
suppressIf :: (RecommendCtx -> Bool) -> RecommendCtx -> Maybe NextStep -> Maybe NextStep
suppressIf _rule _ctx Nothing   = Nothing
suppressIf rule  ctx  (Just ns) = if rule ctx then Nothing else Just ns

-- | Suppression rule #1: suppress when a *count* field in the payload
-- is zero. Used when the recommendation only makes sense when the
-- previous step found at least one candidate (e.g. 'GhcAddImport').
suppressOnZero :: Text -> RecommendCtx -> Bool
suppressOnZero field ctx = case intField field (rcPayload ctx) of
  Just n  -> n <= 0
  Nothing -> False

-- | Suppression rule #2: suppress forward-chaining suggestions when
-- the current response is degraded (@status ∉ {ok, partial}@).
-- Error states should speak for themselves without adding noise.
suppressOnDegraded :: RecommendCtx -> Bool
suppressOnDegraded ctx = rcStatus ctx `notElem` ["ok", "partial"]

--------------------------------------------------------------------------------
-- smart constructors
--------------------------------------------------------------------------------

-- | Shorthand: single-step hint, no chain.
simple :: ToolName -> Text -> Maybe Value -> NextStep
simple tool why ex = NextStep
  { nsTool    = tool
  , nsWhy     = why
  , nsExample = ex
  , nsChain   = Nothing
  }

-- | Multi-step hint: the first step is the primary suggestion;
-- 'chain' carries the full bundle the agent can batch via
-- @ghc_batch(actions=chain)@.
chained :: ToolName -> Text -> Maybe Value -> [ChainStep] -> NextStep
chained tool why ex chain = (simple tool why ex) { nsChain = Just chain }

-- | Build a chain step from (tool, args object).
step :: ToolName -> Value -> ChainStep
step tool args = ChainStep { csTool = tool, csArgs = args }

--------------------------------------------------------------------------------
-- decision table
--------------------------------------------------------------------------------

-- | Map a (toolName, wasSuccessful, payload) triple to the next
-- recommended tool. 'Nothing' means no strong suggestion — the
-- agent should fall back to 'ghc_workflow(action="help")' if
-- genuinely unsure.
suggestNext :: ToolName -> Bool -> Value -> Maybe NextStep
suggestNext toolName ok payload
  | not ok    = Nothing   -- errors speak for themselves; let the agent parse them
  | otherwise = dispatch toolName payload

-- The exhaustive case below makes adding a new 'ToolName'
-- constructor a compile error here until you've decided whether it
-- has a follow-up hint or not — i.e. you can't accidentally ship a
-- new tool whose successes silently miss 'nextStep' (the original
-- rationale for issue #44).
dispatch :: ToolName -> Value -> Maybe NextStep
dispatch name payload = case name of

  -- #94 Phase C step 5: ghc_project — action-discriminated, so the
  -- nextStep depends on which action just ran. We discriminate by
  -- payload shape because the args aren't in scope here:
  --   * 'scaffolded' field present → switch
  --   * 'host' field present       → bootstrap
  --   * 'errors'/'warnings' fields → validate
  --   * otherwise (cabal_path/...) → create
  --
  -- The hints below are byte-for-byte ports of the per-tool nextStep
  -- arms that lived here pre-consolidation; only the dispatch
  -- discriminator changed.
  GhcProject -> projectNext payload

  -- After editing deps, reload to pick up the new package graph.
  GhcDeps -> case depsAction payload of
    Just "add"     -> Just loadAfterDepsEdit
    Just "remove"  -> Just loadAfterDepsEdit
    -- #94 Phase C: explain hands the agent the conflicting package's
    -- name; the canonical follow-up is bumping that constraint via
    -- another ghc_deps call (action=add or remove).
    Just "explain" -> Just (simple GhcDeps
      "The conflict's root_cause names the package whose pin is forcing \
      \the solver into the dead end. Either bump that constraint via \
      \ghc_deps action=add (with a wider version range) or remove it via \
      \ghc_deps action=remove."
      (Just (object
          [ "action"  .= ("add" :: Text)
          , "package" .= ("<conflicting-pkg>" :: Text)
          , "version" .= (">= <wider-range>" :: Text)
          ])))
    _              -> Nothing
    where
      loadAfterDepsEdit = simple GhcLoad
        "Dependency set changed. Reload your entry module so the \
        \GHCi session sees the new package graph."
        (Just (object
            [ "module_path" .= ("<your entry module>" :: Text) ]))

  -- Module loaded: dispatch on error + warning shape.
  --   * errors present    → Nothing (errors speak for themselves)
  --   * only typed holes  → ghc_hole (types + in-scope fits)
  --   * other warnings    → ghc_fix_warning (it auto-patches
  --                          unused-imports, type-defaults,
  --                          incomplete-uni-patterns, redundant-
  --                          constraints; rich error-category
  --                          coverage that the agent would
  --                          otherwise hand-fix)
  --   * clean compile     → ghc_suggest for QuickCheck laws
  GhcLoad -> case (loadWarningKind payload, loadHasErrors payload) of
    (_,            True) -> Nothing
    (LWTypedHoles, False) -> Just (simple GhcHole
      "The load reported typed-hole warnings — 'ghc_hole' gives \
      \you their expected types and in-scope fits in one call."
      Nothing)
    (LWFixable,    False) -> Just (simple GhcFixWarning
      "The load reported warnings the fix-warning tool can auto-\
      \patch (unused import, type-defaults, incomplete-uni-pattern, \
      \redundant constraint, …). Feed the first warning's \
      \file+line+hint in; it returns the rewritten file as a patch \
      \the agent reviews before writing."
      (Just (object
          [ "module_path" .= ("<same module you just loaded>" :: Text) ])))
    (LWNone,       False) -> Just (simple GhcSuggest
      "Module compiles clean. Ask 'ghc_suggest' for QuickCheck \
      \laws its type signatures imply; feed the High-confidence \
      \ones into 'ghc_quickcheck'."
      (Just (object
          [ "function_name" .= ("<pick one from the module>" :: Text) ])))

  -- Typed holes listed → implementation work, then reload.
  GhcHole -> Just (simple GhcLoad
    "Implement the holes using the fits listed above, then reload \
    \with diagnostics=true to confirm the types now line up."
    (Just (object
        [ "module_path" .= ("<same module you just inspected>" :: Text)
        , "diagnostics" .= True
        ])))

  -- Arbitrary template generated → paste + reload.
  GhcArbitrary -> Just (simple GhcLoad
    "Paste the instance into the module that declares the type, \
    \then reload to confirm it compiles."
    Nothing)

  -- Suggestions → run them via quickcheck, pick highest confidence.
  GhcSuggest -> Just (simple GhcQuickCheck
    "Feed the highest-confidence suggestion into quickcheck. \
    \Passing properties auto-persist to .haskell-flows/properties.json \
    \for the next regression run."
    (Just (object
        [ "property"    .= ("<copy from suggestion.property>" :: Text)
        , "module_path" .= ("<module defining the function>" :: Text)
        ])))

  -- QuickCheck passed → keep chaining, or gate.
  -- #94 Phase C: ghc_quickcheck now also handles the determinism
  -- mode (runs >= 2). Multi-run responses carry a 'runs' field in
  -- the payload (the single-run path does not), so we use that as
  -- the discriminator and route to the legacy determinismNext logic.
  GhcQuickCheck
    | isDeterminismPayload payload -> Just (determinismNext payload)
    | otherwise -> case qcState payload of
        Just "passed" -> Just (simple GhcCheckModule
          "Law holds. Either run 'ghc_suggest' for the next candidate, \
          \or roll up into a per-module gate. For flakiness confidence, \
          \re-run ghc_quickcheck with runs>=3."
          (Just (object [ "module_path" .= ("<same module>" :: Text) ])))
        Just "failed" -> Just (simple GhcEval
          "Property failed. Evaluate the reported counter-example with \
          \'ghc_eval' to see intermediate values before editing."
          Nothing)
        _ -> Nothing

  -- Regression list → run the set.
  GhcRegression -> case regressionAction payload of
    Just "list" -> Just (simple GhcRegression
      "You now know the persisted set. Run it to confirm every \
      \property still holds after recent edits."
      (Just (object [ "action" .= ("run" :: Text) ])))
    Just "run"  -> Just (simple GhcCheckProject
      "All persisted properties re-played. Roll into the project-wide \
      \gate for pre-push readiness."
      Nothing)
    _ -> Nothing

  -- Refactor landed → verify compile + rerun regressions.
  -- #94 Phase C: 'move_symbol' (the merged ghc_move) is multi-file
  -- and benefits from a project-wide gate; 'rename_local' /
  -- 'extract_binding' are single-file and the per-module reload is
  -- the natural follow-up.
  GhcRefactor -> case envField "action" payload of
    Just (String "move_symbol") -> Just (simple GhcCheckProject
      "Move was applied AND the source target loaded clean. Run \
      \ghc_check_project for the whole-project gate so any unrewritten \
      \consumer (qualified import, hiding clause, Haddock ref) surfaces \
      \with file + line."
      Nothing)
    _ -> Just (simple GhcLoad
      "Refactor was snapshot-and-compile-verified, but a reload with \
      \diagnostics=true surfaces new holes or warnings in one shot."
      (Just (object
          [ "module_path" .= ("<same module>" :: Text)
          , "diagnostics" .= True
          ])))

  -- Per-module gate passed → project-wide gate.
  GhcCheckModule -> Just (simple GhcCheckProject
    "Module-complete. Run the project-wide gate to confirm every \
    \other module still compiles cleanly with your changes."
    Nothing)

  -- Project gate green → pre-push finalizer chain.
  GhcCheckProject -> Just (chained GhcGate
    "Project-wide gate is green. Run ghc_gate for the pre-push \
    \finalizer (regression + cabal test + cabal build in one call). \
    \Coverage is the optional follow-up."
    Nothing
    [ step GhcGate     (object [])
    , step GhcCoverage (object [])
    ])

  -- #94 Phase C: toolchain (status or warmup) — if everything green, go build.
  GhcToolchain -> Just (simple GhcWorkflow
    "With the toolchain confirmed, 'ghc_workflow(action=\"help\")' \
    \gives you the next action tailored to the session's current \
    \state (alive GHCi, loaded modules, etc)."
    (Just (object [ "action" .= ("help" :: Text) ])))

  -- Lint surface → interpret yourself; no one-shot fix.
  GhcLint -> Nothing

  -- Format → reload to confirm no behaviour change.
  GhcFormat -> Just (simple GhcLoad
    "Formatter rewrote the module. Reload to confirm it still \
    \compiles and no whitespace-sensitive construct broke."
    Nothing)

  -- Batch → no single next step (depends on what the batch did); let
  -- the agent look at the individual results.
  GhcBatch -> Nothing

  --------------------------------------------------------------------
  -- BUG-06: Phase 11f..11n tools — positive entries so the "every
  -- successful response carries nextStep" promise holds across the
  -- whole registry.
  --------------------------------------------------------------------

  -- Gate passed → green to push. On fail, drill in per module.
  GhcGate -> Just (gateNext payload)

  -- test/Spec.hs materialised → run ghc_gate to exercise it through
  -- cabal test (same semantics as CI without the MCP in the loop).
  GhcQuickCheckExport -> Just (simple GhcGate
    "test/Spec.hs is now materialised. Run ghc_gate to replay the \
    \persisted properties the same way cabal test will in CI — this \
    \is the regression check that catches a property breaking between \
    \export + push."
    Nothing)


  -- Issue #64: the auditor flagged contradictory pairs (or
  -- nothing). When findings exist, the canonical follow-up is
  -- ghc_property_lifecycle drop on the wrong property; when
  -- empty, the next step is the regular gate.
  GhcPropertyAudit -> Just (simple GhcPropertyLifecycle
    "Audit completed. If 'findings' is non-empty, decide which \
    \property reflects real intent and drop the other via \
    \ghc_property_lifecycle. If empty, the store is consistent — \
    \run ghc_check_project."
    Nothing)

  -- Issue #61 Phase 2: baseline persistence is live.
  -- If the caller set save_baseline=true the mean is now persisted;
  -- the canonical follow-up is a second run with compare_baseline=true
  -- to detect regressions. For first-time profiling, recommend saving.
  GhcPerf -> Just (simple GhcPerf
    "Phase 2: use save_baseline=true to persist this mean_ns, then \
    \compare_baseline=true on the next run to detect regressions \
    \(>10% slower triggers status='refused'). Run with a different \
    \implementation to compare wall-clock performance."
    (Just (object
        [ "expression"       .= ("<same expression>" :: Text)
        , "compare_baseline" .= True
        ])))

  -- Issue #59 Phase 2: verify_patch is live.
  -- The canonical flow: agent uses its LLM to propose a patch,
  -- feeds it as verify_patch, tool applies/recompiles/restores,
  -- and reports error_resolved. Then ghc_load to confirm.
  GhcExplainError -> Just (simple GhcExplainError
    "Phase 2: feed a candidate fix as verify_patch={line, old, new} \
    \to let the tool apply it, recompile, check error_resolved, then \
    \restore the file. Iterate until error_resolved=true, then \
    \ghc_load to confirm the final state."
    (Just (object
        [ "module_path"  .= ("<same module>" :: Text)
        , "verify_patch" .= object
            [ "line" .= (0 :: Int)
            , "old"  .= ("<old text>" :: Text)
            , "new"  .= ("<new text>" :: Text)
            ]
        ])))

  -- Issue #60: the audit just persisted a batch of properties;
  -- the natural follow-up is the project-level gate that
  -- replays the new + existing set under the post-audit
  -- regression store.
  GhcLab -> Just (simple GhcCheckProject
    "Module audit completed. Run ghc_check_project to confirm \
    \the regression-store delta replays cleanly under the whole \
    \project, then ghc_gate before push."
    Nothing)


  -- Issue #65 Phase 1: witness already emitted its own nextStep
  -- pointing back at ghc_quickcheck (re-run without instrumentation
  -- to confirm the pass/fail signal). The dispatcher hint here is
  -- a backstop — when the runtime payload carries no nextStep we
  -- still want to nudge the agent towards the canonical follow-up.
  GhcWitness -> Just (simple GhcQuickCheck
    "Witness reported a distribution and any biased buckets. Re-run \
    \the property with ghc_quickcheck (or tighten the Arbitrary \
    \instance) so the next pass/fail signal reflects an unbiased \
    \input space."
    Nothing)

  -- Issue #62: a successful move was already verified via the
  -- internal loadForTarget; the agent's next reasonable check is
  -- the project-level gate so any consumer the heuristic missed
  -- surfaces immediately.

  -- Issue #53: only nudge towards 'ghc_load' when ghc_add_import
  -- actually returned candidate imports. The legacy nextStep ran
  -- unconditionally, so a hoogle-missing or zero-hits response
  -- still claimed \"the import was added\" — a lie that wasted
  -- a follow-up round-trip.
  GhcAddImport -> case importCount payload of
    Just n | n > 0 -> Just (simple GhcLoad
      "Pick one of the candidate imports above and paste it at the \
      \top of your .hs file, then reload to confirm the \
      \\"not in scope\" error is gone."
      (Just (object [ "module_path" .= ("<same module>" :: Text) ])))
    _              -> Nothing

  -- #94 Phase B: action-discriminated successor.  The dispatcher
  -- cares about the post-condition (modules just changed), not which
  -- surface point produced it.  Always recommend a project-wide gate;
  -- both add and remove can dangle imports or break loaders.
  GhcModules -> Just (chained GhcCheckProject
    "Modules registry changed in the .cabal (add or remove). Run \
    \ghc_check_project to surface any compile errors the change \
    \introduced; chained ghc_load follows so the entry module is \
    \live in the GHCi session afterwards."
    Nothing
    [ step GhcCheckProject (object [])
    , step GhcLoad (object
        [ "module_path" .= ("<your entry module>" :: Text) ])
    ])

  -- Applied an export list — reload confirms nothing external broke.
  GhcApplyExports -> Just (simple GhcLoad
    "Module export list was rewritten. Reload to confirm the new \
    \export set still type-checks and every consumer can still \
    \see what it needs."
    (Just (object [ "module_path" .= ("<same module>" :: Text) ])))

  -- Fix-warning emitted a plan — apply it, then reload to confirm.
  GhcFixWarning -> Just (simple GhcLoad
    "The fix plan has been written to disk (apply=true) or returned \
    \as a diff (apply=false — inspect before applying). Reload to \
    \confirm the warning is gone and nothing downstream broke."
    (Just (object [ "module_path" .= ("<same module>" :: Text) ])))

  -- Browse listed bindings — pick one and suggest laws for it.
  GhcBrowse -> Just (simple GhcSuggest
    "You now have the full top-level surface of the module. Pick an \
    \interesting binding and ask ghc_suggest for QuickCheck laws \
    \its signature implies. Names that hint at optimisation \
    \(simplify / normalize / fold / ...) bump soundness rules to \
    \High confidence automatically."
    (Just (object [ "function_name" .= ("<one of the browsed names>" :: Text) ])))

  -- Imports list is a diagnostic aid — no forced next step.
  GhcImports -> Nothing

  -- Lifecycle mgmt of the property store — if action=list the
  -- next natural step is to prune or run; leave the choice open.
  GhcPropertyLifecycle -> case regressionAction payload of
    Just "list" -> Just (simple GhcRegression
      "Now that you can see the store, run the regression to \
      \confirm every entry still passes — flaky / broken properties \
      \should be pruned before the next push."
      (Just (object [ "action" .= ("run" :: Text) ])))
    _ -> Nothing

  -- Workflow meta — would loop if we suggested itself.
  GhcWorkflow -> Nothing

  -- Exploratory / terminal tools — no strong suggestion.
  GhcType     -> Nothing
  GhcInfo     -> Nothing
  GhcEval     -> Nothing
  GhcGoto     -> Nothing
  GhcDoc      -> Nothing
  GhcComplete -> Nothing
  HoogleSearch -> Nothing
  GhcCoverage -> Nothing

-- | #94 Phase C step 5: pick the right next-step based on which
-- 'ghc_project' action ran. We discriminate by payload shape:
--
--   * @scaffolded@ field present → @action=switch@ ran.
--   * @host@ field present       → @action=bootstrap@ ran.
--   * @errors@ field is an Int   → @action=validate@ ran.
--   * otherwise                  → @action=create@ ran (the response
--                                   has @cabal_path@ etc but no
--                                   single field is reliably
--                                   discriminative; we treat 'create'
--                                   as the catch-all).
projectNext :: Value -> Maybe NextStep
projectNext payload
  -- switch
  | Just (Bool False) <- envField "scaffolded" payload =
      Just (simple GhcProject
        "Switched to an empty directory. Scaffold a fresh cabal \
        \package here with 'ghc_project(action=create)' (library + \
        \test-suite stub) before any other tool has something \
        \to load."
        (Just (object
            [ "action" .= ("create" :: Text)
            , "name"   .= ("<pkg-name>" :: Text)
            ])))
  | Just _ <- envField "scaffolded" payload =
      Just (simple GhcWorkflow
        "Project root swapped. Ask 'ghc_workflow(status)' to \
        \orient yourself in the new project: phase classifier, \
        \tools active, and staleness check against the new .cabal."
        (Just (object [ "action" .= ("status" :: Text) ])))
  -- bootstrap
  | Just _ <- envField "host" payload =
      Just (simple GhcWorkflow
        "Host rules preview emitted. Re-run with write=true to persist \
        \them under .claude/ or .cursor/, then 'ghc_workflow(help)' for \
        \the next project-level step."
        (Just (object [ "action" .= ("help" :: Text) ])))
  -- validate (errors > 0)
  | Just n <- cabalErrors payload, n > 0 =
      Just (simple GhcDeps
        "The .cabal file has errors. Fix them via 'ghc_deps' rather \
        \than editing by hand — the post-edit invariant check catches \
        \shape bugs before they land."
        (Just (object [ "action" .= ("list" :: Text) ])))
  -- validate (clean) — suppress
  | Just _ <- envField "errors" payload = Nothing
  -- create — everything else
  | otherwise = Just (chained
      GhcDeps
      "Your scaffold has only `base`. Add the deps you need (QuickCheck \
      \for tests, runtime libraries for the library stanza) before \
      \wiring up modules. The attached chain is the canonical \
      \project-bootstrap plan — you can batch it via ghc_batch."
      (Just (object
          [ "action"  .= ("add" :: Text)
          , "package" .= ("QuickCheck" :: Text)
          , "version" .= (">= 2.14" :: Text)
          , "stanza"  .= ("test-suite" :: Text)
          ]))
      [ step GhcDeps (object
          [ "action"  .= ("add" :: Text)
          , "package" .= ("QuickCheck" :: Text)
          , "version" .= (">= 2.14" :: Text)
          , "stanza"  .= ("test-suite" :: Text) ])
      , step GhcModules (object
          [ "action"  .= ("add" :: Text)
          , "modules" .= (["<Module.Name>"] :: [Text]) ])
      , step GhcLoad (object
          [ "module_path" .= ("<path to your entry module>" :: Text) ])
      ])

-- | 'ghc_gate' payload carries per-step status. On green, push is
-- unblocked; on red, the agent should narrow down per module.
gateNext :: Value -> NextStep
gateNext payload
  | gatePassed payload = simple GhcCoverage
      "ghc_gate is green — regression + cabal test + cabal build \
      \all passed. Optional: run ghc_coverage for the HPC summary. \
      \Otherwise you're clear to git commit + push."
      Nothing
  | otherwise = simple GhcCheckProject
      "At least one gate step failed. Drop one level down into \
      \ghc_check_project to isolate the red module, then drill in \
      \with ghc_check_module + ghc_load(diagnostics=true)."
      Nothing

-- | #94 Phase C: discriminate ghc_quickcheck single-run vs multi-run
-- (determinism) responses. The Determinism handler emits a payload
-- with a top-level @runs@ field (the requested run count); the
-- single-run handler does not. We auto-drill the @result@ envelope
-- because tool payloads sit under @result.runs@ post-#90.
isDeterminismPayload :: Value -> Bool
isDeterminismPayload payload = case envField "runs" payload of
  Just _  -> True
  Nothing -> False

-- | 'ghc_determinism' payload has a top-level @success@ bool.
-- Stable → trust for regression; flaky → show the counter-example.
determinismNext :: Value -> NextStep
determinismNext payload
  | determinismPassed payload = simple GhcRegression
      "Property passed every run — safe to add to the regression \
      \set. 'ghc_regression(action=\"run\")' confirms none of \
      \the stored set regressed after your recent changes."
      (Just (object [ "action" .= ("run" :: Text) ]))
  | otherwise = simple GhcQuickCheck
      "Property was flaky (failed at least one run). Re-run \
      \ghc_quickcheck to get a counter-example you can evaluate \
      \with ghc_eval, then fix the underlying code."
      Nothing

--------------------------------------------------------------------------------
-- payload probes (small, hand-written, no lens-aeson dep)
--------------------------------------------------------------------------------

-- | Look up a field, auto-drilling through the @result@ envelope
-- when the field isn't at the top level (issue #90 Phase D).
--
-- Tool payloads moved under @result@ post-#90; this helper makes
-- the router see them transparently. Top-level keys
-- (@status@, @error@, @nextStep@) resolve directly because the
-- top-level lookup hits first.
envField :: Text -> Value -> Maybe Value
envField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just inner -> Just inner
  Nothing    -> case KeyMap.lookup (Key.fromText "result") o of
    Just (Object r) -> KeyMap.lookup (Key.fromText k) r
    _               -> Nothing
envField _ _ = Nothing

-- | Extract a string field from a JSON object payload. Auto-drills
-- through the post-#90 envelope. Returns 'Nothing' if the field is
-- missing or its value is not a string.
stringField :: Text -> Value -> Maybe Text
stringField k v = case envField k v of
  Just (String s) -> Just s
  _               -> Nothing

-- | Extract an integer field. Auto-drills through @result@.
intField :: Text -> Value -> Maybe Int
intField k v = case envField k v of
  Just (Number n) -> Just (round n)
  _               -> Nothing

-- | Issue #53: count of candidate imports in a 'ghc_add_import'
-- response. Drives the suppress-nextStep-on-zero-hits gate.
importCount :: Value -> Maybe Int
importCount = intField "count"

-- | Classify the 'warnings' field of a 'ghc_load' response.
-- Drives the fix_warning-vs-hole-vs-suggest fork in 'dispatch'.
data LoadWarningKind
  = LWNone          -- ^ no warnings
  | LWTypedHoles    -- ^ every warning is a typed-hole
  | LWFixable       -- ^ at least one warning is NOT a typed-hole,
                    --   and is fixable by ghc_fix_warning
  deriving (Eq, Show)

loadWarningKind :: Value -> LoadWarningKind
loadWarningKind v = case envField "warnings" v of
  Just (Array xs)
    | null xs                      -> LWNone
    | all isTypedHoleWarning xs    -> LWTypedHoles
    | otherwise                    -> LWFixable
  _                                -> LWNone

-- | A warning entry counts as a typed-hole iff its 'message' text
-- mentions "typed hole". GHC's diagnostic wording is stable on
-- this phrase and is what 'ghc_hole' pattern-matches internally.
isTypedHoleWarning :: Value -> Bool
isTypedHoleWarning (Object o) = case KeyMap.lookup "message" o of
  Just (String s) ->
    "typed hole" `T.isInfixOf` T.toLower s
    || "found hole" `T.isInfixOf` T.toLower s
  _ -> False
isTypedHoleWarning _ = False

loadHasErrors :: Value -> Bool
loadHasErrors v = case envField "errors" v of
  Just (Array a) -> not (null a)
  _              -> False

depsAction :: Value -> Maybe Text
depsAction = stringField "action"

regressionAction :: Value -> Maybe Text
regressionAction = stringField "action"

qcState :: Value -> Maybe Text
qcState = stringField "state"

cabalErrors :: Value -> Maybe Int
cabalErrors = intField "errors"

-- | Issue #90 Phase D: 'success' was dropped from the wire.
-- These helpers now read the envelope's @status@ discriminator
-- and return True iff status='ok' (or 'partial', matching the
-- legacy projection).
gatePassed :: Value -> Bool
gatePassed = statusOk_

-- | Same for 'ghc_determinism'.
determinismPassed :: Value -> Bool
determinismPassed = statusOk_

-- | Internal: success-equivalent boolean. Reads the envelope's
-- @status@ discriminator first; falls back to the pre-#90
-- @success :: Bool@ shape for callers (and unit tests) that
-- pass the legacy payload directly.
statusOk_ :: Value -> Bool
statusOk_ v = case envField "status" v of
  Just (String "ok")      -> True
  Just (String "partial") -> True
  Just _                  -> False
  Nothing                 -> case envField "success" v of
    Just (Bool b) -> b
    _             -> False

--------------------------------------------------------------------------------
-- injection
--------------------------------------------------------------------------------

-- | Splice a 'NextStep' into the first 'TextContent' block of a
-- 'ToolResult', assuming that block's text is JSON-encoded. If the
-- content is not JSON or not an object, the tool result is returned
-- unchanged — we prefer silently skipping injection over corrupting
-- a non-JSON payload.
injectNextStep :: NextStep -> ToolResult -> ToolResult
injectNextStep ns tr = tr { trContent = map splice (trContent tr) }
  where
    splice (TextContent t) = case decodeObject t of
      Nothing -> TextContent t
      Just o  ->
        let enriched = Object (KeyMap.insert "nextStep" (toJSON ns) o)
        in TextContent (encodeText enriched)

-- | Decode a Text into a JSON object. Returns 'Nothing' if the Text
-- is not valid JSON or not an object at the top level.
decodeObject :: Text -> Maybe (KeyMap.KeyMap Value)
decodeObject t =
  case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (Object o) -> Just o
    _               -> Nothing

encodeText :: Value -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode
