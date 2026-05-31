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
    -- * PR-4 Phase 2: dogfood nudge for the MCP itself
  , DogfoodHint (..)
  , withDogfoodHint
  , isWriteTool
  , modulePathInSelf
    -- * Issue #195 (exported for unit tests only)
  , hasDocFalse
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
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
  , nsDogfood :: !(Maybe DogfoodHint)
    -- ^ PR-4 Phase 2: optional sidebar field surfaced when the active
    -- 'projectDir' is a haskell-flows MCP source tree AND the tool
    -- just edited a self-mutable file. Carries an orthogonal nudge
    -- ("after green, run ci-local + commit+push direct to master")
    -- without overriding 'nsTool' / 'nsWhy'. Agents that ignore the
    -- field see no behaviour change.
  }
  deriving stock (Eq, Show)

-- | Sidebar nudge for the dogfood-fix-in-place flow (PR-4 Phase 2).
-- Same shape as a single-step 'NextStep' but namespaced under
-- @dogfood@ in the wire output to make the orthogonality explicit:
-- @nsTool@ is "what work tool comes next?" while 'DogfoodHint' is
-- "what META workflow applies because you're touching the MCP itself?"
data DogfoodHint = DogfoodHint
  { dhTool :: !ToolName
  , dhArgs :: !Value
  , dhWhy  :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON DogfoodHint where
  toJSON dh = object
    [ "tool" .= toolNameText (dhTool dh)
    , "args" .= dhArgs dh
    , "why"  .= dhWhy dh
    ]

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
      <> maybe [] (\d -> ["dogfood" .= d]) (nsDogfood ns)

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
  , nsDogfood = Nothing
  }

-- | Multi-step hint: the first step is the primary suggestion;
-- 'chain' carries the full bundle the agent can batch via
-- @ghc_batch(actions=chain)@.
chained :: ToolName -> Text -> Maybe Value -> [ChainStep] -> NextStep
chained tool why ex chain = (simple tool why ex) { nsChain = Just chain }

-- | PR-4 Phase 2: attach the dogfood-flow hint to an existing
-- 'NextStep' when ALL three conditions hold:
--
--   1. @isSelfProject@ — the MCP detected its own source tree as
--      the active 'projectDir' (cabal-name heuristic in
--      'HaskellFlows.Mcp.SelfProject').
--   2. The tool that just succeeded is in the write-tool set
--      (see 'isWriteTool').
--   3. The payload's @module_path@ (when present) is under one of
--      the self-mutable subdirs ('modulePathInSelf'), or absent
--      (some write-tools don't carry one — the heuristic still
--      fires because the agent is in a self-project doing write
--      work).
--
-- When the conditions don't all hold, the original 'NextStep' is
-- returned untouched. This keeps the dogfood nudge OFF for any
-- non-self project — read-only safe.
withDogfoodHint
  :: Bool          -- ^ isSelfProject
  -> [FilePath]    -- ^ selfMutableSubdirs (passed in to keep this
                   --   module decoupled from SelfProject's path list)
  -> ToolName
  -> Value         -- ^ tool's success payload
  -> NextStep
  -> NextStep
withDogfoodHint isSelf subdirs toolName payload ns
  | not isSelf            = ns
  | not (isWriteTool toolName) = ns
  | not (modulePathInSelf subdirs payload) = ns
  | otherwise = ns { nsDogfood = Just dogfoodHint }
  where
    dogfoodHint = DogfoodHint
      { dhTool = GhcWorkflow
      , dhArgs = object [ "action" .= ("help" :: Text) ]
      , dhWhy  = "this is the haskell-flows MCP itself; after green, \
                 \run scripts/ci-local.sh on demand and commit+push \
                 \direct to master per the dogfood-fix-in-place flow \
                 \(no reinstall mid-session)."
      }

-- | PR-4 Phase 2: tools that mutate the source tree. Used by
-- 'withDogfoodHint' to gate the dogfood nudge — the prompt doesn't
-- make sense after a read-only inspection (ghc_type, ghc_browse, …).
isWriteTool :: ToolName -> Bool
isWriteTool t = t `elem`
  [ GhcLoad           -- triggers compile + tracks edits implicitly
  , GhcCheckModule
  , GhcCheckProject
  , GhcLint
  , GhcQuickCheck
  , GhcFormat
  , GhcRefactor
  , GhcFixWarning
  , GhcApplyExports
  , GhcAddImport
  , GhcArbitrary
  , GhcModules
  ]

-- | PR-4 Phase 2: check whether the payload's @module_path@ field
-- (when present) lives under one of the self-mutable subdirs.
-- Returns 'True' when the path is absent — some write-tools emit
-- payloads without the field, and the agent IS still doing self-
-- project work, so we err on the side of nudging.
modulePathInSelf :: [FilePath] -> Value -> Bool
modulePathInSelf subdirs payload = case stringField "module_path" payload of
  Nothing   -> True   -- absent → don't gate, fire on tool-set match alone
  Just path ->
    let modPath = T.unpack path
    in any (`isPrefixOfPath` modPath) subdirs
  where
    -- Path-prefix check that respects path separators: "src" is a
    -- prefix of "src/X.hs" but NOT of "srcExternal/X.hs".
    isPrefixOfPath :: FilePath -> FilePath -> Bool
    isPrefixOfPath dir target =
      dir == target
      || isPathPrefix (dir <> "/")  target
      || isPathPrefix (dir <> "\\") target   -- Windows separator
    isPathPrefix :: FilePath -> FilePath -> Bool
    isPathPrefix prefix s = take (length prefix) s == prefix

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
  | not ok    = suggestOnError toolName payload
  | otherwise = case dispatch toolName payload of
      Just ns -> Just ns
      -- A success arm declined to suggest. If the payload is actually a
      -- structured failure the arm couldn't route (the common
      -- isOk=True + status=failed shape), fall through to the
      -- failure-path router rather than leaving the agent empty-handed.
      Nothing -> suggestOnError toolName payload

-- | Failure-path routing (plan A5). The pre-A5 contract left every error
-- with @nextStep = Nothing@ ("errors speak for themselves") — but a
-- compile/type/scope error is exactly where a fresh agent most needs a
-- nudge to the recovery tool. This routes the curated, mechanically-
-- recoverable error KINDS to 'ghc_explain_error' (which decodes the GHC
-- diagnostic and proposes a verifiable patch — imports, signatures, scope
-- fixes), feeding it the error message verbatim. Conservative by design:
-- only the kinds below route; unstructured / security / arg errors return
-- 'Nothing' so the agent reads the message (the pre-A5 behaviour). The
-- self-loop guard keeps a failing 'ghc_explain_error' from recommending
-- itself.
suggestOnError :: ToolName -> Value -> Maybe NextStep
suggestOnError toolName payload = case errorKind payload of
  Just k
    | k `elem` ["compile_error", "type_error", "not_in_scope"]
    , toolName /= GhcExplainError ->
        Just (simple GhcExplainError
          "The code failed to compile. 'ghc_explain_error' decodes the GHC \
          \diagnostic and proposes a verifiable patch (missing import, \
          \signature, or scope fix) — feed it the error text below."
          (Just (object [ "error_text" .= errorMessage payload ])))
  _ -> Nothing

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
          [ "module_path" .= sameModule payload ])))
    (LWNone,       False) -> Just (simple GhcSuggest
      "Module compiles clean. Ask 'ghc_suggest' for QuickCheck \
      \laws its type signatures imply; feed the High-confidence \
      \ones into 'ghc_quickcheck'."
      (Just (object
          [ "function_name" .= ("<pick one from the module>" :: Text) ])))

  -- Typed holes listed → scratch the hole-filler hypothesis, type-check
  -- it against the live session, then reload. Writing to scratch first
  -- lets the LLM verify the fill compiles before touching source and
  -- leaves a record of the reasoning the user can read mid-session.
  GhcHole -> Just (chained GhcScratch
    "Write the hole-filler to the scratchpad, type-check it with \
    \action=check, then use action=promote (or paste manually) once \
    \the type is confirmed. The attached chain bundles all three steps."
    (Just (object
        [ "action" .= ("write" :: Text)
        , "code"   .= ("<fill expression for the hole>" :: Text)
        , "note"   .= ("hole-filler hypothesis" :: Text)
        ]))
    [ step GhcScratch (object
        [ "action" .= ("check" :: Text)
        , "id"     .= ("<id from the write above>" :: Text) ])
    , step GhcLoad (object
        [ "module_path" .= sameModule payload
        , "diagnostics" .= True ])
    ])

  -- Arbitrary template generated → paste + import QC + reload + first
  -- law in a single batchable plan. The instance is dead until imported,
  -- reloaded, and exercised, so the chain captures all three follow-ups.
  GhcArbitrary -> Just (chained GhcLoad
    "Paste the instance into the module, add the QuickCheck import, \
    \reload, and exercise the new instance with a roundtrip law. The \
    \attached chain bundles the three follow-ups for ghc_batch."
    Nothing
    [ step GhcAddImport (object
        [ "name" .= ("Test.QuickCheck" :: Text) ])
    , step GhcLoad (object
        [ "module_path" .= ("<module where you pasted the instance>" :: Text)
        , "diagnostics" .= True ])
    , step GhcQuickCheck (object
        [ "property"    .= ("\\x -> roundtrip x === x" :: Text)
        , "module_path" .= sameModule payload ])
    ])

  -- Suggestions → record the law in the scratchpad first (for
  -- posterity and user visibility), type-check it, then quickcheck.
  -- The chain bundles write → check → quickcheck → replay so the LLM
  -- can drive the full flow in one ghc_batch round-trip.
  GhcSuggest -> Just (chained GhcScratch
    "Write the law candidate to the scratchpad before running it — \
    \the entry records the reasoning and the type-check confirms \
    \the property expression is well-formed. The chain continues \
    \to ghc_quickcheck and a full regression replay."
    (Just (object
        [ "action" .= ("write" :: Text)
        , "code"   .= ("<copy from suggestion.property>" :: Text)
        , "kind"   .= ("note" :: Text)
        , "note"   .= ("law candidate from ghc_suggest" :: Text)
        ]))
    [ step GhcScratch (object
        [ "action" .= ("check" :: Text)
        , "id"     .= ("<id from the write above>" :: Text) ])
    , step GhcQuickCheck (object
        [ "property"    .= ("<copy from suggestion.property>" :: Text)
        , "module_path" .= ("<module defining the function>" :: Text) ])
    , step GhcPropertyStore (object
        [ "action" .= ("run" :: Text) ])
    ])

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
          (Just (object [ "module_path" .= sameModule payload ])))
        Just "failed" -> Just (simple GhcEval
          "Property failed. Evaluate the reported counter-example with \
          \'ghc_eval' to see intermediate values before editing."
          Nothing)
        _ -> Nothing

  -- #94 Phase C step 6: ghc_property_store. The next-step depends
  -- on which action ran. We use 'regressionAction' which reads the
  -- 'action' field — the consolidated tool's 'list'/'run' branches
  -- preserve that field. The 'export'/'audit' branches don't carry
  -- the field; we discriminate them from the @list@/@run@ pair via
  -- characteristic payload fields ('files_written' for export,
  -- 'pairs' / 'contradictions' for audit) before falling through.
  GhcPropertyStore -> propertyStoreNext payload

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
          [ "module_path" .= sameModule payload
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

  -- Lint hits → ghc_fix_warning auto-patches the most common ones
  -- (unused-imports, redundant-bracket, use-isJust, etc.). Suppressed
  -- when 'count' is zero — a clean lint pass needs no follow-up.
  GhcLint -> case intField "count" payload of
    Just n | n > 0 -> Just (simple GhcFixWarning
      "Lint surface listed. ghc_fix_warning auto-patches the common \
      \HLint hits (unused-imports, redundant-bracket, use-isJust, \
      \type-defaults). Feed the first hit's file+line+severity in \
      \and inspect the patch before applying."
      (Just (object [ "module_path" .= ("<first hit's module>" :: Text) ])))
    _              -> Nothing

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
  -- Error explained → record the proposed fix in the scratchpad so
  -- the user can see the reasoning and the LLM can type-check the
  -- hypothesis before touching source. The chain continues to
  -- verify_patch (apply-and-recompile) once the fix is confirmed.
  GhcExplainError -> Just (chained GhcScratch
    "Write the proposed fix to the scratchpad first — the entry \
    \records the reasoning and action=check confirms it's well-typed \
    \before touching source. Then feed it as verify_patch to apply, \
    \recompile, and check error_resolved."
    (Just (object
        [ "action" .= ("write" :: Text)
        , "code"   .= ("<proposed fix>" :: Text)
        , "note"   .= ("fix hypothesis from ghc_explain_error" :: Text)
        ]))
    [ step GhcScratch (object
        [ "action" .= ("check" :: Text)
        , "id"     .= ("<id from the write above>" :: Text) ])
    , step GhcExplainError (object
        [ "module_path"  .= sameModule payload
        , "verify_patch" .= object
            [ "line" .= (0 :: Int)
            , "old"  .= ("<old text>" :: Text)
            , "new"  .= ("<new text>" :: Text)
            ]
        ])
    ])

  -- Issue #60 + chain: the audit just persisted a new batch of
  -- properties. A pairwise audit catches contradictions with the
  -- prior set BEFORE the project gate replays them, then the
  -- check_project replay confirms cross-module consistency.
  GhcLab -> Just (chained GhcPropertyStore
    "Module audit completed and persisted new properties. The chain \
    \first audits the store for pairwise contradictions with prior \
    \entries, then replays the full project gate so cross-module \
    \regressions surface in the same round-trip."
    (Just (object [ "action" .= ("audit" :: Text) ]))
    [ step GhcPropertyStore (object [ "action" .= ("audit" :: Text) ])
    , step GhcCheckProject  (object [])
    ])


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
      (Just (object [ "module_path" .= sameModule payload ])))
    _              -> Nothing

  -- #94 Phase B: action-discriminated successor.  The dispatcher
  -- cares about the post-condition (modules just changed), not which
  -- surface point produced it.  Always recommend a project-wide gate;
  -- both add and remove can dangle imports or break loaders.
  GhcModules -> Just (modulesNext payload)

  -- Applied an export list — reload confirms nothing external broke.
  GhcApplyExports -> Just (simple GhcLoad
    "Module export list was rewritten. Reload to confirm the new \
    \export set still type-checks and every consumer can still \
    \see what it needs."
    (Just (object [ "module_path" .= sameModule payload ])))

  -- Fix-warning emitted a plan — apply it, then reload to confirm.
  GhcFixWarning -> Just (simple GhcLoad
    "The fix plan has been written to disk (apply=true) or returned \
    \as a diff (apply=false — inspect before applying). Reload to \
    \confirm the warning is gone and nothing downstream broke."
    (Just (object [ "module_path" .= sameModule payload ])))

  -- Browse listed bindings — pick one and suggest laws for it.
  GhcBrowse -> Just (simple GhcSuggest
    "You now have the full top-level surface of the module. Pick an \
    \interesting binding and ask ghc_suggest for QuickCheck laws \
    \its signature implies. Names that hint at optimisation \
    \(simplify / normalize / fold / ...) bump soundness rules to \
    \High confidence automatically."
    (Just (object [ "function_name" .= ("<one of the browsed names>" :: Text) ])))

  -- Imports listed → orient on the most-used module and pick a
  -- candidate binding via ghc_browse for the next discovery step.
  GhcImports -> Just (simple GhcBrowse
    "Live imports listed. Browse one of them (typically the most-used \
    \in your code) to discover bindings whose laws you can probe with \
    \ghc_suggest + ghc_quickcheck."
    (Just (object [ "module" .= ("<one of the imports above>" :: Text) ])))

  -- Workflow meta — would loop if we suggested itself.
  GhcWorkflow -> Nothing

  -- Type signature in hand → derive QuickCheck laws from it.
  GhcType -> Just (simple GhcSuggest
    "Type signature in hand. ghc_suggest derives candidate QuickCheck \
    \laws from a function's signature; feed the High-confidence ones \
    \into ghc_quickcheck."
    (Just (object [ "function_name" .= ("<the symbol you just typed>" :: Text) ])))

  -- Definition site located → surface the prose contract via Haddock.
  -- On no_match, the name is not in scope: redirect to hoogle_search. (#185)
  GhcInfo
    | statusNoMatch_ payload -> Just (simple HoogleSearch
        "Name not found in interactive scope — hoogle_search discovers \
        \names across Hackage and surfaces the module to import."
        (Just (object [ "query" .= echoField "name" "<the name you looked up>" payload ])))
    | otherwise -> Just (simple GhcDoc
        "Definition + kind + instances are in. ghc_doc retrieves the \
        \Haddock block (if any) for the contract / corner-cases the \
        \author documented."
        (Just (object [ "name" .= echoField "name" "<same name you just inspected>" payload ])))

  -- Expression evaluated → if it was a property, lift to QC harness.
  -- Suppressed on degraded status (a failed eval has its error, no
  -- need for a generic next-step).
  GhcEval
    | not (statusOk_ payload) -> Nothing
    | otherwise -> Just (simple GhcQuickCheck
        "Expression evaluated. If you were testing a property by hand, \
        \lift the same predicate into ghc_quickcheck so QC explores \
        \the input space + auto-persists the law on pass."
        (Just (object
            [ "property"    .= ("<\\x -> ...>" :: Text)
            , "module_path" .= ("<module providing the binding>" :: Text)
            ])))

  -- Source location returned → browse the module's surface to find
  -- siblings related to the symbol you jumped to.
  -- On no_match: for project-local symbols the session may not have
  -- loaded the containing module — ghc_load is the right first step.
  -- Only fall back to hoogle_search for Hackage-hosted names. (#251)
  GhcGoto
    | statusNoMatch_ payload -> Just (simple GhcLoad
        "Name not found in the loaded session — load the module that \
        \defines it with ghc_load, then retry ghc_goto. If the name is \
        \from an external package, use hoogle_search instead."
        (Just (object [ "module_path" .= ("<path/to/Module.hs>" :: Text) ])))
    | otherwise -> Just (simple GhcBrowse
        "You located the definition. Browse the module to discover sibling \
        \bindings — common patterns + alternative entry points."
        (Just (object [ "module" .= echoField "module" "<location.module from the result>" payload ])))

  -- Doc read → browse the module for siblings with similar contracts.
  -- On no_match, the name is not in scope at all: redirect to hoogle_search. (#185)
  -- Issue #195: distinguish "not in scope" from "in scope, no Haddock".
  -- When the name WAS found (found_in_scope=true or status=ok + hasDoc=false),
  -- ghc_info is more useful than hoogle_search (which searches Hackage).
  GhcDoc
    | statusNoMatch_ payload -> Just (simple HoogleSearch
        "Name not found in Haddock — hoogle_search discovers names \
        \across Hackage and surfaces the module to import."
        (Just (object [ "query" .= echoField "name" "<the name you looked up>" payload ])))
    | hasDocFalse payload -> Just (simple GhcInfo
        "Name is in scope but has no doc string. ghc_info returns the \
        \type, definition site, and instances in one call — more useful \
        \than hoogle_search for a locally-defined name."
        (Just (object [ "name" .= echoField "name" "<same name>" payload ])))
    | otherwise -> Just (simple GhcBrowse
        "Doc read. Browse the same module's full export surface to spot \
        \siblings whose contracts likely follow the same shape."
        (Just (object [ "module" .= ("<module hosting the name>" :: Text) ])))

  -- Complete → drill into a candidate via ghc_info. Suppressed when
  -- the prefix matched zero in-scope identifiers.
  GhcComplete -> case intField "count" payload of
    Just n | n > 0 -> Just (simple GhcInfo
      "You have candidate names. ghc_info on one returns its kind, \
      \definition site, and instances in a single call."
      (Just (object [ "name" .= ("<one of the candidates above>" :: Text) ])))
    _              -> Nothing

  -- Hoogle hits → chain into ghc_add_import to scaffold the import.
  -- Suppressed on zero hits (no candidates to import).
  HoogleSearch -> case intField "count" payload of
    Just n | n > 0 -> Just (chained GhcAddImport
      "Hoogle hits include the module each name lives in. \
      \ghc_add_import scaffolds the import; reload to confirm \
      \the missing-symbol error is gone."
      (Just (object [ "name" .= ("<one of the hit names>" :: Text) ]))
      [ step GhcAddImport
          (object [ "name" .= ("<one of the hit names>" :: Text) ])
      , step GhcLoad
          (object [ "module_path" .= ("<your entry module>" :: Text) ])
      ])
    _              -> Nothing

  -- Coverage report read → ghc_gate is the next pre-push step.
  -- Suppressed on degraded status (failed coverage = surface the
  -- error, not a generic forward-chain).
  GhcCoverage
    | not (statusOk_ payload) -> Nothing
    | otherwise -> Just (simple GhcGate
        "Coverage report read. ghc_gate is the next pre-push finalizer \
        \(regression + cabal test + cabal build in one shot). On green, \
        \you're clear to commit + push."
        Nothing)

  -- #253: ghc_scratch — action-discriminated. The dispatcher picks the
  -- next step based on which action just ran (read off the @action@
  -- field in the payload). Phase 1 ships data-bound actions (write /
  -- list / show / clear); check + promote return a structured
  -- not_implemented for now but the nextStep arms still exist so the
  -- LLM gets directed at the right next call once the next phase
  -- lands.
  GhcScratch -> scratchNext payload

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
-- | #262: design-first routing for ghc_modules. When action="add"
-- scaffolded new module stubs (payload carries a non-empty
-- @created_files@), nudge the agent to sketch + type-check each
-- module's design in the scratchpad BEFORE populating source — the
-- round-trip is faster and reversible. The chain carries concrete
-- file paths (not placeholders), so it is genuinely ghc_batch-ready.
-- When nothing was created (remove, or an idempotent add), fall back
-- to the project-wide gate.
modulesNext :: Value -> NextStep
modulesNext payload = case createdFilesField payload of
  files@(f0 : _) ->
    chained GhcScratch
      "New module stubs were scaffolded. Sketch each module's design in \
      \the scratchpad and type-check it before populating source — the \
      \round-trip is faster and reversible than editing blind. The chain \
      \scratches each new file, then loads the first."
      (Just (object
          [ "action" .= ("write" :: Text)
          , "id"     .= scratchIdFor f0
          , "code"   .= ("-- sketch the types / grammar for this module" :: Text)
          ]))
      ( [ step GhcScratch (object
            [ "action" .= ("write" :: Text)
            , "id"     .= scratchIdFor f
            , "code"   .= ("-- sketch the types / grammar for this module" :: Text)
            ])
        | f <- files
        ]
        <> [ step GhcLoad (object
               [ "module_path" .= f0, "diagnostics" .= True ]) ]
      )
  [] ->
    chained GhcCheckProject
      "Modules registry changed in the .cabal (remove, or an idempotent \
      \add). Run ghc_check_project to surface any compile errors the \
      \change introduced; the chained ghc_load keeps the entry module \
      \live in the GHCi session afterwards."
      Nothing
      [ step GhcCheckProject (object [])
      , step GhcLoad (object
          [ "module_path" .= ("<your entry module>" :: Text) ])
      ]

-- | Read the @created_files@ array of path strings from a
-- ghc_modules(add) payload. Empty when absent or on remove.
createdFilesField :: Value -> [Text]
createdFilesField v = case envField "created_files" v of
  Just (Array xs) -> [ s | String s <- toList xs ]
  _               -> []

-- | #262: a stable scratchpad id for a created file, e.g.
-- @src/Expr/Pretty.hs@ → @design-Expr-Pretty@. Re-runs reuse the id so
-- duplicate entries don't pile up.
scratchIdFor :: Text -> Text
scratchIdFor path =
  let noSrc = fromMaybe path (T.stripPrefix "src/" path)
      noExt = fromMaybe noSrc (T.stripSuffix ".hs" noSrc)
  in "design-" <> T.replace "/" "-" noExt

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
  -- bootstrap: written path — rules already on disk (#179)
  | Just (String "written") <- envField "mode" payload
  , Just _ <- envField "host" payload =
      Just (simple GhcWorkflow
        "Rules written to disk. Run 'ghc_workflow(action=\"help\")' to \
        \get the next project-level step."
        (Just (object [ "action" .= ("help" :: Text) ])))
  -- bootstrap: preview path — file not yet written
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
      (gateGreenText payload)
      Nothing
  | otherwise = simple GhcCheckProject
      "At least one gate step failed. Drop one level down into \
      \ghc_check_project to isolate the red module, then drill in \
      \with ghc_check_module + ghc_load(diagnostics=true)."
      Nothing

-- | Issue #208: derive the success text from the payload's 'summary'
-- field (which only lists the non-skipped gates) instead of
-- hardcoding all three names. The hardcoded text claimed all three
-- gates passed even when the caller used skip_regression /
-- skip_cabal_test / skip_cabal_build.
gateGreenText :: Value -> Text
gateGreenText payload = case envField "summary" payload of
  Just (String s) ->
    s <> " Optional: run ghc_coverage for the HPC summary."
  _ ->
    "ghc_gate is green — all gates passed. \
    \Optional: run ghc_coverage for the HPC summary. \
    \Otherwise you're clear to git commit + push."

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
--
-- #94 Phase C step 6: the regression-replay tool is now
-- 'ghc_property_store(action=run)' — the recommendation example
-- carries the 'action' field as before.
determinismNext :: Value -> NextStep
determinismNext payload
  | determinismPassed payload = simple GhcPropertyStore
      "Property passed every run — safe to add to the regression \
      \set. 'ghc_property_store(action=\"run\")' confirms none of \
      \the stored set regressed after your recent changes."
      (Just (object [ "action" .= ("run" :: Text) ]))
  | otherwise = simple GhcQuickCheck
      "Property was flaky (failed at least one run). Re-run \
      \ghc_quickcheck to get a counter-example you can evaluate \
      \with ghc_eval, then fix the underlying code."
      Nothing

-- | #94 Phase C step 6: pick the right next-step based on which
-- 'ghc_property_store' action ran. Routing:
--
--   * @action=list@      → run the persisted set
--   * @action=run@       → roll into the project-wide gate
--   * (looks like export — @files_written@ is non-empty) → run gate
--   * (looks like audit  — @findings@ field exists)      → list,
--                          so the agent can decide which entry to drop
--   * otherwise → Nothing (nothing actionable)
propertyStoreNext :: Value -> Maybe NextStep
propertyStoreNext payload = case regressionAction payload of
  Just "list" -> Just (simple GhcPropertyStore
    "You now know the persisted set. Run it to confirm every \
    \property still holds after recent edits."
    (Just (object [ "action" .= ("run" :: Text) ])))
  Just "run"  -> Just (simple GhcCheckProject
    "All persisted properties re-played. Roll into the project-wide \
    \gate for pre-push readiness."
    Nothing)
  _ ->
    -- export branch: 'files_written' carries the path on success.
    if hasField "files_written" payload
      then Just (simple GhcGate
        "test/Spec.hs is now materialised. Run ghc_gate to replay \
        \the persisted properties the same way cabal test will in \
        \CI — this is the regression check that catches a property \
        \breaking between export + push."
        Nothing)
    -- audit branch: 'findings' is the contradictions array.
    else if hasField "findings" payload
      then Just (simple GhcPropertyStore
        "Audit completed. If 'findings' is non-empty, decide which \
        \property reflects real intent and run \
        \ghc_property_store(action=\"list\") to pick the entry. If \
        \empty, the store is consistent — run ghc_check_project."
        (Just (object [ "action" .= ("list" :: Text) ])))
    else Nothing
  where
    hasField k v = case envField k v of
      Just _  -> True
      Nothing -> False

-- | #253: action-discriminated nextStep for ghc_scratch.
--
-- We discriminate on:
--   * 'id' present in payload + ('result' present) → check just ran.
--   * 'cleared' = true                              → bulk clear ran.
--   * 'removed' present                             → single-id clear ran.
--   * 'count' + 'entries' field                     → list ran.
--   * Single-entry shape (no 'count', has 'id' + 'code') → show or write.
--
-- The hints route the LLM into the pair-programming flow:
--   write → check (verify the type)
--   check (type_ok) → promote (or quickcheck if it's a property)
--   check (type_error) → write (corrected hypothesis)
--   show → check (still the natural verification step)
--   list → write (when empty) or show (when non-empty)
--   clear → write (start fresh)
scratchNext :: Value -> Maybe NextStep
scratchNext payload
  -- Bulk clear → invite a fresh write.
  | Just (Bool True) <- envField "cleared" payload =
      Just (simple GhcScratch
        "Scratchpad truncated. Record your next hypothesis with \
        \action=write(code=\"...\")."
        (Just (object
            [ "action" .= ("write" :: Text)
            , "code"   .= ("<your Haskell snippet>" :: Text)
            ])))
  -- Single-id clear → invite the next write.
  | Just _ <- envField "removed" payload =
      Just (simple GhcScratch
        "Entry removed. Use action=list to see what's left, or \
        \action=write to record the next hypothesis."
        (Just (object [ "action" .= ("list" :: Text) ])))
  -- Check ran (result field carries the kind).
  | Just (Object r) <- envField "result" payload
  , Just (String k) <- KeyMap.lookup "kind" r =
      case k of
        "type_ok"    -> Just (simple GhcScratch
          "Type-check passed. action=promote splices this entry into \
          \a target module (snapshot-and-compile-verify; atomic rollback \
          \on failure)."
          (Just (object
              [ "action"        .= ("promote" :: Text)
              , "id"            .= scratchEntryId payload
              , "target_module" .= ("<src/Foo.hs>" :: Text)
              , "target_line"   .= (1 :: Int)
              ])))
        "type_error" -> Just (simple GhcScratch
          "Type-check failed. Write a corrected hypothesis under the \
          \same id; action=check will re-verify."
          (Just (object
              [ "action" .= ("write" :: Text)
              , "id"     .= scratchEntryId payload
              , "code"   .= ("<corrected snippet>" :: Text)
              ])))
        _ -> Nothing
  -- list ran (count + entries shape).
  | Just (Number 0) <- envField "count" payload =
      Just (simple GhcScratch
        "Scratchpad is empty. Record your first hypothesis with \
        \action=write."
        (Just (object
            [ "action" .= ("write" :: Text)
            , "code"   .= ("<your Haskell snippet>" :: Text)
            ])))
  | Just _ <- envField "entries" payload =
      Just (simple GhcScratch
        "Pick an entry from the list and inspect it with action=show, \
        \or run action=check to type-check an Open one."
        (Just (object [ "action" .= ("show" :: Text), "id" .= ("<one of the ids>" :: Text) ])))
  -- Write or show landed on a single entry (carries 'id' + 'kind').
  | Just _ <- envField "id" payload =
      Just (simple GhcScratch
        "Entry persisted / shown. Type-check it with action=check."
        (Just (object
            [ "action" .= ("check" :: Text)
            , "id"     .= scratchEntryId payload
            ])))
  | otherwise = Nothing
  where
    scratchEntryId v = case stringField "id" v of
      Just t  -> toJSON t
      Nothing -> String "<entry-id>"

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

-- | #A5: the structured error 'kind' (e.g. "compile_error", "type_error",
-- "not_in_scope") read from the envelope's top-level @error@ object.
-- Drives 'suggestOnError'. Returns 'Nothing' for an unstructured error
-- (e.g. @error@ is a bare string), so those keep suppressing the hint.
errorKind :: Value -> Maybe Text
errorKind p = stringField "kind" =<< envField "error" p

-- | #A5: the envelope's error 'message', fed to ghc_explain_error as
-- @error_text@. Empty when absent.
errorMessage :: Value -> Text
errorMessage p = fromMaybe "" (stringField "message" =<< envField "error" p)

-- | #270: resolve the "<same module>" placeholder family to the
-- concrete @module_path@ the current tool's payload carries (ghc_load,
-- ghc_hole, ghc_refactor, ghc_fix_warning, ghc_apply_exports all echo
-- it), so the emitted chain is genuinely ghc_batch-ready. Falls back to
-- the placeholder when the payload has no module_path (tools that don't
-- take one) — an honest agent-fill slot.
sameModule :: Value -> Text
sameModule payload = fromMaybe "<same module>" (stringField "module_path" payload)

-- | #A4 (residual): resolve a placeholder from a field the CURRENT payload
-- echoes (generalises 'sameModule'). ghc_info / ghc_doc echo @name@,
-- ghc_goto echoes @module@ — so the canonical follow-up example can carry
-- the concrete value instead of a @\<placeholder\>@, making it
-- copy-paste / ghc_batch ready. Falls back to the placeholder when the
-- field is absent, so it is always safe to apply (only ever upgrades).
echoField :: Text -> Text -> Value -> Text
echoField fld placeholder payload = fromMaybe placeholder (stringField fld payload)

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

-- | True when the response status is @no_match@ — the tool ran
-- cleanly but found no result. Used by lookup tools (ghc_info,
-- ghc_doc, ghc_goto) to route 'nextStep' to 'hoogle_search'. (#185)
statusNoMatch_ :: Value -> Bool
statusNoMatch_ v = envField "status" v == Just (String "no_match")

-- | Issue #195: True when @ghc_doc@ returned status=ok but the name
-- has no documentation (@hasDoc=false@). Uses a case-match rather
-- than @== Just (Bool False)@ to avoid a polymorphic-equality
-- comparison with the Aeson 'Value' constructor.
hasDocFalse :: Value -> Bool
hasDocFalse v = case envField "hasDoc" v of
  Just (Bool b) -> not b
  _             -> False

--------------------------------------------------------------------------------
-- injection
--------------------------------------------------------------------------------

-- | Splice a 'NextStep' into the first 'TextContent' block of a
-- 'ToolResult', assuming that block's text is JSON-encoded. If the
-- content is not JSON or not an object, the tool result is returned
-- unchanged — we prefer silently skipping injection over corrupting
-- a non-JSON payload.
-- | Splice a 'NextStep' into the first 'TextContent' block of a
-- 'ToolResult' — but only when the payload does not already carry a
-- 'nextStep' key. This makes the dispatcher hint a true backstop:
-- tool-specific hints (set via 'Env.withNextStep') are preserved;
-- the dispatcher fills in only when the tool has no opinion.
--
-- Previously this used 'KeyMap.insert' which always overwrote, so
-- 'Env.withNextStep moduleNotInGraphNextStep' in 'Browse.handle'
-- was silently overridden by the global GhcBrowse → ghc_suggest hint.
injectNextStep :: NextStep -> ToolResult -> ToolResult
injectNextStep ns tr = tr { trContent = map splice (trContent tr) }
  where
    splice (TextContent t) = case decodeObject t of
      Nothing -> TextContent t
      Just o  ->
        -- Preserve a tool-specific nextStep; inject only as fallback.
        if KeyMap.member "nextStep" o
          then TextContent t
          else TextContent (encodeText (Object (KeyMap.insert "nextStep" (toJSON ns) o)))

-- | Decode a Text into a JSON object. Returns 'Nothing' if the Text
-- is not valid JSON or not an object at the top level.
decodeObject :: Text -> Maybe (KeyMap.KeyMap Value)
decodeObject t =
  case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just (Object o) -> Just o
    _               -> Nothing

encodeText :: Value -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode
