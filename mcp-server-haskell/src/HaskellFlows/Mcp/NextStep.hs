-- | @nextStep@ — structured "what to do next" hint injected into
-- every successful tool response.
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
-- >     "tool": "ghci_deps",
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
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol

-- | Structured next-step hint. 'nsExample' is an optional sample
-- arguments object the agent can use verbatim. 'nsChain' (BUG-22)
-- is an optional multi-step plan — the agent can execute it as a
-- single @ghci_batch@ call, or walk the steps one by one. The
-- primary @tool@ + @why@ are always the first step's intent, so
-- an agent that ignores @chain@ still gets the right first call.
data NextStep = NextStep
  { nsTool    :: !Text
  , nsWhy     :: !Text
  , nsExample :: !(Maybe Value)
  , nsChain   :: !(Maybe [ChainStep])
  }
  deriving stock (Eq, Show)

-- | One step in a multi-step plan. The fields mirror the shape
-- @ghci_batch@ accepts (@{tool, args}@) so the agent can pass
-- @chain@ straight to @ghci_batch(actions=chain)@.
data ChainStep = ChainStep
  { csTool :: !Text
  , csArgs :: !Value
  }
  deriving stock (Eq, Show)

instance ToJSON ChainStep where
  toJSON cs = object
    [ "tool" .= csTool cs
    , "args" .= csArgs cs
    ]

instance ToJSON NextStep where
  toJSON ns =
    object $
      [ "tool" .= nsTool ns
      , "why"  .= nsWhy ns
      ]
      <> maybe [] (\e -> ["example" .= e]) (nsExample ns)
      <> maybe [] (\c -> ["chain"   .= c]) (nsChain ns)

--------------------------------------------------------------------------------
-- smart constructors
--------------------------------------------------------------------------------

-- | Shorthand: single-step hint, no chain.
simple :: Text -> Text -> Maybe Value -> NextStep
simple tool why ex = NextStep
  { nsTool    = tool
  , nsWhy     = why
  , nsExample = ex
  , nsChain   = Nothing
  }

-- | Multi-step hint: the first step is the primary suggestion;
-- 'chain' carries the full bundle the agent can batch via
-- @ghci_batch(actions=chain)@.
chained :: Text -> Text -> Maybe Value -> [ChainStep] -> NextStep
chained tool why ex chain = (simple tool why ex) { nsChain = Just chain }

-- | Build a chain step from (tool, args object).
step :: Text -> Value -> ChainStep
step tool args = ChainStep { csTool = tool, csArgs = args }

--------------------------------------------------------------------------------
-- decision table
--------------------------------------------------------------------------------

-- | Map a (toolName, wasSuccessful, payload) triple to the next
-- recommended tool. 'Nothing' means no strong suggestion — the
-- agent should fall back to 'ghci_workflow(action="help")' if
-- genuinely unsure.
suggestNext :: Text -> Bool -> Value -> Maybe NextStep
suggestNext toolName ok payload
  | not ok    = Nothing   -- errors speak for themselves; let the agent parse them
  | otherwise = dispatch toolName payload

dispatch :: Text -> Value -> Maybe NextStep
dispatch name payload = case name of

  -- New scaffold → project-bootstrap chain: add the most common
  -- test-suite dep, register the first module, load to confirm.
  -- The agent can execute the three steps one by one OR hand the
  -- chain to ghci_batch for a single round-trip.
  "ghci_create_project" -> Just (chained
    "ghci_deps"
    "Your scaffold has only `base`. Add the deps you need (QuickCheck \
    \for tests, runtime libraries for the library stanza) before \
    \wiring up modules. The attached chain is the canonical \
    \project-bootstrap plan — you can batch it via ghci_batch."
    (Just (object
        [ "action"  .= ("add" :: Text)
        , "package" .= ("QuickCheck" :: Text)
        , "version" .= (">= 2.14" :: Text)
        , "stanza"  .= ("test-suite" :: Text)
        ]))
    [ step "ghci_deps" (object
        [ "action"  .= ("add" :: Text)
        , "package" .= ("QuickCheck" :: Text)
        , "version" .= (">= 2.14" :: Text)
        , "stanza"  .= ("test-suite" :: Text) ])
    , step "ghci_add_modules" (object
        [ "modules" .= (["<Module.Name>"] :: [Text]) ])
    , step "ghci_load" (object
        [ "module_path" .= ("<path to your entry module>" :: Text) ])
    ])

  -- After editing deps, reload to pick up the new package graph.
  "ghci_deps" -> case depsAction payload of
    Just "add"    -> Just loadAfterDepsEdit
    Just "remove" -> Just loadAfterDepsEdit
    _             -> Nothing
    where
      loadAfterDepsEdit = simple "ghci_load"
        "Dependency set changed. Reload your entry module so the \
        \GHCi session sees the new package graph."
        (Just (object
            [ "module_path" .= ("<your entry module>" :: Text) ]))

  -- Module loaded clean → propose laws next.
  "ghci_load" -> case (loadHasWarnings payload, loadHasErrors payload) of
    (_, True)     -> Nothing  -- errors speak for themselves
    (True, False) -> Just (simple "ghci_hole"
      "The load reported warnings — if they are typed holes, \
      \'ghci_hole' gives you their expected types and in-scope \
      \fits in one call."
      Nothing)
    (False, False) -> Just (simple "ghci_suggest"
      "Module compiles clean. Ask 'ghci_suggest' for QuickCheck \
      \laws its type signatures imply; feed the High-confidence \
      \ones into 'ghci_quickcheck'."
      (Just (object
          [ "function_name" .= ("<pick one from the module>" :: Text) ])))

  -- Typed holes listed → implementation work, then reload.
  "ghci_hole" -> Just (simple "ghci_load"
    "Implement the holes using the fits listed above, then reload \
    \with diagnostics=true to confirm the types now line up."
    (Just (object
        [ "module_path" .= ("<same module you just inspected>" :: Text)
        , "diagnostics" .= True
        ])))

  -- Arbitrary template generated → paste + reload.
  "ghci_arbitrary" -> Just (simple "ghci_load"
    "Paste the instance into the module that declares the type, \
    \then reload to confirm it compiles."
    Nothing)

  -- Suggestions → run them via quickcheck, pick highest confidence.
  "ghci_suggest" -> Just (simple "ghci_quickcheck"
    "Feed the highest-confidence suggestion into quickcheck. \
    \Passing properties auto-persist to .haskell-flows/properties.json \
    \for the next regression run."
    (Just (object
        [ "property"    .= ("<copy from suggestion.property>" :: Text)
        , "module_path" .= ("<module defining the function>" :: Text)
        ])))

  -- QuickCheck passed → keep chaining, or gate.
  "ghci_quickcheck" -> case qcState payload of
    Just "passed" -> Just (simple "ghci_check_module"
      "Law holds. Either run 'ghci_suggest' for the next candidate, \
      \or roll up into a per-module gate. For flakiness confidence, \
      \ghci_determinism re-runs the property 3+ times."
      (Just (object [ "module_path" .= ("<same module>" :: Text) ])))
    Just "failed" -> Just (simple "ghci_eval"
      "Property failed. Evaluate the reported counter-example with \
      \'ghci_eval' to see intermediate values before editing."
      Nothing)
    _ -> Nothing

  -- Regression list → run the set.
  "ghci_regression" -> case regressionAction payload of
    Just "list" -> Just (simple "ghci_regression"
      "You now know the persisted set. Run it to confirm every \
      \property still holds after recent edits."
      (Just (object [ "action" .= ("run" :: Text) ])))
    Just "run"  -> Just (simple "ghci_check_project"
      "All persisted properties re-played. Roll into the project-wide \
      \gate for pre-push readiness."
      Nothing)
    _ -> Nothing

  -- Refactor landed → verify compile + rerun regressions.
  "ghci_refactor" -> Just (simple "ghci_load"
    "Refactor was snapshot-and-compile-verified, but a reload with \
    \diagnostics=true surfaces new holes or warnings in one shot."
    (Just (object
        [ "module_path" .= ("<same module>" :: Text)
        , "diagnostics" .= True
        ])))

  -- Per-module gate passed → project-wide gate.
  "ghci_check_module" -> Just (simple "ghci_check_project"
    "Module-complete. Run the project-wide gate to confirm every \
    \other module still compiles cleanly with your changes."
    Nothing)

  -- Project gate green → pre-push finalizer chain.
  "ghci_check_project" -> Just (chained "ghci_gate"
    "Project-wide gate is green. Run ghci_gate for the pre-push \
    \finalizer (regression + cabal test + cabal build in one call). \
    \Coverage is the optional follow-up."
    Nothing
    [ step "ghci_gate"     (object [])
    , step "ghci_coverage" (object [])
    ])

  -- Toolchain — if everything green, go build.
  "ghci_toolchain_status" -> Just (simple "ghci_workflow"
    "With the toolchain confirmed, 'ghci_workflow(action=\"help\")' \
    \gives you the next action tailored to the session's current \
    \state (alive GHCi, loaded modules, etc)."
    (Just (object [ "action" .= ("help" :: Text) ])))

  -- Cabal validated → if clean, proceed with deps / build.
  "ghci_validate_cabal" -> case cabalErrors payload of
    Just n | n > 0 -> Just (simple "ghci_deps"
      "The .cabal file has errors. Fix them via 'ghci_deps' rather \
      \than editing by hand — the post-edit invariant check catches \
      \shape bugs before they land."
      (Just (object [ "action" .= ("list" :: Text) ])))
    _ -> Nothing

  -- Lint surface → interpret yourself; no one-shot fix.
  "ghci_lint" -> Nothing

  -- Format → reload to confirm no behaviour change.
  "ghci_format" -> Just (simple "ghci_load"
    "Formatter rewrote the module. Reload to confirm it still \
    \compiles and no whitespace-sensitive construct broke."
    Nothing)

  -- Batch → no single next step (depends on what the batch did); let
  -- the agent look at the individual results.
  "ghci_batch" -> Nothing

  --------------------------------------------------------------------
  -- BUG-06: Phase 11f..11n tools — positive entries so the "every
  -- successful response carries nextStep" promise holds across the
  -- whole registry.
  --------------------------------------------------------------------

  -- Gate passed → green to push. On fail, drill in per module.
  "ghci_gate" -> Just (gateNext payload)

  -- test/Spec.hs materialised → run ghci_gate to exercise it through
  -- cabal test (same semantics as CI without the MCP in the loop).
  "ghci_quickcheck_export" -> Just (simple "ghci_gate"
    "test/Spec.hs is now materialised. Run ghci_gate to replay the \
    \persisted properties the same way cabal test will in CI — this \
    \is the regression check that catches a property breaking between \
    \export + push."
    Nothing)

  -- Determinism check → if stable, propagate to regression; if
  -- flaky, ask for a fresh ghci_quickcheck run to see counter-
  -- example before deleting.
  "ghci_determinism" -> Just (determinismNext payload)

  -- Adding an import resolves a \"not in scope\" error; reload to
  -- confirm the fix.
  "ghci_add_import" -> Just (simple "ghci_load"
    "The import was added to the module header. Reload the module \
    \to confirm the \"not in scope\" error is gone."
    (Just (object [ "module_path" .= ("<same module>" :: Text) ])))

  -- New modules registered + scaffolded — fill them in, add deps if
  -- they need any new libraries, then load.
  "ghci_add_modules" -> Just (chained "ghci_load"
    "New modules are registered in the .cabal and scaffolded as empty \
    \stubs under src/. Implement them, then reload an entry module \
    \to pick the new layout up. The chain is the canonical \
    \\"scaffolded → loaded\" sequence."
    (Just (object [ "module_path" .= ("<pick an entry module>" :: Text) ]))
    [ step "ghci_load" (object
        [ "module_path" .= ("<your entry module>" :: Text) ])
    , step "ghci_check_project" (object [])
    ])

  -- Applied an export list — reload confirms nothing external broke.
  "ghci_apply_exports" -> Just (simple "ghci_load"
    "Module export list was rewritten. Reload to confirm the new \
    \export set still type-checks and every consumer can still \
    \see what it needs."
    (Just (object [ "module_path" .= ("<same module>" :: Text) ])))

  -- Fix-warning emitted a plan — apply it, then reload to confirm.
  "ghci_fix_warning" -> Just (simple "ghci_load"
    "The fix plan has been written to disk (apply=true) or returned \
    \as a diff (apply=false — inspect before applying). Reload to \
    \confirm the warning is gone and nothing downstream broke."
    (Just (object [ "module_path" .= ("<same module>" :: Text) ])))

  -- Browse listed bindings — pick one and suggest laws for it.
  "ghci_browse" -> Just (simple "ghci_suggest"
    "You now have the full top-level surface of the module. Pick an \
    \interesting binding and ask ghci_suggest for QuickCheck laws \
    \its signature implies. Names that hint at optimisation \
    \(simplify / normalize / fold / ...) bump soundness rules to \
    \High confidence automatically."
    (Just (object [ "function_name" .= ("<one of the browsed names>" :: Text) ])))

  -- Imports list is a diagnostic aid — no forced next step.
  "ghci_imports" -> Nothing

  -- Lifecycle mgmt of the property store — if action=list the
  -- next natural step is to prune or run; leave the choice open.
  "ghci_property_lifecycle" -> case regressionAction payload of
    Just "list" -> Just (simple "ghci_regression"
      "Now that you can see the store, run the regression to \
      \confirm every entry still passes — flaky / broken properties \
      \should be pruned before the next push."
      (Just (object [ "action" .= ("run" :: Text) ])))
    _ -> Nothing

  -- Optional toolchain warmup — next step is the status/help router.
  "ghci_toolchain_warmup" -> Just (simple "ghci_workflow"
    "Optional binaries probed. Ask 'ghci_workflow(action=\"help\")' \
    \for a session-state-aware pointer at the next action."
    (Just (object [ "action" .= ("help" :: Text) ])))

  -- Workflow meta — would loop if we suggested itself.
  "ghci_workflow" -> Nothing

  -- Exploratory / terminal tools — no strong suggestion.
  "ghci_type"     -> Nothing
  "ghci_info"     -> Nothing
  "ghci_eval"     -> Nothing
  "ghci_goto"     -> Nothing
  "ghci_doc"      -> Nothing
  "ghci_complete" -> Nothing
  "hoogle_search" -> Nothing
  "ghci_coverage" -> Nothing

  _ -> Nothing

-- | 'ghci_gate' payload carries per-step status. On green, push is
-- unblocked; on red, the agent should narrow down per module.
gateNext :: Value -> NextStep
gateNext payload
  | gatePassed payload = simple "ghci_coverage"
      "ghci_gate is green — regression + cabal test + cabal build \
      \all passed. Optional: run ghci_coverage for the HPC summary. \
      \Otherwise you're clear to git commit + push."
      Nothing
  | otherwise = simple "ghci_check_project"
      "At least one gate step failed. Drop one level down into \
      \ghci_check_project to isolate the red module, then drill in \
      \with ghci_check_module + ghci_load(diagnostics=true)."
      Nothing

-- | 'ghci_determinism' payload has a top-level @success@ bool.
-- Stable → trust for regression; flaky → show the counter-example.
determinismNext :: Value -> NextStep
determinismNext payload
  | determinismPassed payload = simple "ghci_regression"
      "Property passed every run — safe to add to the regression \
      \set. 'ghci_regression(action=\"run\")' confirms none of \
      \the stored set regressed after your recent changes."
      (Just (object [ "action" .= ("run" :: Text) ]))
  | otherwise = simple "ghci_quickcheck"
      "Property was flaky (failed at least one run). Re-run \
      \ghci_quickcheck to get a counter-example you can evaluate \
      \with ghci_eval, then fix the underlying code."
      Nothing

--------------------------------------------------------------------------------
-- payload probes (small, hand-written, no lens-aeson dep)
--------------------------------------------------------------------------------

-- | Extract a string field from a JSON object payload. Returns
-- 'Nothing' if the payload is not an object, the field is missing,
-- or its value is not a string.
stringField :: Text -> Value -> Maybe Text
stringField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (String s) -> Just s
  _               -> Nothing
stringField _ _ = Nothing

-- | Extract an integer field.
intField :: Text -> Value -> Maybe Int
intField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (Number n) -> Just (round n)
  _               -> Nothing
intField _ _ = Nothing

-- | True if the payload's @warnings@ array is non-empty.
loadHasWarnings :: Value -> Bool
loadHasWarnings (Object o) = case KeyMap.lookup "warnings" o of
  Just (Array a) -> not (null a)
  _              -> False
loadHasWarnings _ = False

loadHasErrors :: Value -> Bool
loadHasErrors (Object o) = case KeyMap.lookup "errors" o of
  Just (Array a) -> not (null a)
  _              -> False
loadHasErrors _ = False

depsAction :: Value -> Maybe Text
depsAction = stringField "action"

regressionAction :: Value -> Maybe Text
regressionAction = stringField "action"

qcState :: Value -> Maybe Text
qcState = stringField "state"

cabalErrors :: Value -> Maybe Int
cabalErrors = intField "errors"

-- | 'ghci_gate' payload has a top-level @success@ bool.
gatePassed :: Value -> Bool
gatePassed = boolField "success"

-- | 'ghci_determinism' payload has a top-level @success@ bool.
determinismPassed :: Value -> Bool
determinismPassed = boolField "success"

-- | Extract a boolean field; missing / malformed → False.
boolField :: Text -> Value -> Bool
boolField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (Bool b) -> b
  _             -> False
boolField _ _ = False

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
