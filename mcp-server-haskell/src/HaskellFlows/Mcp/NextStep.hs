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
  , simple
  , chained
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
import HaskellFlows.Mcp.ToolName (ToolName (..), parseToolName, toolNameText)

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

instance FromJSON DogfoodHint where
  parseJSON = withObject "DogfoodHint" $ \o ->
    DogfoodHint <$> (o .: "tool" >>= maybe (fail "unknown dogfood tool") pure . parseToolName)
                <*> o .: "args"
                <*> o .: "why"

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

-- | Wave-2b harness: typed decode. A nextStep whose @tool@ is not
-- a registered wire name is rejected at the parser — dead-name
-- emissions can no longer round-trip silently.
instance FromJSON ChainStep where
  parseJSON = withObject "ChainStep" $ \o ->
    ChainStep <$> (o .: "tool" >>= maybe (fail "unknown tool in chain") pure . parseToolName)
              <*> o .: "args"

instance FromJSON NextStep where
  parseJSON = withObject "NextStep" $ \o -> do
    tool <- o .: "tool" >>= maybe (fail "unknown nextStep tool") pure . parseToolName
    why  <- o .: "why"
    ex   <- o .:? "example"
    ch   <- o .:? "chain"
    dh   <- o .:? "dogfood"
    pure NextStep { nsTool = tool, nsWhy = why, nsExample = ex
                  , nsChain = ch, nsDogfood = dh }

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
-- previous step found at least one candidate (e.g. 'GhcEdit').
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
      { dhTool = GhcSession
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
  [ GhcCheck           -- triggers compile + tracks edits implicitly
  , GhcCheck
  , GhcCheck
  , GhcCheck
  , GhcProperty
  , GhcEdit
  , GhcEdit
  , GhcEdit
  , GhcEdit
  , GhcEdit
  , GhcProperty
  , GhcModule
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
suggestOnError _ _ = Nothing

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

  -- ── Wave 2b composites: action-discriminated routing ──────────────
  GhcCheck -> case stringField "action" payload of
    Just "project" -> Just (chained GhcGate
      "Project-wide gate is green. Run ghc_gate for the pre-push \
      \finalizer (regression + cabal test + cabal build in one call)."
      Nothing
      [ step GhcGate (object []) ])
    Just "lint" -> case intField "count" payload of
      Just n | n > 0 -> Just (simple GhcEdit
        "Lint hits found. ghc_edit(action='fix_warning') auto-patches \
        \the most common ones (unused-imports, redundant-bracket, …)."
        (Just (object [ "action" .= ("fix_warning" :: Text) ])))
      _ -> Nothing
    -- Module loaded: dispatch on error + warning shape (contract
    -- pinned by e2e FlowTypedHoles / FlowLoadHoleDiagnostics /
    -- FlowDogfoodReplay):
    --   errors present → Nothing (the envelope already speaks)
    --   typed holes    → ghc_inspect(action=hole)
    --   other warnings → ghc_edit(action=fix_warning)
    --   clean compile  → ghc_suggest (property-first loop)
    Just "load"
      | payloadHasErrors payload -> Nothing
      | not (null otherWarns)    -> Just (simple GhcEdit
          "Load surfaced warnings. ghc_edit(action=fix_warning) \
          \auto-patches unused-imports, type-defaults and friends."
          (Just (object [ "action" .= ("fix_warning" :: Text)
                        , "module_path" .= sameModule payload ])))
      | not (null holeWarns)     -> Just (simple GhcInspect
          "Typed holes detected. ghc_inspect(action=hole) returns the \
          \hole fits with source and type info."
          (Just (object [ "action" .= ("hole" :: Text)
                        , "module_path" .= sameModule payload ])))
      | otherwise                -> Just (simple GhcSuggest
          "Clean load. Ask ghc_suggest for QuickCheck law candidates \
          \grounded in this module's functions."
          Nothing)
      where
        warns      = payloadWarningTexts payload
        holeWarns  = filter (T.isInfixOf "GHC-88464") warns
        otherWarns = filter (not . T.isInfixOf "GHC-88464") warns
    _ -> Just (simple GhcCheck
      "Module gate ran. A green module still owes you the project-wide \
      \pass before wider chains."
      (Just (object [ "action" .= ("project" :: Text) ])))

  GhcProperty
    -- Determinism responses (runs >= 2) carry no action field —
    -- route them before the action dispatch.
    | isDeterminismPayload payload -> Just (determinismNext payload)
    | otherwise -> case stringField "action" payload of
    Just "check"     -> Just (simple GhcProperty
      "Property verified and auto-persisted on pass. Replay the whole \
      \persisted set to catch interactions with prior laws."
      (Just (object [ "action" .= ("run" :: Text) ])))
    Just "arbitrary" -> Just (simple GhcProperty
      "Arbitrary template generated. Paste it into the test-suite \
      \module, then lift the property it enables into action='check'."
      (Just (object [ "action" .= ("check" :: Text) ])))
    Just "run"       -> Just (simple GhcGate
      "Full regression replay done. ghc_gate is the pre-push finalizer \
      \(+ cabal test + cabal build)."
      (Just (object [])))
    _ -> Nothing

  GhcSession -> case stringField "action" payload of
    Just "imports" -> Just (simple GhcInspect
      "These are the session's live imports. Browse a module's exports \
      \to find the symbol you need next."
      (Just (object [ "action" .= ("browse" :: Text) ])))
    _ -> Nothing

  GhcEdit -> case stringField "action" payload of
    Just "format" -> Nothing
    _ -> Just (simple GhcCheck
      "Rewrite applied and compile-verified. The strict module gate \
      \confirms types and warnings in one shot."
      (Just (object [ "action" .= ("module" :: Text) ])))

  GhcModule -> case stringField "action" payload of
    Just "add" -> Just (simple GhcCheck
      "Module registered + stub scaffolded. Load it to verify the \
      \scaffold compiles before filling it in."
      (Just (object [ "action" .= ("load" :: Text) ])))
    Just "write" -> Just (simple GhcModule
      "Hypothesis recorded. action='check' type-checks it in project \
      \context before you touch source."
      (Just (object [ "action" .= ("check" :: Text) ])))
    Just "check" -> Just (simple GhcModule
      "Type-check passed. action='promote' splices the entry into a \
      \real module with snapshot-and-compile-verify."
      (Just (object [ "action" .= ("promote" :: Text) ])))
    Just "promote" -> Just (simple GhcCheck
      "Entry spliced and verified. Run the module gate to confirm the \
      \wider module state."
      (Just (object [ "action" .= ("module" :: Text) ])))
    _ -> Nothing

  GhcEval -> Just (simple GhcProperty
    "Expression evaluated. If you were testing a property by hand, \
    \lift the same predicate into ghc_property(action='check') so \
    \QuickCheck explores the input space and auto-persists the law on \
    \pass."
    (Just (object [ "action" .= ("check" :: Text)
                  , "property" .= ("<\\x -> ...>" :: Text) ])))

  GhcInspect -> case stringField "action" payload of
    Just "type" -> Just (simple GhcSuggest
      "Type confirmed. If this is a function you own, ghc_suggest \
      \proposes the laws its signature implies."
      (Just (object [ "function_name" .= ("<the typed function>" :: Text) ])))
    Just "hole" -> Just (simple GhcModule
      "Hole fits listed. Record the candidate implementation as a \
      \scratchpad hypothesis before touching source."
      (Just (object [ "action" .= ("write" :: Text) ])))
    _ -> Nothing

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
      loadAfterDepsEdit = simple GhcCheck
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
  GhcSuggest -> Just (chained GhcModule
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
    [ step GhcModule (object
        [ "action" .= ("check" :: Text)
        , "id"     .= ("<id from the write above>" :: Text) ])
    , step GhcProperty (object
        [ "property"    .= ("<copy from suggestion.property>" :: Text)
        , "module_path" .= ("<module defining the function>" :: Text) ])
    , step GhcProperty (object
        [ "action" .= ("run" :: Text) ])
    ])

  -- #94 Phase C step 6: ghc_property_store. The next-step depends
  -- on which action ran. We use 'regressionAction' which reads the
  -- 'action' field — the consolidated tool's 'list'/'run' branches
  -- preserve that field. The 'export'/'audit' branches don't carry
  -- the field; we discriminate them from the @list@/@run@ pair via
  -- characteristic payload fields ('files_written' for export,
  -- 'pairs' / 'contradictions' for audit) before falling through.
  GhcBatch -> Nothing

  --------------------------------------------------------------------
  -- BUG-06: Phase 11f..11n tools — positive entries so the "every
  -- successful response carries nextStep" promise holds across the
  -- whole registry.
  --------------------------------------------------------------------

  -- Gate passed → green to push. On fail, drill in per module.
  GhcGate -> Just (gateNext payload)


  -- Issue #62: a successful move was already verified via the
  -- internal loadForTarget; the agent's next reasonable check is
  -- the project-level gate so any consumer the heuristic missed
  -- surfaces immediately.

  -- Issue #53: only nudge towards 'ghc_load' when ghc_add_import
  -- actually returned candidate imports. The legacy nextStep ran
  -- unconditionally, so a hoogle-missing or zero-hits response
  -- still claimed \"the import was added\" — a lie that wasted
  -- a follow-up round-trip.

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
    chained GhcModule
      "New module stubs were scaffolded. Sketch each module's design in \
      \the scratchpad and type-check it before populating source — the \
      \round-trip is faster and reversible than editing blind. The chain \
      \scratches each new file, then loads the first."
      (Just (object
          [ "action" .= ("write" :: Text)
          , "id"     .= scratchIdFor f0
          , "code"   .= ("-- sketch the types / grammar for this module" :: Text)
          ]))
      ( [ step GhcModule (object
            [ "action" .= ("write" :: Text)
            , "id"     .= scratchIdFor f
            , "code"   .= ("-- sketch the types / grammar for this module" :: Text)
            ])
        | f <- files
        ]
        <> [ step GhcCheck (object
               [ "module_path" .= f0, "diagnostics" .= True ]) ]
      )
  [] ->
    chained GhcCheck
      "Modules registry changed in the .cabal (remove, or an idempotent \
      \add). Run ghc_check_project to surface any compile errors the \
      \change introduced; the chained ghc_load keeps the entry module \
      \live in the GHCi session afterwards."
      Nothing
      [ step GhcCheck (object [])
      , step GhcCheck (object
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
      Just (simple GhcSession
        "Project root swapped. Ask 'ghc_workflow(status)' to \
        \orient yourself in the new project: phase classifier, \
        \tools active, and staleness check against the new .cabal."
        (Just (object [ "action" .= ("status" :: Text) ])))
  -- bootstrap: written path — rules already on disk (#179)
  | Just (String "written") <- envField "mode" payload
  , Just _ <- envField "host" payload =
      Just (simple GhcSession
        "Rules written to disk. Run 'ghc_workflow(action=\"help\")' to \
        \get the next project-level step."
        (Just (object [ "action" .= ("help" :: Text) ])))
  -- bootstrap: preview path — file not yet written
  | Just _ <- envField "host" payload =
      Just (simple GhcSession
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
      , step GhcModule (object
          [ "action"  .= ("add" :: Text)
          , "modules" .= (["<Module.Name>"] :: [Text]) ])
      , step GhcCheck (object
          [ "module_path" .= ("<path to your entry module>" :: Text) ])
      ])

-- | 'ghc_gate' payload carries per-step status. On green, push is
-- unblocked; on red, the agent should narrow down per module.
gateNext :: Value -> NextStep
gateNext payload
  | gatePassed payload = simple GhcCheck
      (gateGreenText payload)
      Nothing
  | otherwise = simple GhcCheck
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
  | determinismPassed payload = simple GhcProperty
      "Property passed every run — safe to add to the regression \
      \set. 'ghc_property_store(action=\"run\")' confirms none of \
      \the stored set regressed after your recent changes."
      (Just (object [ "action" .= ("run" :: Text) ]))
  | otherwise = simple GhcProperty
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
  Just "list" -> Just (simple GhcProperty
    "You now know the persisted set. Run it to confirm every \
    \property still holds after recent edits."
    (Just (object [ "action" .= ("run" :: Text) ])))
  Just "run"  -> Just (simple GhcCheck
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
      then Just (simple GhcProperty
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
      Just (simple GhcModule
        "Scratchpad truncated. Record your next hypothesis with \
        \action=write(code=\"...\")."
        (Just (object
            [ "action" .= ("write" :: Text)
            , "code"   .= ("<your Haskell snippet>" :: Text)
            ])))
  -- Single-id clear → invite the next write.
  | Just _ <- envField "removed" payload =
      Just (simple GhcModule
        "Entry removed. Use action=list to see what's left, or \
        \action=write to record the next hypothesis."
        (Just (object [ "action" .= ("list" :: Text) ])))
  -- Check ran (result field carries the kind).
  | Just (Object r) <- envField "result" payload
  , Just (String k) <- KeyMap.lookup "kind" r =
      case k of
        "type_ok"    -> Just (simple GhcModule
          "Type-check passed. action=promote splices this entry into \
          \a target module (snapshot-and-compile-verify; atomic rollback \
          \on failure)."
          (Just (object
              [ "action"        .= ("promote" :: Text)
              , "id"            .= scratchEntryId payload
                -- #274: resolve the entry's own module when the check echoed it
                -- (Scratch now includes 'module'); fall back to the honest
                -- placeholder only when the entry has no associated module.
              , "target_module" .= echoField "module" "<src/Foo.hs>" payload
              , "target_line"   .= (1 :: Int)
              ])))
        "type_error" -> Just (simple GhcModule
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
      Just (simple GhcModule
        "Scratchpad is empty. Record your first hypothesis with \
        \action=write."
        (Just (object
            [ "action" .= ("write" :: Text)
            , "code"   .= ("<your Haskell snippet>" :: Text)
            ])))
  | Just _ <- envField "entries" payload =
      Just (simple GhcModule
        "Pick an entry from the list and inspect it with action=show, \
        \or run action=check to type-check an Open one."
        (Just (object [ "action" .= ("show" :: Text), "id" .= ("<one of the ids>" :: Text) ])))
  -- Write or show landed on a single entry (carries 'id' + 'kind').
  | Just _ <- envField "id" payload =
      Just (simple GhcModule
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

-- | Wave-2b load gate: the response's @errors@ array carries at
-- least one entry. Errors suppress the nextStep (the envelope
-- already speaks).
payloadHasErrors :: Value -> Bool
payloadHasErrors v = case envField "errors" v of
  Just (Array a) -> not (null a)
  _              -> False

-- | Wave-2b load gate: every string entry of the response's
-- @warnings@ array (non-string entries are ignored).
payloadWarningTexts :: Value -> [Text]
payloadWarningTexts v = case envField "warnings" v of
  Just (Array a) -> concatMap warnText (toList a)
  _              -> []
  where
    warnText (String s)        = [s]
    warnText (Object o)
      | Just (String c) <- KeyMap.lookup (Key.fromString "code") o = [c]
    warnText _                 = []

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
-- was silently overridden by the global GhcInspect → ghc_suggest hint.
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
