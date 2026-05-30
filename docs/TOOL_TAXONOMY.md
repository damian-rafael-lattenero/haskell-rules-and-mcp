# haskell-flows MCP — Tool Taxonomy

> Issue #94 Phase A.  The canonical classification of all 36 registered tools.
> The four-category breakdown is **CI-enforced** by `testCategoryCountsMatchTaxonomy`,
> and `testTaxonomyDocListsAllTools` (#268) fails CI if this file omits any
> registered wire name or states the wrong total — both in `test/Spec.hs`.
> Any change here must be accompanied by a matching change in
> `toolCategory :: ToolName -> ToolCategory` in `src/HaskellFlows/Mcp/ToolName.hs`.

---

## Category definitions

| Category | Description |
|---|---|
| **Primitive** | Atomic operation the agent can compose. No other tool provides the same capability. Removing a primitive loses functionality permanently. |
| **Composite** | Internally chains ≥2 primitives; exposed as a single surface point for round-trip convenience. |
| **Gate** | Zero-argument (or single-flavour) composite that returns a binary green/red decision. Used as a pre-push hook. |
| **Control-plane** | Talks *about* the MCP or toolchain, not about Haskell source. Used for orientation and recovery. |

---

## Primitives (27)

### Read / inspect

| Tool | Wire name | Notes |
|---|---|---|
| `GhcLoad` | `ghc_load` | Compile a module; return diagnostics |
| `GhcType` | `ghc_type` | `:t <expr>` |
| `GhcInfo` | `ghc_info` | `:i <name>` — declaration + instances |
| `GhcEval` | `ghc_eval` | Single-line expression eval |
| `GhcHole` | `ghc_hole` | Typed holes + in-scope fits |
| `GhcComplete` | `ghc_complete` | `:complete repl` prefix completions |
| `GhcGoto` | `ghc_goto` | Source location of a name |
| `GhcBrowse` | `ghc_browse` | Module top-level surface |
| `GhcImports` | `ghc_imports` | Current interactive context |
| `GhcDoc` | `ghc_doc` | Haddock for a name |
| `HoogleSearch` | `hoogle_search` | Query hoogle by name or type |

### Write / refactor

| Tool | Wire name | Notes |
|---|---|---|
| `GhcRefactor` | `ghc_refactor` | `rename_local` + `extract_binding` + `move_symbol` (#94 Phase C: subsumes the retired `ghc_move`) |
| `GhcFormat` | `ghc_format` | fourmolu / ormolu formatter |
| `GhcApplyExports` | `ghc_apply_exports` | Rewrite module export list |
| `GhcFixWarning` | `ghc_fix_warning` | Auto-patch a GHC warning |
| `GhcAddImport` | `ghc_add_import` | Add a missing import |
| `GhcArbitrary` | `ghc_arbitrary` | Generate `Arbitrary` instance template |

### Dependency + project management

| Tool | Wire name | Notes |
|---|---|---|
| `GhcDeps` | `ghc_deps` | `list` / `add` / `remove` / `explain` build-depends (#94 Phase C: `explain` subsumes the retired `ghc_deps_explain`) |
| `GhcModules` | `ghc_modules` | Action-discriminated module registry: `action=add` registers + scaffolds; `action=remove` de-registers (#94 Phase B) |
| `GhcProject` | `ghc_project` | Project lifecycle: `action=create` scaffolds a cabal package; `action=switch` repoints the active root; `action=validate` runs `cabal check` + heuristics; `action=bootstrap` emits or writes host-rules (#94 Phase C step 5: subsumes the retired `ghc_create_project` + `ghc_switch_project` + `ghc_validate_cabal` + `ghc_bootstrap`) |

### Property-first testing

| Tool | Wire name | Notes |
|---|---|---|
| `GhcQuickCheck` | `ghc_quickcheck` | Run a QC property; auto-persist on pass. Pass `runs >= 2` to repeat the property for flakiness detection (#94 Phase C: subsumes the retired `ghc_determinism`) |
| `GhcSuggest` | `ghc_suggest` | Propose QuickCheck laws for a function signature |
| `GhcPropertyStore` | `ghc_property_store` | Property-store lifecycle: `action=list` (introspect), `action=run` (replay every persisted property), `action=export` (materialise `test/Spec.hs`), `action=audit` (pairwise contradiction probe) (#94 Phase C step 6: subsumes the retired `ghc_property_lifecycle` + `ghc_regression` + `ghc_quickcheck_export` + `ghc_property_audit`) |

### Phase-2 advanced

| Tool | Wire name | Notes |
|---|---|---|
| `GhcPerf` | `ghc_perf` | Profile an expression; save/compare baseline |
| `GhcWitness` | `ghc_witness` | Report input-distribution witness for a property |
| `GhcExplainError` | `ghc_explain_error` | Explain + verify a patch for a GHC error |

### Pair-programming canvas

| Tool | Wire name | Notes |
|---|---|---|
| `GhcScratch` | `ghc_scratch` | Persistent LLM code canvas: `action=write` records a Haskell hypothesis under an id; `action=check` type-checks it; `action=list`/`show` introspect; `action=clear` deletes; `action=promote` splices verified code into a target module via `ghc_refactor`'s snapshot-and-compile-verify (#253) |

---

## Composites (4)

| Tool | Wire name | What it composes |
|---|---|---|
| `GhcGate` | `ghc_gate` | property-store replay + `cabal test` + `cabal build` |
| `GhcLab` | `ghc_lab` | `ghc_browse` + `ghc_suggest` + `ghc_quickcheck` per binding + optional `ghc_property_store` audit |
| `GhcCoverage` | `ghc_coverage` | `cabal test --enable-coverage` + HPC report parse |
| `GhcBatch` | `ghc_batch` | N sequential tool calls with `fail_fast` control |

---

## Gates (3)

| Tool | Wire name | What it checks |
|---|---|---|
| `GhcCheckModule` | `ghc_check_module` | Per-file: compile + warnings + holes + property replay |
| `GhcCheckProject` | `ghc_check_project` | Whole-project: same as above across all exposed-modules |
| `GhcLint` | `ghc_lint` | HLint over the project (matches CI) |

---

## Control-plane (2)

| Tool | Wire name | What it does |
|---|---|---|
| `GhcWorkflow` | `ghc_workflow` | `status` / `help` / `next` — session-state-aware orientation |
| `GhcToolchain` | `ghc_toolchain` | Action-discriminated probe / warmup of cabal / ghc / hlint / fourmolu / hoogle / hls (`action=status` \| `warmup`) — #94 Phase C: subsumes the retired `ghc_toolchain_status` + `ghc_toolchain_warmup` |

---

## Totals

| Category | Count |
|---|---|
| Primitive | 27 |
| Composite | 4 |
| Gate | 3 |
| Control-plane | 2 |
| **Total** | **36** |

* Phase B retrofit: `GhcModules` replaced `GhcAddModules` +
  `GhcRemoveModules` outright (47 → 45 — two less, one new).
* Phase C step 1: `GhcDeps action="explain"` replaced
  `GhcDepsExplain` outright (45 → 44 — one less).
* Phase C step 2: `GhcToolchain action="status"|"warmup"` replaced
  `GhcToolchainStatus` + `GhcToolchainWarmup` outright (44 → 43 —
  two less, one new).
* Phase C step 3: `GhcQuickCheck runs>=2` replaced `GhcDeterminism`
  outright (43 → 42 — one less).
* Phase C step 4: `GhcRefactor action="move_symbol"` replaced
  `GhcMove` outright (42 → 41 — one less).
* Phase C step 5: `GhcProject action="create"|"switch"|"validate"|"bootstrap"`
  replaced `GhcCreateProject` + `GhcSwitchProject` + `GhcValidateCabal` +
  `GhcBootstrap` outright (41 → 38 — four less, one new).
* Phase C step 6: `GhcPropertyStore action="list"|"run"|"export"|"audit"`
  replaced `GhcPropertyLifecycle` + `GhcRegression` + `GhcQuickCheckExport` +
  `GhcPropertyAudit` outright (38 → 35 — four less, one new).
* #253: `GhcScratch` added as a new primitive — persistent LLM code
  canvas / pair-programming surface (35 → 36 — one new).

With a single internal consumer there was no deprecation cost to
honour, so the legacy wire surface was removed in the same commit as
each new action's introduction.

Cap: **50** tools (enforced by `testToolCountWithinCap` in `test/Spec.hs`).
Bumping the cap requires an explicit PR with rationale.

---

## Consolidation history (issue #94 — all landed)

Every merge below shipped; the action-discriminated successor is now the
only wire surface. The live total is **36 tools** (see Totals above),
CI-enforced by `testCategoryCountsMatchTaxonomy` + `testTaxonomyDocListsAllTools`.

| Retired wire tools | Action-discriminated successor |
|---|---|
| `ghc_add_modules` + `ghc_remove_modules` | `ghc_modules { action: "add" \| "remove" }` (Phase B) |
| `ghc_deps_explain` | `ghc_deps { action: "explain" }` (Phase C) |
| `ghc_create_project` + `ghc_switch_project` + `ghc_validate_cabal` + `ghc_bootstrap` | `ghc_project { action: "create" \| "switch" \| "validate" \| "bootstrap" }` (Phase C) |
| `ghc_property_lifecycle` + `ghc_regression` + `ghc_quickcheck_export` + `ghc_property_audit` | `ghc_property_store { action: "list" \| "run" \| "export" \| "audit" }` (Phase C) |
| `ghc_toolchain_warmup` + `ghc_toolchain_status` | `ghc_toolchain { action: "status" \| "warmup" }` (Phase C) |
| `ghc_move` | `ghc_refactor { action: "move_symbol" }` (Phase C) |
| `ghc_determinism` | `ghc_quickcheck { runs: N }` (Phase C) |
