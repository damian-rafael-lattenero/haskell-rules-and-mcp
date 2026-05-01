# haskell-flows MCP — Tool Taxonomy

> Issue #94 Phase A.  The canonical classification of all 46 registered tools.
> The four-category breakdown is **CI-enforced** by `testCategoryCountsMatchTaxonomy`
> in `test/Spec.hs`; any change here must be accompanied by a matching change
> in `toolCategory :: ToolName -> ToolCategory` in `src/HaskellFlows/Mcp/ToolName.hs`.

---

## Category definitions

| Category | Description |
|---|---|
| **Primitive** | Atomic operation the agent can compose. No other tool provides the same capability. Removing a primitive loses functionality permanently. |
| **Composite** | Internally chains ≥2 primitives; exposed as a single surface point for round-trip convenience. |
| **Gate** | Zero-argument (or single-flavour) composite that returns a binary green/red decision. Used as a pre-push hook. |
| **Control-plane** | Talks *about* the MCP or toolchain, not about Haskell source. Used for orientation and recovery. |

---

## Primitives (36)

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
| `GhcRefactor` | `ghc_refactor` | `rename_local` + `extract_binding` |
| `GhcMove` | `ghc_move` | Move a symbol across modules (future: `refactor action=move_symbol`) |
| `GhcFormat` | `ghc_format` | fourmolu / ormolu formatter |
| `GhcApplyExports` | `ghc_apply_exports` | Rewrite module export list |
| `GhcFixWarning` | `ghc_fix_warning` | Auto-patch a GHC warning |
| `GhcAddImport` | `ghc_add_import` | Add a missing import |
| `GhcArbitrary` | `ghc_arbitrary` | Generate `Arbitrary` instance template |

### Dependency + project management

| Tool | Wire name | Notes |
|---|---|---|
| `GhcDeps` | `ghc_deps` | `list` / `add` / `remove` build-depends |
| `GhcDepsExplain` | `ghc_deps_explain` | Explain a dependency conflict (future: `deps action=explain`) |
| `GhcModules` | `ghc_modules` | Action-discriminated module registry: `action=add` registers + scaffolds; `action=remove` de-registers (#94 Phase B) |
| `GhcCreateProject` | `ghc_create_project` | Scaffold a new cabal package (future: `project action=create`) |
| `GhcSwitchProject` | `ghc_switch_project` | Switch the active project root (future: `project action=switch`) |
| `GhcValidateCabal` | `ghc_validate_cabal` | `cabal check` + heuristics (future: `project action=validate`) |
| `GhcBootstrap` | `ghc_bootstrap` | Emit or write host-rules doc (future: `project action=bootstrap`) |

### Property-first testing

| Tool | Wire name | Notes |
|---|---|---|
| `GhcQuickCheck` | `ghc_quickcheck` | Run a single QC property; auto-persist on pass |
| `GhcDeterminism` | `ghc_determinism` | Run property N times for flakiness detection (future: `quickcheck runs=N`) |
| `GhcSuggest` | `ghc_suggest` | Propose QuickCheck laws for a function signature |
| `GhcPropertyLifecycle` | `ghc_property_lifecycle` | `list` / `drop` the property store (future: `property_store action=list|drop`) |
| `GhcRegression` | `ghc_regression` | Replay all persisted properties (future: `property_store action=run`) |
| `GhcQuickCheckExport` | `ghc_quickcheck_export` | Materialise `test/Spec.hs` (future: `property_store action=export`) |
| `GhcPropertyAudit` | `ghc_property_audit` | Audit store for contradictions (future: `property_store action=audit`) |

### Phase-2 advanced

| Tool | Wire name | Notes |
|---|---|---|
| `GhcPerf` | `ghc_perf` | Profile an expression; save/compare baseline |
| `GhcWitness` | `ghc_witness` | Report input-distribution witness for a property |
| `GhcExplainError` | `ghc_explain_error` | Explain + verify a patch for a GHC error |

---

## Composites (4)

| Tool | Wire name | What it composes |
|---|---|---|
| `GhcGate` | `ghc_gate` | `ghc_regression` + `cabal test` + `cabal build` |
| `GhcLab` | `ghc_lab` | `ghc_browse` + `ghc_suggest` + `ghc_quickcheck` per binding + optional `ghc_property_audit` |
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

## Control-plane (3)

| Tool | Wire name | What it does |
|---|---|---|
| `GhcWorkflow` | `ghc_workflow` | `status` / `help` / `next` — session-state-aware orientation |
| `GhcToolchainStatus` | `ghc_toolchain_status` | Probe cabal / ghc / hlint / fourmolu / hoogle / hls |
| `GhcToolchainWarmup` | `ghc_toolchain_warmup` | Pre-warm optional binaries (hoogle index, formatter) |

---

## Totals

| Category | Count |
|---|---|
| Primitive | 35 |
| Composite | 4 |
| Gate | 3 |
| Control-plane | 3 |
| **Total** | **45** |

Phase B (retrofit): `GhcModules` replaces `GhcAddModules` +
`GhcRemoveModules` outright. With a single internal consumer there
was no deprecation cost to honour, so the legacy wire surface was
removed in the same commit as the new tool's introduction.
Net surface change: 46 → 45 tools (one less primitive concept).

Cap: **50** tools (enforced by `testToolCountWithinCap` in `test/Spec.hs`).
Bumping the cap requires an explicit PR with rationale.

---

## Planned consolidation (issue #94 Phases B–F)

The "future:" notes above indicate tools that will be merged into
action-discriminated primitives in later phases:

| Today (14 tools) | Replacement |
|---|---|
| `ghc_add_modules` + `ghc_remove_modules` | `modules { action: "add" \| "remove" }` |
| `ghc_deps_explain` | `deps { action: "explain" }` |
| `ghc_create_project` + `ghc_switch_project` + `ghc_validate_cabal` + `ghc_bootstrap` | `project { action: "create" \| "switch" \| "validate" \| "bootstrap" }` |
| `ghc_property_lifecycle` + `ghc_regression` + `ghc_quickcheck_export` + `ghc_property_audit` | `property_store { action: "list" \| "drop" \| "run" \| "export" \| "audit" }` |
| `ghc_toolchain_warmup` | `toolchain { action: "warmup" }` |
| `ghc_move` | `refactor { action: "move_symbol" }` |
| `ghc_determinism` | `quickcheck { runs: N }` |

Post-consolidation: **31 tools**, ~**22 distinct concepts**.
