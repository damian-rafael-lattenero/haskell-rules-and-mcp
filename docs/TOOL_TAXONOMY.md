# haskell-flows MCP — Tool Taxonomy

> Wave 2b (2026-10).  The canonical classification of all 13 registered tools.
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
| **Composite** | Internally chains ≥2 primitives; exposed as a single surface point via an `action` discriminator. |
| **Gate** | Zero-argument (or single-flavour) composite that returns a binary green/red decision. Used as a pre-push hook. |
| **Control-plane** | Talks *about* the MCP or toolchain, not about Haskell source. Used for orientation and recovery. |

---

## Primitives (6)

| Tool | Wire name | Notes |
|---|---|---|
| `GhcEval` | `ghc_eval` | Evaluate an expression in the cached GHCi env |
| `GhcInspect` | `ghc_inspect` | action: `type`/`hole`/`info`/`browse`/`complete`/`goto`/`doc` — the seven thin read verbs |
| `GhcDeps` | `ghc_deps` | action: `add`/`remove`/`list`/`explain` — dependency surgery on the .cabal |
| `GhcSuggest` | `ghc_suggest` | Law candidates from the current module (property-first loop) |
| `GhcProject` | `ghc_project` | action: `create`/`switch`/`list`/`validate`/`bootstrap` — project scaffolding |

## Gates (1)

| Tool | Wire name | Notes |
|---|---|---|
| `GhcCheck` | `ghc_check` | action: `load`/`module`/`project`/`lint` — compile/typecheck gates; successor of `ghc_load`/`ghc_check_module`/`ghc_check_project`/`ghc_lint` |

## Composites (5)

| Tool | Wire name | Notes |
|---|---|---|
| `GhcProperty` | `ghc_property` | action: `check`/`store verbs`/`arbitrary` — QuickCheck run, store/replay, determinism (`runs>=2`), `Arbitrary` derivation; successor of `ghc_quickcheck`/`ghc_property_store` |
| `GhcEdit` | `ghc_edit` | action: refactor verbs + `import`/`exports`/`fix_warning`/`format`; successor of `ghc_refactor`/`ghc_add_import`/`ghc_apply_exports`/`ghc_fix_warning`/`ghc_format` |
| `GhcModule` | `ghc_module` | action: module add/remove + scratchpad `write`/`check`/`list`/`show`/`clear`/`promote`; successor of `ghc_modules`/`ghc_scratch` |
| `GhcGate` | `ghc_gate` | Full pre-push gate: build + tests + lints + property replay |
| `GhcBatch` | `ghc_batch` | Multi-tool macro: regression replay from the property store |

## Control-plane (1)

| Tool | Wire name | Notes |
|---|---|---|
| `GhcSession` | `ghc_session` | action: `status`/`help`/`toolchain`/`warmup`/`imports`/workflow verbs; successor of `ghc_workflow`/`ghc_toolchain`/`ghc_imports` |

---

**Total: **12** tools** (5 primitives + 1 gate + 5 composites + 1 control-plane), down from
31 in wave 2a and 36 at the original audit. Every retired wire name was folded into a
composite `action` — see `docs/condensation-proposal-2026-10.md` for the full mapping.
