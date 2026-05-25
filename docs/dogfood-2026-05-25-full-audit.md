# Full tool audit dogfooding session — 2026-05-25

## Executive summary

All 35 MCP tools exercised against a fresh scratch project
(`/tmp/dogfood-scratch-2026-05-25`). **9 issues filed**, ranging
from `priority:low` to `priority:high`. The core GHC-API session,
project scaffolding, property testing happy-path, and refactor
safety-net all held up. The issues are concentrated in: output
capture from instrumented QC runs (witness, audit), refactor
coverage (extract_binding, move_symbol export lists), and two
diagnostic discrepancy / remediation accuracy gaps.

---

## Issues filed

| # | Tool | Finding | Severity |
|---|------|---------|----------|
| [#224](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/224) | `ghc_goto` | Qualified preload name (`Data.List.sort`) gives "not in scope" remediation though `sort` IS in scope via unqualified preload | priority:low |
| [#225](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/225) | `ghc_complete` | Qualified prefix (`Data.List.`) returns 0 candidates with wrong remediation though `Data.List` is a session preload | priority:low |
| [#226](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/226) | `ghc_arbitrary` | Parametric type (`data Tagged a b`) misidentified as "GHC wired-in primitive" — reproducible on clean-compiling module | priority:medium |
| [#227](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/227) | `ghc_refactor` | `extract_binding` fails with parse errors on guard branches and where-clause bindings — safety net works but feature is unusable for common patterns | priority:medium |
| [#228](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/228) | `ghc_refactor` | `move_symbol` does not update source module's explicit export list → always fails with "Not in scope" rollback when source has exports | priority:high |
| [#229](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/229) | `ghc_witness` | Distribution always empty (`passed:0, failed:0, total_labels:0, qc_raw_output:""`) for every property — feature entirely non-functional | priority:high |
| [#230](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/230) | `ghc_property_store` | `audit` pre-classifies compatible laws as `"contradictory-pair"` then skips due to probe failure — same root cause as #229 | priority:medium |
| [#231](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/231) | `ghc_property_store` | `export` generates Spec.hs with unused module imports (GHC-66111) and missing type signatures on `prop_N` bindings (GHC-38417) | priority:medium |
| [#232](https://github.com/damian-react-lattenero/haskell-rules-and-mcp/issues/232) | `ghc_check_module` | Reports "no warnings (-Wall clean)" for GHC-18042 type-defaulting warning that `ghc_load` and `cabal test` both surface — stale session cache skips re-compile | priority:medium |

---

## Tool-by-tool verdict

### Control-plane (2/2 ✅)

| Tool | Call | Result |
|------|------|--------|
| `ghc_workflow status` | start-of-session | 35 tools active, phase=PreScaffold |
| `ghc_workflow help` | phase nudge | state-aware steps returned |
| `ghc_workflow next` | post-edit | suggests `ghc_load --diagnostics` |
| `ghc_toolchain status` | start-of-session | 8/8 available, no blocking gates |
| `ghc_toolchain warmup` | mid-session | gates_warm=true |

### Project lifecycle (✅)

| Tool | Call | Result |
|------|------|--------|
| `ghc_project create write=false` | preview | correct 4-file scaffold preview |
| `ghc_project create write=true` | create | 4 files written |
| `ghc_project switch` | switch | previous/current correctly reported |
| `ghc_project validate` | validate | 0 errors, 0 warnings |
| `ghc_project bootstrap host=claude-code write=false` | preview | full .claude/rules/haskell-flows-mcp.md generated |

### Module & dependency management (✅)

| Tool | Call | Result |
|------|------|--------|
| `ghc_deps list` | list | stanzas correctly listed |
| `ghc_deps add QuickCheck` | add | added to test-suite |
| `ghc_deps remove QuickCheck` | remove | removed correctly |
| `ghc_deps explain base` | explain | stanzas + import-sites (0 explicit imports = correct) |
| `ghc_modules add ["Math","Text"]` | add | cabal + stub files created |
| `ghc_modules remove DogfoodScratch.Text` | remove | correctly blocked by downstream import in Spec.hs |

### Read / inspect (mostly ✅, 2 UX issues)

| Tool | Result |
|------|--------|
| `ghc_load` | ✅ — diagnostics=true surfaces holes |
| `ghc_type` | ✅ |
| `ghc_info` | ✅ |
| `ghc_eval` (pure) | ✅ |
| `ghc_eval` (IO) | ✅ — `putStrLn` captured |
| `ghc_hole` | ✅ — gold: type + fits + bindings |
| `ghc_complete` (unqualified) | ✅ |
| `ghc_complete` (qualified prefix) | 🟡 #225 — wrong remediation |
| `ghc_goto` (local) | ✅ — has_location=true in interpreted mode |
| `ghc_goto` (qualified preload) | 🟡 #224 — wrong remediation |
| `ghc_goto` (unqualified preload) | ✅ — returns module, has_location=false (compiled) |
| `ghc_browse` | ✅ |
| `ghc_imports` | ✅ — source + preloads separated |
| `ghc_doc` | ✅ — local Haddock returned |
| `hoogle_search` (name) | ✅ |
| `hoogle_search` (type sig) | ✅ — `Int -> Int -> Int` → 2 specific hits |

### Write / refactor (mixed)

| Tool | Result |
|------|--------|
| `ghc_fix_warning GHC-18042` | ✅ — fixable=false, correct (type-defaulting has no auto-fix) |
| `ghc_format` (check + write) | ✅ — fourmolu |
| `ghc_arbitrary` (sum type) | ✅ — `oneof` template |
| `ghc_arbitrary` (parametric) | 🐛 #226 |
| `ghc_add_import` (unqualified) | ✅ — session_updated=true |
| `ghc_add_import` (qualified) | 🟡 — session_updated=false, 5 candidates, no auto-inject |
| `ghc_apply_exports` (preview + write) | ✅ |
| `ghc_refactor rename_local` (dry_run + apply) | ✅ — 2 occurrences, compile-verified |
| `ghc_refactor extract_binding` | 🐛 #227 |
| `ghc_refactor move_symbol` | 🐛 #228 |
| `ghc_refactor list_actions` | ✅ |

### Property testing (mostly ✅, 2 broken)

| Tool | Result |
|------|--------|
| `ghc_suggest add` | ✅ — Associative(high) + Commutative(medium) |
| `ghc_suggest reverseWords` | ✅ — Idempotent + Involutive(both medium, correct) |
| `ghc_quickcheck` (pass) | ✅ — 100 tests passed, auto-persisted |
| `ghc_quickcheck` (fail) | ✅ — counterexample `" "` for reverseWords involutive |
| `ghc_quickcheck runs=3` | ✅ — flakiness detection |
| `ghc_property_store list` | ✅ |
| `ghc_property_store run` | ✅ — 2/2 pass |
| `ghc_property_store export` | 🟡 #231 — works but generates warnings |
| `ghc_property_store audit` | 🐛 #230 |
| `ghc_witness` | 🐛 #229 — entirely non-functional |

### Advanced tools (✅)

| Tool | Result |
|------|--------|
| `ghc_perf save_baseline` | ✅ — wall-clock harness, mean/median/min/max |
| `ghc_perf compare_baseline` | 🟡 — no explicit comparison output when expression differs from saved baseline |
| `ghc_explain_error` (Phase 1) | ✅ — structured context, imports, enclosing slice |
| `ghc_explain_error` (verify_patch) | ✅ — error_resolved=true, file restored |

### Composites / gates (✅)

| Tool | Result |
|------|--------|
| `ghc_batch` (4 actions, fail_fast=false) | ✅ — 4/4 ok |
| `ghc_batch` (4 actions, 1 fails) | ✅ — partial result, correct |
| `ghc_lab min_confidence=high` | ✅ — discovered multiply associativity, skipped factorial/isPrime correctly |
| `ghc_coverage` | ✅ — 8 HPC metrics, 31% average (expected for minimal test suite) |
| `ghc_gate` | ✅ — regression 4/4 + cabal test PASS + cabal build OK |
| `ghc_check_module` | 🐛 #232 — misses cached warnings |
| `ghc_check_project` | ✅ — 3/3 modules green |
| `ghc_lint` | ✅ — 0 hints on src/ |

---

## Triage

### Priority: high (fix before next release)
- **#228** `move_symbol` export list — feature always fails on realistic code
- **#229** `ghc_witness` empty distribution — feature entirely broken

### Priority: medium (fix for production quality)
- **#226** `ghc_arbitrary` parametric types
- **#227** `ghc_refactor extract_binding` unusable
- **#230** `ghc_property_store audit` probe failure + wrong pre-classification
- **#231** `ghc_property_store export` generates warning-dirty Spec.hs
- **#232** `ghc_check_module` stale-cache warning gap

### Priority: low (UX polish)
- **#224** `ghc_goto` qualified preload remediation
- **#225** `ghc_complete` qualified prefix remediation

---

## What is solid

The architectural foundations held up throughout:
- Snapshot-and-compile-verify on all refactor actions — no data loss
- Module-not-found safety check in `ghc_modules remove`
- In-process GHC API session (fast, no subprocess overhead for basic ops)
- `ghc_hole` — still the gold-standard feature
- `ghc_batch` with `fail_fast=false` — partial results correctly reported
- Property auto-persistence on pass
- `ghc_gate` end-to-end: regression → cabal test → cabal build green

## Branch

`dogfooding/full-tool-audit-2026-05-25`
