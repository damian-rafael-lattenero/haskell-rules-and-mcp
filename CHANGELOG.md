# Changelog

All notable changes to the `haskell-flows` MCP server are documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

The headline of `0.2.0` (planned) is **a smaller, more orthogonal tool
surface**: 47 wire surfaces collapsed to 31 via six action-discriminated
primitives. Every action that existed in 0.1.0 still exists; the verb
moved into an `action` field instead of a separate tool name.

### Changed (BREAKING — wire surface consolidation, issue #94)

The legacy per-verb tools are **removed outright** (single-internal-consumer
project — no deprecation period). Migrate by inlining the verb into an
`action` field on the consolidated tool. Behaviour is byte-identical: the
new tool's dispatcher forwards to the same handler.

| Before (0.1.0)                                                                | After (`0.2.0`)                                                                  | Issue        |
|-------------------------------------------------------------------------------|----------------------------------------------------------------------------------|--------------|
| `ghc_add_modules` + `ghc_remove_modules`                                      | `ghc_modules { action: "add" \| "remove" }`                                      | #94 Phase B  |
| `ghc_deps_explain`                                                            | `ghc_deps { action: "explain", cabal_output }`                                   | #94 Phase C₁ |
| `ghc_toolchain_status` + `ghc_toolchain_warmup`                               | `ghc_toolchain { action: "status" \| "warmup" }` (defaults to status)            | #94 Phase C₂ |
| `ghc_determinism`                                                             | `ghc_quickcheck { runs: N }` — pass `runs >= 2` for flakiness detection          | #94 Phase C₃ |
| `ghc_move`                                                                    | `ghc_refactor { action: "move_symbol", from, to, symbol }`                       | #94 Phase C₄ |
| `ghc_create_project` + `ghc_switch_project` + `ghc_validate_cabal` + `ghc_bootstrap` | `ghc_project { action: "create" \| "switch" \| "validate" \| "bootstrap" }` | #94 Phase C₅ |
| `ghc_property_lifecycle` + `ghc_regression` + `ghc_quickcheck_export` + `ghc_property_audit` | `ghc_property_store { action: "list" \| "run" \| "export" \| "audit" }` | #94 Phase C₆ |

Tool count: **47 → 31** (16 fewer wire surfaces, six new
action-discriminated primitives). Concept count drops from ~38 to ~22 —
the agent now memorises one tool per verb-cluster rather than four.

The four-category taxonomy in `docs/TOOL_TAXONOMY.md`
(Primitive / Composite / Gate / Control-plane) is now CI-enforced via
`testCategoryCountsMatchTaxonomy` so future drift between the taxonomy
table and the live registry is a compile-error.

### Added

- **Per-tool versioning** (#99 Phases A+B) — every `tools/list` entry
  now publishes a `version` field, and every tool response carries a
  `meta.tool_version`. Bump rules: MAJOR on input/output shape change,
  MINOR on additive fields, PATCH on bug fixes within shape.
- **`--version` / `--help` CLI flags** (#99 Phase A) — the binary can
  be queried at the OS boundary without booting the JSON-RPC loop.
- **Discriminated schemas** (#92) — tools whose per-action required
  fields differ (`ghc_refactor`, `ghc_deps`, the new `ghc_modules` /
  `ghc_project` / `ghc_property_store`) publish a `oneOf` schema that
  tells hosts the *real* contract instead of a flat `required` list
  that lied. A property test in the test suite forbids regressions.
- **`Mcp.Schema` builder module** (#92 Phase A) — `discriminatedSchema`
  + `SchemaBranch` + the field-shape helpers
  (`stringField` / `integerField` / `booleanField` / `arrayField` /
  `constString`) are the canonical way to declare a tool's input shape.
- **Latency-budget scaffold** (#96 Phases A+B) — every tool has a
  `(p50, p95)` budget in `Bench/Budget.hs`; an in-process bench harness
  measures the reference project; CI gate (Phase C, pending) will refuse
  sustained p95 violations.
- **Bench `benchmark` stanza** in the cabal file — `cabal bench` runs
  the suite locally; the methodology lives in `docs/Bench.md`.
- **Tool taxonomy** (#94 Phase A) — `ToolCategory` ADT + `toolCategory`
  total function with exhaustive case; published as a four-bucket table
  in `docs/TOOL_TAXONOMY.md` and CI-locked against drift.
- **Structured JSON-Lines logging** (#98 Phases A+B+C+D) — every tool
  call emits `tool_call_start` / `tool_call_end` events with
  `trace_id`; `--debug-events` adds GHC-session lifecycle events;
  opt-in audit log writes to `.haskell-flows/audit.jsonl`.
- **NextStep golden dispatch snapshot** (#95 Phase C) — 62-row table
  pins the post-success hint for every (tool, payload) combination;
  drift is review-gated.
- **NextStep quality gates D + E** (#95) — `nsWhy` ≥ 10 chars + ends
  with a period; `nsChain` ≤ 4 steps. The agent never sees a
  one-word "why" or a 12-step plan.
- **NextStep dangling-reference detector** (#95 Phase A lite) — every
  recommendation's `tool` field must resolve to a registered tool.
  Catches the bug class where a renamed tool leaves a stale hint.
- **NextStep suppression API** (#95 Phase A) — `suppressIf` /
  `suppressOnZero` / `suppressOnDegraded` make it explicit when a
  successful response should *not* emit a hint.
- **Cross-tool stringification harness** (#91 Phase A) — four post-#88
  migrated tools (`ghc_load`, `ghc_check_module`, `ghc_check_project`,
  `ghc_quickcheck`) get a single test that proves their JSON shape is
  byte-stable across an in-process round-trip.
- **`ghc_perf` / `ghc_lab` / `ghc_property_audit` / `ghc_explain_error`
  / `ghc_witness` Phase 2** (#93) — baseline persistence, vacuous-
  property detection, structured explanation context, distribution
  labelling. Surfaces are additive (MAJOR=1).
- **Cross-tool security harness** (#100 Phases C+D+E) — every tool
  that takes a path runs through the same canonical-path check;
  `SECURITY.md` documents the trust boundary.

### Fixed

- **Schema lied about `ghc_refactor` required fields** (#92 Phase B) —
  pre-fix the schema said `[action]` was required for both
  `rename_local` and `extract_binding`, but the runtime demanded
  more. A host that respected the schema sent plausible-but-rejected
  requests. The new `oneOf` shape mirrors runtime exactly.
- **Same fix for `ghc_deps`** (#92 Phase B continued) — `add` /
  `remove` both require `package` at parse time AND in the published
  schema.
- **Stanza-flag invalidation after dep edit** — `ghc_deps(add|remove)`
  invalidates the cached stanza-flag bootstrap so the next tool call
  rebuilds with the new package set.
- **`ghc_property_audit` Phase 2** — vacuous-property detection
  (precondition is never satisfied) added; UNIQUE-key prefix
  collision in the canonical-form dedupe fixed.
- **NextStep dispatch table is exhaustive** — every `ToolName`
  constructor must be handled; adding a new tool without a
  `nextStep` arm is a compile error.
- **`ghc_check_module` diagnostic filter** — diagnostics from
  unrelated modules in the load batch no longer leak into the
  per-module gate (filter by `geFile` suffix).

### Security

- **Path-traversal property fuzzer** (#100 Phase A) — QuickCheck
  generator against `mkModulePath`'s smart-constructor invariant.
- **Symlink-escape canary test** (#100 Phase B) — confirms
  `mkModulePath` rejects symlinks that resolve outside the project
  root.
- **Cross-tool canonical check** (#100 Phases C-E) — every
  path-accepting tool routes through the same guard; documented
  in `SECURITY.md`.

### Docs

- **`docs/TOOL_TAXONOMY.md`** — four-category breakdown of the live
  tool registry, CI-enforced.
- **`docs/Bench.md`** (#96) — measurement methodology + how to
  interpret `(p50, p95)` budgets.
- **`docs/concurrency.md`** (#97 Phase A) — explicit contract for
  the property store + GHCi session under concurrent calls.
- **`docs/binary-size.md`** (#101 Phase A) — baseline measurement;
  Phase B will add `strip` + `-split-sections` to reduce
  the 199 MB → ~135 MB.

### Known limitations

(unchanged from `0.1.0`)

- **Platform reality** — only `darwin-arm64` is verified end-to-end.
  Other platforms fall through to host PATH.
- **Single maintainer** — bus factor of 1.
- **Advanced types** — regex-based type-string parsing; tail shapes
  (higher-rank polymorphism, type families, GADTs) silently produce
  zero suggestions.

### Pending (tracked in [GitHub issues](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues))

- Phase D — upstream-first tool resolution (mirror becomes fallback).
- Phase E — Nix flake for declarative dev shell.
- Phase F — Discourse Haskell announcement.
- #97 Phase B — file-locking the property store.
- #101 Phase B — `strip` + `-split-sections` in `install-mcp.sh`.
- #96 Phase C — wire bench into CI gate.

## [0.1.0] - 2026-04-19

First tagged release for community review. The MCP is technically mature
(1157 tests, deterministic) but marked **experimental** until the community
feedback cycle is closed. What `0.1.0` contains:

### Added

- **Seven property-suggestion engines** with explicit confidence ratings:
  endomorphism, binary-op, list-endomorphism, roundtrip,
  evaluator-preservation, constant-folding-soundness, functor-laws.
- **Property persistence** — every passing QuickCheck property auto-saves to
  `.haskell-flows/properties.json`; `ghc_regression(action="run")` replays
  the whole set; `ghc_quickcheck_export` materializes a runnable
  `test/Spec.hs` with descriptive labels.
- **Consolidated gate** — `ghc_workflow(action="gate")` orchestrates
  regression + cabal_test + cabal_build in a single call with per-step
  durations and partial-success semantics.
- **Path-traversal guard** — `HaskellFlows.Types.mkModulePath` smart
  constructor centralises the traversal check; rejects `../etc/passwd`
  and symlink escapes.
- **Manifest consistency invariant** — new unit test refuses any diff that
  lets `releases[].sha256` and `tools[].sha256` drift for the same binary.

### Fixed

- **Orphan `.download` files** on checksum mismatch during auto-download are
  cleaned up before the next attempt.
- **Property store corruption recovery** — `loadStore` now distinguishes
  ENOENT (fresh project) from parse / schema errors. Corrupt files are
  quarantined as `.corrupt-<ts>` instead of silently wiped.
- **Fetch timeout** — `AbortController` with a 5-minute cap prevents a
  hung CDN from wedging the MCP session indefinitely.
- **Concurrent download race** — `IN_FLIGHT: Map<destPath, Promise>` makes
  parallel callers share the same download; eliminates partial-write
  corruption under parallel tool calls.
- **GHCi health detection** — unexpected process exit now marks the
  session as `corrupted`; subsequent tool calls fail fast with an
  actionable "call restart()" message instead of hanging.
- **Arbitrary auto-detect** — after a successful load, `:info Arbitrary`
  is probed; the workflow guidance no longer falsely reports "No
  Arbitrary instances" when the module literally defines them.
- **Ambiguity hint** — `parseAmbiguousTypeVariable` detects the GHC
  error produced when a property's return type cannot be inferred under
  `-fdefer-type-errors` and attaches a concrete annotation suggestion
  on BOTH the incremental and non-incremental response paths.
- **Cross-module suggest** — `handleAnalyze` loads the whole project via
  `loadModules(paths, names)` before the sibling probe, so
  `evaluator-preservation` engine fires on real projects (it previously
  unit-tested fine but never triggered in production flows).
- **Manifest SHA256 drift** — `releases[].sha256` and `tools[].sha256`
  now agree for every bundled binary; invariant test catches future drift.

### Changed

- **`ghc_load` scope semantics** — new `mode: "replace" | "additive"`
  parameter; `additive` uses `:add` instead of `:l` so cross-module
  property tests preserve prior scope.
- **`basic-lint-rules` fallback** reduced to lexically-safe rules
  (trailing whitespace, tabs-in-indentation, partial Prelude functions)
  after false-positives on module headers and nested constructor
  applications were found in real code.
- **Test suite export** — `ghc_quickcheck_export` uses `label` →
  `law` → `functionName` → `property_N` priority; sanitizes unsafe
  characters; appends `_2`/`_3` on collision.
- **Manifest env override** — new `HASKELL_FLOWS_MANIFEST_PATH` env var
  lets e2e tests point the subprocess at an alternate manifest without
  editing the checked-in file, eliminating cross-process download races.

### Security

- **Path traversal** (CWE-22) — `resolveModulePath` rejects escapes before
  any filesystem access.
- **Restart loop DoS** (CWE-400) — `mcp_reload_code` is staleness-gated
  and rate-limited; no recursive self-restart possible.
- **Insufficient data verification** (CWE-345) — manifest consistency
  test prevents silent SHA256 drift between the two manifest sections.
- **Supply-chain trust** — bundled binaries are mirrors of upstream (hlint,
  fourmolu, ormolu, hls); SHA256 pinned in the manifest. Phase D will move
  upstream to primary and the mirror to fallback.

### Known limitations

- **Platform reality** — only `darwin-arm64` has bundled tool binaries
  verified end-to-end. Other platforms fall through to host PATH.
- **Single maintainer** — bus factor of 1. `CONTRIBUTING.md` invites help.
- **Advanced types** — suggest engines parse type strings with regex; tail
  shapes (higher-rank polymorphism, type families, GADTs) produce zero
  suggestions silently.

---

## Historical — pre-`0.1.0` phases (for reference)

The `0.1.0` release consolidates six phases of pre-public development.
Summaries kept for traceability; full commit history is in git.

- **Fase 6.1** — manifest sha256 drift fix + 3 invariant tests (+9 unit).
- **Fase 6** — `mcp_reload_code` hot-reload tool; deterministic e2e via
  `HASKELL_FLOWS_MANIFEST_PATH` env override (+9 unit, +4 e2e).
- **Fase 5.1** — `handleAnalyze` now loads the project before the
  cross-module probe; `_ambiguityHint` attached at the pre-flight
  typecheck early return (+9 unit).
- **Fase 5** — 5 robustness bugs (auto-download orphan cleanup, fetch
  timeout, concurrent download lock, property-store corruption recovery,
  ghci-session health detection) + 3 ergonomic observations + 2
  refactors (+24 unit).
- **Fase 4 (hyper-stabilization)** — centralized release manifest; URL
  validation + CI gate; `ghc_format` degraded fallback;
  `basic-lint-rules` false-positive cleanup; `ghc_load(mode="additive")`;
  `LawEngine` interface + 3 new engines; `label` field on
  `ghc_quickcheck`; `ghc_workflow(action="gate")` aggregator.
- **Fase 3** — upstream fallback URLs, opt-in local telemetry, operator
  runbook for `tools-v1.0`, cross-module browse fix in
  `ghc_suggest(analyze)`, `cabal_coverage` HTML report parser as a
  third fallback.
- **Fase 2** — toolchain warmup, global strict Zod validation via
  `registerStrictTool`, mass migration of ~42 tools, removal of 8
  peripheral tools, `cabal_coverage` tix fallback, publish-release
  script.
- **Fase 1** — tautology law removal in `function-laws`;
  `ghc_check_module` export-list awareness;
  `ghc_toolchain_status` state propagation;
  `ghc_create_project` + `ghc_add_modules` (replacing `ghc_init` +
  `ghc_scaffold`); `ghc_lint` degraded fallback.

[Unreleased]: https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/tag/v0.1.0
