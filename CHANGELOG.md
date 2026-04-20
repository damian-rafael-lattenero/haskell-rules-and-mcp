# Changelog

All notable changes to the `haskell-flows` MCP server are documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Future work is tracked in the plan file (`docs/community-launch/`) and the
[GitHub issues](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues):

- Phase D — upstream-first tool resolution (mirror becomes fallback).
- Phase E — Nix flake for declarative dev shell.
- Phase F — Discourse Haskell announcement.

## [0.1.0] - 2026-04-19

First tagged release for community review. The MCP is technically mature
(1157 tests, deterministic) but marked **experimental** until the community
feedback cycle is closed. What `0.1.0` contains:

### Added

- **Seven property-suggestion engines** with explicit confidence ratings:
  endomorphism, binary-op, list-endomorphism, roundtrip,
  evaluator-preservation, constant-folding-soundness, functor-laws.
- **Property persistence** — every passing QuickCheck property auto-saves to
  `.haskell-flows/properties.json`; `ghci_regression(action="run")` replays
  the whole set; `ghci_quickcheck_export` materializes a runnable
  `test/Spec.hs` with descriptive labels.
- **Consolidated gate** — `ghci_workflow(action="gate")` orchestrates
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

- **`ghci_load` scope semantics** — new `mode: "replace" | "additive"`
  parameter; `additive` uses `:add` instead of `:l` so cross-module
  property tests preserve prior scope.
- **`basic-lint-rules` fallback** reduced to lexically-safe rules
  (trailing whitespace, tabs-in-indentation, partial Prelude functions)
  after false-positives on module headers and nested constructor
  applications were found in real code.
- **Test suite export** — `ghci_quickcheck_export` uses `label` →
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
  validation + CI gate; `ghci_format` degraded fallback;
  `basic-lint-rules` false-positive cleanup; `ghci_load(mode="additive")`;
  `LawEngine` interface + 3 new engines; `label` field on
  `ghci_quickcheck`; `ghci_workflow(action="gate")` aggregator.
- **Fase 3** — upstream fallback URLs, opt-in local telemetry, operator
  runbook for `tools-v1.0`, cross-module browse fix in
  `ghci_suggest(analyze)`, `cabal_coverage` HTML report parser as a
  third fallback.
- **Fase 2** — toolchain warmup, global strict Zod validation via
  `registerStrictTool`, mass migration of ~42 tools, removal of 8
  peripheral tools, `cabal_coverage` tix fallback, publish-release
  script.
- **Fase 1** — tautology law removal in `function-laws`;
  `ghci_check_module` export-list awareness;
  `ghci_toolchain_status` state propagation;
  `ghci_create_project` + `ghci_add_modules` (replacing `ghci_init` +
  `ghci_scaffold`); `ghci_lint` degraded fallback.

[Unreleased]: https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/tag/v0.1.0
