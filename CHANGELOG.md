# Changelog

All notable changes to the `haskell-flows` MCP server are documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-10-10

**Stable. Maintenance only from here: bugfixes and GHC-compatibility
work; no feature roadmap.** Windows is not supported.

This entry covers everything that landed on `master` since `0.1.0`.
The intermediate `0.2.0` / `Unreleased` notes were never released;
their surface was superseded by the condensation below (full history
in git).

The headline is an honest, verifiable core: the 36-tool surface
condensed to **12 action-discriminated tools** over **one embedded
ghcide engine**, plus whole-module authorship and a property-first
loop proven by three dogfood runs with mechanical E4 scores
(`docs/dogfood-2026-10-10.md`).

### Changed (BREAKING)

- **ghcide is the only backend.** Every tool routes through one
  embedded ghcide `IdeSession` guarded by an `MVar`. The legacy
  in-process GHC-API engine (`ApiSession`, `HscEnv` management,
  `resetHscEnvInPlace`) is deleted. One session means one truth:
  parser diagnostics are visible to `ghc_check` (they were silently
  dropped before), and eval/scope state cannot disagree with the
  typechecker.
- **Bench harness removed** (`benchmarks/`, the nightly gate, and
  `scripts/bench-mcp.sh`). It targeted the deleted legacy engine and
  no longer compiled; the budget table itself stays in
  `src/HaskellFlows/Bench/Budget.hs`.
- **36 → 12 tools.** The retired verbs live on as `action` fields on
  the consolidated tools; the full successor mapping is in
  `docs/TOOL_TAXONOMY.md` (CI-enforced against the live registry).
  Biggest migrations: `ghc_quickcheck_export` / `ghc_regression` /
  property audit → `ghc_property { action: "export" | "run" | "list" }`;
  `ghc_create_project` / `ghc_switch_project` / `ghc_bootstrap` →
  `ghc_project`; `ghc_toolchain_status` → `ghc_session`;
  `ghc_scratch` → `ghc_edit` snapshot-verify. All nudges, hints and
  bootstrap docs now speak the 12-tool vocabulary.
- **`ghc_project(action="create")`** anchors relative `path`s to the
  server's startup cwd (previously they landed wherever the session
  last chdir'd), and the scaffold's test-suite depends on
  **QuickCheck** — persisted properties work with no extra step.

### Added

- **`ghc_module(action="write_module")`** — whole-module authorship
  with promote discipline: snapshot → write → compile-verify →
  rollback on failure. Accepts a project-relative `.hs` path or a
  dotted module name (`Hm.Parser` → `src/Hm/Parser.hs`).
- **`--doctor`** — toolchain probe table (hard requirements:
  ghc/cabal; optional per feature: hlint/formatter/hoogle) with
  one-line remediation hints, exit 0/1. The bootstrap story lives
  in `docs/requirements.md`.
- **`--version` single-sourced** from the cabal `version:` field via
  the generated `Paths_haskell_flows_mcp` module.

### Fixed

- `ghc_check` with no `action` fell to a stub response; it now runs
  the project check the instructions always promised.
- Parse errors were invisible to `check(module)` / `check(project)`
  (ghcide publishes them under `GetParsedModule`, which we now read).
- `ghc_eval` / `ghc_suggest` reported only "no anchor typed"; they
  now surface the first non-scope anchor error.
- `ghc_project(action="switch"/"close")` could leave the session
  `MVar` empty, blocking every later tool call forever.
- `ghc_modules(action="add")` wrote synthesised `other-modules`
  entries into the previous stanza (cabal#7554 quirk workaround).
- Files that resolve to no cabal component no longer let the project
  gate report green (component honesty).

### Release

- One release flow (`release.yml`): `haskell-flows-mcp` binaries for
  darwin-arm64 and linux-x64, sdist, and sha256 checksums.
- Verification tiers codified in `scripts/gate.sh`
  (t0 build / t1 unit + canary / t2 full e2e, all hangwatch-guarded).

### Known limitations

- Verified end-to-end on darwin-arm64; linux-x64 ships as
  CI-built release binaries. **Windows: not supported.**
- GHC 9.12 is the supported toolchain: the eval-scope machinery
  imports GHC-9.12-only API and does not build on 9.10.
- Single maintainer.
- Type-string parsing remains regex-based; exotic shapes
  (higher-rank polymorphism, type families, GADTs) produce no
  suggestions.


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
