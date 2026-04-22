# Refactor `startSession` to use the GHC API via hie-bios

Status: **infrastructure landed, default still sequential**. Parallel
mode is wired (`HASKELL_FLOWS_E2E_PARALLEL=N`) but fails for N≥2
because many scenarios still drive the legacy subprocess `Session`
(ghci_load, ghci_refactor, ghci_quickcheck, ghci_check_module, …)
and parallel `cabal repl` spawns contend on `~/.cabal/store`.

Estimated effort to fully unlock parallel default: migrate every
compile-verify path (ghci_load/check_module/refactor) fully off
`Session`, which requires Phase 4+ of the GHC-API rewrite plan to
be done (currently ghci_load is hybrid with Session-authoritative).
Owner: unassigned.

## Context

The e2e test suite (`mcp-server-haskell/test-e2e/`) currently
supports an opt-in parallel mode via `HASKELL_FLOWS_E2E_PARALLEL=N`
that never landed. N=2 was flaky; N≥3 failed hard with a mix of
`posix_spawnp: does not exist`, fake-green 30 ms `ghci_load`
responses, and `SessionExhausted` on already-booted sessions.

After investigation (documented in the git log of master's
`docs/testing.md`), the root cause is architectural, not a bug
we can patch: **we spawn `cabal repl` per scenario**, and
cabal-install upstream is not designed for 3+ concurrent
`cabal repl` children over `~/.cabal/{store,packages}`.

The dev-loop speedup we actually have today is
`HASKELL_FLOWS_E2E_SKIP_SLOW=1` → 222 s → 127 s serial. Good enough
for iteration, but the full suite is still ~3.5 min on CI.

## Current state (Phase 7, infrastructure step)

`test-e2e/Main.hs` now reads `HASKELL_FLOWS_E2E_PARALLEL=N` and
partitions scenarios into fast and slow buckets. Fast scenarios run
through a QSem-bounded pool of width N; slow scenarios (`isSlow` in
the scenario list) stay sequential regardless. Default N=1 preserves
the exact pre-Phase-7 behaviour, so `cabal test haskell-flows-mcp-e2e`
stays green. N≥2 partially works — some fast scenarios fail under
load because their tool calls still route through `Session`'s
`cabal repl`. Stress testing with N=4 × 10 consecutive runs (the
plan's global acceptance criterion) is blocked until more tools
migrate off the subprocess path.

## Why the current approach is fundamentally limited

Every `Scenarios.Flow*` scenario calls `ghci_create_project` +
drives the MCP. The MCP's `Session` layer (`startSession` in
`src/HaskellFlows/Ghci/Session.hs`) spawns:

```haskell
proc "cabal" ["repl", "--build-depends", "QuickCheck"]
```

Each of those processes:

1. Reads and parses `cabal.project` + the per-scenario `.cabal`.
2. Runs the solver against Hackage with the extra `--build-depends`
   constraint — regenerates `dist-newstyle/cache/plan.json`.
3. Touches `~/.cabal/store/ghc-X.Y.Z/package.db` for the link step.
4. Takes an advisory flock on `dist-newstyle/cache`.
5. Starts GHCi under the resolved environment.

Steps 2–4 involve cross-process state. Under concurrency:

* **Step 2** is the biggest cost (~100–500 ms) and touches the
  Hackage index file. Two cabals reading during a refresh race.
* **Step 3** has a file lock that occasionally returns
  `resource busy` under contention.
* **Step 4** caused the `posix_spawnp: does not exist` failures
  in experiments (kernel-level ENOENT race on the fork).

There is no patch at the MCP layer that makes this safe. Proof by
reference: **no major Haskell project runs concurrent `cabal repl`
over distinct projects in its test suite.** Not HLS, not pandoc,
not cabal-install itself, not stack. The pattern does not exist
upstream because cabal doesn't support it.

## The proposed solution

Bypass `cabal repl` entirely in `startSession`. Use the **GHC API
as a library** (via `hie-bios` for cradle resolution) to run GHC
in-process, the same way HLS, ghcid, and every modern IDE-like
tool does.

### Reference implementations

* **haskell-language-server** —
  <https://github.com/haskell/haskell-language-server>. Uses
  `Development.IDE.Session` which in turn uses hie-bios to
  resolve `cradle(cabal) {}` → per-file GHC flags, then imports
  `GHC` as a library. HLS runs N concurrent GHC sessions for N
  open files without filesystem contention because the GHC
  runtime is a library, not a subprocess.
* **ghcid** (modern) — <https://github.com/ndmitchell/ghcid>.
  Supports a `--command` override but the stable path is hie-bios
  resolution + GHCi launched with pre-resolved flags.
* **hie-bios** — <https://github.com/haskell/hie-bios>. The
  library that encapsulates "how do I start GHC for this file?".
  Handles cabal, stack, direct, and bios cradles. Widely
  adopted; the abstraction the ecosystem has standardised on.

### What the refactor looks like

1. Add `hie-bios` as a dependency of `haskell-flows-mcp`.
2. Replace `startSession` to:
   a. Call `HIE.Bios.loadCradle projectDir` → `Cradle`.
   b. Call `HIE.Bios.getCompilerOptions (unProjectDir pd) cradle` →
      `ComponentOptions` (package-db path + module search paths +
      GHC flags).
   c. Spawn `ghci` directly with those flags (or import `GHC` and
      run an in-process interpreter — decide which gives better
      trade-off). Do NOT invoke `cabal repl`.
   d. Run the init script over that GHCi; same sentinel framing.
3. Keep the existing `Session`/`SessionStatus`/`executeNoLock`
   infrastructure unchanged. Only the boot path changes.
4. Drop `sessionCabalArgs`; the `--build-depends QuickCheck`
   injection was the biggest contention source (forces re-planning
   every spawn). Users who need QuickCheck add it to their `.cabal`
   (which we already support via `ghci_deps`) OR install it to the
   user env once via `cabal install --lib QuickCheck`.

### Expected outcome

* `HASKELL_FLOWS_E2E_PARALLEL=N` becomes usable for any N up to
  machine cores (each session is an independent GHCi child with
  its own heap, no cross-process cabal contention).
* Expected full-suite wall time: ~200 s / N. On a 4-core laptop,
  N=4 → ~50 s. On CI (typically 2 cores), N=2 → ~100 s.
* Side benefits: `startSession` gets faster in serial too
  (bypasses cabal's plan step, which is a 300–500 ms overhead
  per spawn). Smaller memory footprint (no cabal child kept
  resident).

## Implementation plan

Roughly ordered by dependency:

1. **Spike: prove hie-bios resolves our scaffolded projects.**
   In a throwaway branch, write a 30-line program that calls
   `HIE.Bios.loadCradle` on a tempdir created by
   `ghci_create_project`, prints the resulting
   `ComponentOptions`, and exits. Verify the options include
   what we need (a `-package-db` path, `-hide-all-packages`,
   module search paths). ~2 hours.

2. **New `startSession` (in-process-GHCi, GHC-API variant).**
   Likely the bigger-ROI path, but also the bigger change. Import
   `GHC` qualified, wire up a session with `runGhc` +
   `setSessionDynFlags`. Tie it into the existing sentinel
   framing. Preserve `Session`/`killSession`/`executeNoLock`
   signatures so tools stay unchanged. ~1 day.

3. **OR — new `startSession` (ghci-subprocess-with-explicit-flags
   variant).** Cheaper path: still use subprocess GHCi but pass
   the hie-bios-resolved flags directly, no `cabal repl`
   wrapper. Removes the cabal-contention source without moving
   to in-process GHC. ~4 hours. Good fallback if the GHC-API
   variant hits unforeseen issues (loading user code's TH
   requires more ceremony in-process than subprocess).

4. **Update `sessionCabalArgs` / `sessionArgs` naming** to
   reflect the new reality. Remove `--build-depends QuickCheck`.

5. **Update `test-e2e/Main.hs`** to default
   `HASKELL_FLOWS_E2E_PARALLEL` to a reasonable number (e.g. 4)
   and lift the "flaky" warning.

6. **Update `docs/testing.md`** — the "Running scenarios in
   parallel" section currently explains why N>2 doesn't work.
   Replace with a short "how many threads to use" guide.

## Acceptance criteria

* `cabal test haskell-flows-mcp-e2e` with `HASKELL_FLOWS_E2E_PARALLEL=4`
  passes 100% green across 10 consecutive runs on macOS-arm64
  and Linux-x86_64. (Today it fails deterministically.)
* Serial run (`HASKELL_FLOWS_E2E_PARALLEL=1`, full suite, no
  skip-slow) is ≤ 180 s (today: ~222 s).
* All 216 existing scenario checks still pass. Zero regression.
* HLint clean.
* `scripts/ci-local.sh --fast` green.

## Risks and mitigations

* **TemplateHaskell support in-process.** GHC-API sessions
  handle TH, but the setup is subtler than with the subprocess
  path. Mitigation: use option (3) above (subprocess GHCi with
  hie-bios-resolved flags) if TH turns out to be fiddly.
* **hie-bios API churn.** The library has had breaking changes
  between versions. Mitigation: pin with a tight version bound,
  review the release notes before bumping.
* **`--build-depends QuickCheck` users.** Today every MCP user
  implicitly gets QuickCheck in scope. Removing that breaks
  `ghci_quickcheck` on projects that don't have QC in their
  cabal. Mitigation: either auto-detect + suggest `ghci_deps add
  QuickCheck` on first `ghci_quickcheck` call, or inject QC
  transparently into the cradle's package DB at boot.

## Why this is worth doing (beyond test speedup)

The paralelism gap is a symptom. The real value is:

* **Faster cold start** — cabal's resolver is 300–500 ms dead
  weight on every `startSession`. For agents that spawn fresh
  sessions per tool call (not our default, but possible), this
  adds up fast.
* **Smaller footprint** — no cabal child resident; just one GHCi.
* **Better error surfaces** — hie-bios exposes cradle-resolution
  errors structurally (no package matching `XYZ`, etc.), which
  we can surface to the agent as first-class diagnostics instead
  of parsing cabal's stderr.
* **Multi-project support** — hie-bios natively handles stack
  projects and direct projects, not just cabal. Opens the door
  to MCP users who don't use cabal.

## Not doing for now

* **Per-scenario `--store-dir` isolation.** Tested. Losses far
  exceed gains — each scenario recompiles QuickCheck from
  scratch (~30 s) because the scenario-local store is cold.
* **Pre-warming `~/.cabal/store` in Main.hs.** Tested. The
  store is already warm after `cabal test haskell-flows-mcp-e2e`
  compiles the lib — pre-warming adds no value, contention is
  elsewhere.
* **Raising `executeNoLock` timeouts globally.** Masks some
  symptoms; doesn't help the ones where the cabal child actually
  deadlocked on a store lock.

All three are recorded here so the next person doesn't waste
time re-exploring.
