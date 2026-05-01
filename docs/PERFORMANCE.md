# haskell-flows-mcp — Performance Methodology

> Issue [#96](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/96) Phase B.  Documents how per-tool latency budgets
> are measured + interpreted, and what the *informational* `cabal run
> haskell-flows-mcp-bench` output tells you about the binary's
> performance posture.

---

## TL;DR

* Every tool has a budget in `HaskellFlows.Bench.Budget` (`mcp-server-haskell/src/HaskellFlows/Bench/Budget.hs`).
* Phase A landed *proposed* budgets calibrated against the
  subprocess-over-stdio transport.
* Phase B (this doc) ships a real measurement harness in
  `mcp-server-haskell/benchmarks/Main.hs` that exercises a
  representative subset (currently 13 tools) against
  `benchmarks/Reference/`.
* Phase B is **informational**.  No CI gate fires today — the bench
  emits `OK` / `WARN` rows; only Phase C will turn the WARN into a
  failed CI step.

---

## How the bench works

```text
┌──────────────────────┐          ┌──────────────────────┐
│   benchmarks/Main.hs │ ──────►  │  in-process Server   │
│   (timing harness)   │  N=10    │  (HaskellFlows.Mcp.* │
│                      │  per     │   .Server)           │
│   computeStats       │  tool    │                      │
│   (p50 / p95)        │          │   handleRequest      │
└──────────────────────┘          └──────────────────────┘
        │
        ▼
  one row / tool:
   OK / WARN  toolname  p50  p95  mean  budget
```

1. Resolve `benchmarks/Reference/` (the canonical test project) and
   copy it to a hermetic temp directory so the bench does not
   pollute the source tree's `.haskell-flows/` or `dist-newstyle/`.
2. Boot one in-process `Server` anchored on the temp copy.
3. **Warm-up call** — one `ghc_workflow status` to pay the boot tax
   before measurements start.
4. For each tool in the bench subset (`benchSubset`), send `N=10`
   `tools/call` requests through `handleRequest` (the same code path
   the stdio transport uses internally).
5. Time each request via `getPOSIXTime`; *discard the first sample*
   (cold-start tax has its own budget).
6. Compute p50 / p95 / mean / stdDev on the warm samples.
7. Compare each measured p95 against `tbP95Ms` from `Budget.hs`;
   tag the row `WARN` on breach.

The full raw output also includes the structured `tool_call_start /
tool_call_end` JSON-Lines events (per [#98](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/98) Phase B), so a single
bench run produces both human-readable rows and machine-parseable
trace events.

---

## In-process vs subprocess — what the numbers mean

The harness uses the **in-process** `Server.handleRequest` path —
no pipe, no JSON serialization through stdio, no fork/exec.  This is
deliberate: [#96](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/96)'s goal is *regression detection on tool-internal
work*, not whole-system wire latency.

Concretely, the bench measures:

| Counted | Excluded |
|---|---|
| GHCi boot for the loaded module | Process spawn (fork + exec) |
| Per-call dispatch + arg validation | JSON-RPC framing on stdio |
| `withGhcSession` MVar acquisition | OS pipe scheduling latency |
| Tool-specific work (parsing, refactor, etc.) | Subprocess fork-bomb mitigations |
| Property-store read/write | Inter-process round-trip ack |

The **in-process numbers are a lower bound** on the wire latency
the JSON-RPC client observes.  Real-world IPC overhead on macOS arm64
adds roughly 1–5 ms per round-trip on a warm stdio pipe (negligible
for a `ghc_check_project` that takes 500 ms; meaningful for a
`ghc_workflow status` that the bench measures at 0–1 ms).

Phase B's contract: when an *internal* regression makes a tool
slower, the bench detects it.  When the *transport* changes (e.g. a
new envelope wrapper in [#90](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/90)), the bench will not flag it — Phase D
of [#91](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/91) (subprocess harness with full wire-protocol coverage)
catches that class of regression.

---

## Phase B baseline (current measurements)

Run on macOS arm64, M1, ghc-9.12.2, optimised build (`-O1`),
hermetic temp project copy.  N=10 warm samples, first sample
discarded.

| Tool | Measured p50 | Measured p95 | Budget p95 | Headroom |
|---|---:|---:|---:|---:|
| `ghc_workflow` (status)         |    0 ms |    1 ms |   200 ms | 200× |
| `ghc_toolchain_status`          |  129 ms |  184 ms |   300 ms | 1.6× |
| `ghc_validate_cabal`            |   25 ms |   50 ms |   500 ms | 10× |
| `ghc_load` (warm, in-process)   |   68 ms |   77 ms |   800 ms | 10× |
| `ghc_type`                      |    1 ms |    2 ms |   200 ms | 100× |
| `ghc_info`                      |    1 ms |    2 ms |   300 ms | 150× |
| `ghc_eval`                      |   17 ms |   80 ms |   500 ms | 6× |
| `ghc_imports`                   |    1 ms |    1 ms |   200 ms | 200× |
| `ghc_browse`                    |    1 ms |    1 ms |   300 ms | 300× |
| `ghc_complete`                  |    1 ms |    2 ms |   200 ms | 100× |
| `ghc_goto`                      |    1 ms |    1 ms |   200 ms | 200× |
| `ghc_suggest`                   |   16 ms |   56 ms |   400 ms | 7× |
| `ghc_check_module`              |   13 ms |   52 ms | 1 500 ms | 30× |

**Observation:** every measured tool has *substantial* headroom on
its budget today (the largest p95 measurement uses 62 % of the
budget; most use < 10 %).  This is consistent with the design intent:
budgets are calibrated for the slow path (subprocess transport, large
projects, cold caches); the warm in-process path is much faster.

The wide headroom is a feature, not a bug — it means a 5–10× slowdown
is detectable as a meaningful regression without fighting noise on
tool calls that already run in ~1 ms.

---

## How to run the bench

```bash
# Repo root (pick up cabal.project + benchmarks/Reference)
cd ~/Personal-Projects/haskell-rules-and-mcp/mcp-server-haskell
cabal run haskell-flows-mcp-bench

# Or via the helper script (sets PATH for ghcup-installed cabal):
scripts/bench-mcp.sh
```

Sample output (truncated — full run shows all 13 tools):

```text
==================================================================
haskell-flows-mcp-bench — Phase B (#96)
Per-tool latency measurement against benchmarks/Reference/
==================================================================

Reference project (hermetic copy): /tmp/hflows-bench-XXXXXX/Reference

Warm-up (ghc_workflow status — boot probe)…

Running benchmark subset (N=10 per tool, first sample discarded):
------------------------------------------------------------------
  OK   ghc_workflow             p50=   0ms p95=    1ms mean=   0ms — budget p50=50 p95=200
  OK   ghc_toolchain_status     p50= 129ms p95=  184ms mean= 135ms — budget p50=100 p95=300
  …
  OK   ghc_check_module         p50=  13ms p95=   52ms mean=  17ms — budget p50=500 p95=1500
------------------------------------------------------------------

All measured tools within budget.
Phase B is informational; the gate (Phase C) is not yet wired into CI.
```

---

## Adding tools to the bench subset

`benchmarks/Main.hs::benchSubset` is the canonical list of tools
exercised on every run.  To extend it:

1. Add a `(ToolName, Value)` row with the canonical args payload.
2. Make sure the args resolve against `benchmarks/Reference/`
   without setup.  Tools that need a dependency (e.g. an `import`
   that's not yet present) should either ship a fixture in
   `benchmarks/Reference/src/` or land their own scenario.
3. Re-run `cabal run haskell-flows-mcp-bench` and confirm the tool
   shows up in the output table.

The bench subset is intentionally small (currently 13 of 46 tools)
to keep the inner loop fast.  The full sweep is Phase D's nightly
job.

---

## Updating budgets

When a tool's behaviour legitimately changes — e.g. [#93](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/93) added a
constructor-classify pass to `ghc_witness` that's intrinsically
slower — the budget needs to be bumped.  The protocol:

1. Run `cabal run haskell-flows-mcp-bench`.  Note the new p50 / p95.
2. Edit `mcp-server-haskell/src/HaskellFlows/Bench/Budget.hs`.
3. Set the new p50 to `max(measured_p95, OLD_p50)` (the budget should
   still be a reasonable headroom over the median).
4. Set the new p95 to `max(measured_p95 * 1.5, OLD_p95)` (a 50 %
   safety margin over the new measurement absorbs noise).
5. Update the `tbNotes` field with the rationale + issue number.
6. Re-run the bench; confirm the row reports `OK`.
7. Document the bump in the PR description.

Phase C will additionally require updating `CHANGELOG.md` (per
issue [#99](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/99) Phase D) when a budget moves.

---

## Roadmap

| Phase | Scope | Status |
|---|---|---|
| A | Budget table + pure timing helpers + reference project scaffold | ✅ landed |
| **B** | **In-process measurement harness + this doc + initial baseline** | **✅ landed** |
| C | Wire the bench into a fast-subset CI gate (PR-blocking on >20% p50 / >30% p95 breach) | open |
| D | Nightly full-bench (all 46 tools) + auto-issue on breach | open |
| E | Per-test latency assertions (`assertWithinBudget` helper) | open (optional) |

---

*Last updated: Phase B landing.  Re-run the bench against any newer
build to confirm the regression / improvement story; numbers in this
doc are a point-in-time snapshot of `master`.*
