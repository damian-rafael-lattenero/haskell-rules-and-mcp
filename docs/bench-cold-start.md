# Cold-start benchmark — GhcSession (Phase-2 in-process) vs subprocess ghci

Measured on master @ a287d95 · GHC 9.12.2 · cabal 3.14.2.0 · macOS aarch64.

## Method

Each sample spawns a fresh `haskell-flows-mcp` process, sends `initialize`
+ a single tool call over stdio, and times wall-clock from spawn to the
first `id=2` response. Project: `/tmp/bench-project` — a 1-module library
with `base` as its only dep. 3 trials per tool, after one global warmup
call so cabal planning + package-db caches are resident.

Benchmark script: [`bench/bench-cold-start.py`](../bench/bench-cold-start.py).

## Results (best / avg / worst, ms)

| Tool                   | Backend                         | best | avg  | worst |
|------------------------|---------------------------------|-----:|-----:|------:|
| `ghci_type`            | GhcSession (Phase 2, in-process)|   42 |   43 |    43 |
| `ghci_complete`        | GhcSession                      |   36 |   36 |    37 |
| `ghci_imports`         | GhcSession                      |   36 |   36 |    36 |
| `ghci_goto`            | GhcSession                      |   36 |   37 |    38 |
| `ghci_eval`            | subprocess ghci (legacy)        | 2925 | 3059 |  3147 |
| `ghci_quickcheck`      | subprocess ghci (legacy)        | 2944 | 2971 |  3006 |

Cold-cold (truly first server after power-on, no warmup): ~390 ms for
`ghci_type`. Subsequent cold invocations hit the cabal/plan/db cache.

## Interpretation

* **~80× speedup** on the Phase-2 tool surface. The original plan
  targeted `<1 s` cold start; every Phase-2 tool clears that bar by
  20×.
* The subprocess path's ~3 s is `cabal repl` spin-up: resolving the
  plan, loading `base`, initialising the ghci prompt, sending the
  first `:l` init script. Not dominated by compilation — the project
  has 1 trivial module.
* Phase-2 tools complete in ~37 ms because GhcSession boot reuses the
  already-loaded `ghc` library inside the MCP process; only the
  HscEnv construction + first compile (~350 ms) runs on warmup, and
  auto-load caches it via `gsLoadedRef` for the session lifetime.

## Implications for Phase 4+

Migrating `ghci_eval` in-process (Phase 4 of
[GHC-API-rewrite-plan.md](./GHC-API-rewrite-plan.md)) would shift eval
from the ~3 s legacy path to the ~40 ms GhcSession path — the same
80× win observed here. Same for `ghci_quickcheck` if/when the dual
path is ever retired.

Phase 7's parallelism goal is also gated on these numbers: with
Phase-2 tools already under 50 ms, parallel tool-call fan-out from an
LLM agent becomes a real capability — the serialisation lock on
legacy Session is the remaining blocker, and it only engages for
eval / quickcheck / regression / determinism now.

## Caveats

* macOS aarch64 only. Linux numbers TBD (CI runners).
* Single-module project. Larger projects raise the 390 ms warmup
  proportionally to compile time, but not the post-warmup ~40 ms
  per-tool latency.
* `ghci_type` in this bench queries `map` against the session's
  default Prelude context. Queries that reference project-local
  bindings pay a one-time setContext cost on first call.
