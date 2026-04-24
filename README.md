<div align="center">

# haskell-flows

**An agent-first MCP server for property-driven Haskell development.**

[![Haskell CI](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml)
[![Nix flake](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)
[![Unit tests](https://img.shields.io/badge/unit%20tests-240%20passing-brightgreen)](mcp-server-haskell/test/Spec.hs)
[![E2E scenarios](https://img.shields.io/badge/e2e%20scenarios-37-brightgreen)](mcp-server-haskell/test-e2e/Scenarios)
[![MCP tools](https://img.shields.io/badge/MCP%20tools-39-blue)](mcp-server-haskell/src/HaskellFlows/Tool)
[![GHC](https://img.shields.io/badge/GHC-9.10%20%7C%209.12-8a5aa0)](https://www.haskell.org/ghc/)
[![In-process GHC API](https://img.shields.io/badge/runtime-in--process%20GHC%20API-orange)](mcp-server-haskell/src/HaskellFlows/Ghc/ApiSession.hs)

</div>

---

## TL;DR (30 seconds)

`haskell-flows` is an MCP server that lets **AI agents** (Claude Code, Cursor, any MCP client) drive a **property-first Haskell workflow** end-to-end: scaffold a project, synthesise `Arbitrary` instances, propose QuickCheck laws from a module's type shape, run them, stress them for flakiness, persist passing properties into a regression suite, and collapse pre-commit gates into a single call.

Every tool routes through a **single in-process GHC API session** — no subprocess GHCi, no chatty `:command` capture, no framing fragility. Compile-verification is first-class; every gate is honest.

```text
ghc_create_project  →  ghc_deps / ghc_add_modules
  →  ghc_load        →  ghc_suggest(function_name=…)
  →  ghc_quickcheck  →  ghc_determinism       ← catches flakiness QuickCheck alone misses
  →  ghc_regression  →  ghc_quickcheck_export ← materialises CI-runnable test/Spec.hs
  →  ghc_gate                                 ← pre-push finalizer: regression + cabal test + cabal build
```

See [`docs/flows.md`](docs/flows.md) for rendered Mermaid diagrams of the four central flows: property-first dev loop, project bootstrap, refactor snapshot-and-compile, and session lifecycle.

**Install:** [`docs/install.md`](docs/install.md) — three methods (release binary / from source / Nix flake), plus MCP client wiring for Claude Code, Cursor, etc.

---

## Why this exists

The gap between "LLMs generate Haskell" and "LLMs generate **correct** Haskell" is the gap between plausible syntax and **provable behaviour**. `haskell-flows` closes it with three ideas:

1. **Property-first loop as a first-class workflow.** The agent doesn't just write code; it gets its own function's signature analysed, receives proposed laws with confidence scores, runs them, stresses them for flakiness, and watches them enter a persistent regression suite. The loop is short enough to fit inside a single agent turn.

2. **Semantic `ghc_suggest`, not grep.** Given `simplify :: Expr -> Expr` next to an `eval :: Env -> Expr -> r` in the same module, `ghc_suggest` proposes **evaluator preservation** and **constant-folding soundness** at high confidence — and explicitly *discourages* "involutive" with the rationale that normalisers are idempotent, not involutions. The engines cross-reference sibling top-level bindings to figure out the right law, not a pattern-match on the name.

3. **In-process GHC API, not a subprocess REPL.** Every tool calls `runGhc` / `compileExpr` / `exprType` inside the MCP server's own HscEnv. No pipe framing, no stdout drain, no prompt parsing. Diagnostics come from a `LogAction` hook — structured on emit, not parsed after the fact.

**A concrete example from dogfood** — building a 5-module arithmetic-expression evaluator, the first draft of `simplify` had an *annihilation rewrite* (`Mul 0 x ⇒ Lit 0`). `ghc_quickcheck` passed it at 100/100 runs. `ghc_determinism` at 5 runs caught the bug: `Mul (Lit 0) (Var unbound)` evaluates to `Left (UnboundVar …)` in the original but to `Right 0` after simplification — the rewrite swallowed an error. Without determinism, that broken simplifier would have shipped marked green. **That 5-second extra gate is the difference between "it passed the tests" and "the property really holds".**

---

## What it actually does (2-min scan)

### 1 · Scaffold
One call — `ghc_create_project` — drops a cabal project in place: `<name>.cabal`, `cabal.project`, `src/<Module>.hs` per module, `test/Spec.hs`. The in-process GHC session warms on first use; no manual "start GHCi" step.

### 2 · Suggest properties from type signatures
Load a module with `simplify :: Expr -> Expr` next to `eval :: Env -> Expr -> r`. `ghc_suggest(function_name="simplify")` proposes:

```haskell
-- constant-folding soundness · confidence: high
\env x -> eval env (simplify x) == eval env x
```

with full rationale. **Eight engines** ship today: endomorphism (idempotent / involutive), binary-op (associative / commutative / identity), list-endomorphism, roundtrip, evaluator-preservation, constant-folding soundness, functor laws, and a sibling scanner that walks the module graph to find inverse / interpreter partners. Low-confidence laws come with explicit *"this probably fails because…"* rationale — the engine pushes back when the name hints at a normaliser and the caller asked for "involutive".

### 3 · Run + stress + persist
- `ghc_quickcheck` runs a property; passes auto-persist to `.haskell-flows/properties.json`.
- `ghc_determinism` re-runs N times (default 3, configurable) to catch flakiness **before** the property enters the regression set. This is the tool that separates "passed 100 runs" from "actually holds".
- `ghc_regression(action="run")` replays the persisted set after any edit.
- `ghc_quickcheck_export` materialises the persisted set as a committable `test/Spec.hs` — `cabal test` replays it in CI with no MCP in the loop.

### 4 · Quality gates that don't lie
- `ghc_check_module` / `ghc_check_project` fold compile / warnings / typed-holes / property-replay into one JSON verdict. `warnings_block=true` (the default) refuses to mark a module green if hlint is dirty.
- `ghc_lint` runs real hlint with the same recursive path CI uses.
- `ghc_format` runs fourmolu/ormolu with a layered resolution chain (host PATH → bundled → auto-download). If no real formatter is available the fallback returns `gateEligible: false` — **a degraded pass can never unlock a module-complete gate**.

### 5 · One-call finalizer
```text
ghc_gate()
  → regression · cabal test · cabal build · consolidated JSON
```
Three round-trips collapsed into one, per-step durations and partial-success semantics. `skip_regression` / `skip_cabal_test` / `skip_cabal_build` let the agent opt out of any step; exceptions are caught per-step so one red gate can't take down the session.

### 6 · Self-healing session
The in-process `GhcSession` is guarded by an `MVar` (single-writer to the `HscEnv` — GHC API is not thread-safe inside one env). Any uncaught exception in a tool handler **evicts** the session and the next call boots a fresh one. The inner `ghc_eval` budget (30 s) trips with a structured `error_kind: "timeout"` response and **resets the HscEnv in place** — in-flight callers unblock, subsequent calls see a fresh env. No indefinite hangs, no poisoned state carried forward.

---

## Install + run

**Option A — pre-built binary (fast path).** Each tagged release publishes stripped binaries + SHA256 checksums for `darwin-arm64` and `linux-x64` ([Releases](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases)). Intel-Mac users: build from source via Option B — GitHub retired the `macos-13` x64 runner, so we no longer ship a `darwin-x64` asset.

```bash
PLATFORM=darwin-arm64   # or linux-x64
VERSION=v0.1.0
curl -L -o haskell-flows-mcp.tar.gz \
  "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/$VERSION/haskell-flows-mcp-$PLATFORM.tar.gz"
curl -L -o haskell-flows-mcp.tar.gz.sha256 \
  "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/$VERSION/haskell-flows-mcp-$PLATFORM.tar.gz.sha256"
shasum -a 256 -c haskell-flows-mcp.tar.gz.sha256
mkdir -p "$HOME/.local/bin" && tar -xzf haskell-flows-mcp.tar.gz -C "$HOME/.local/bin/"
```

**Option B — from source (ghcup + cabal):**

```bash
git clone https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp
cd haskell-rules-and-mcp/mcp-server-haskell
cabal install exe:haskell-flows-mcp \
  --installdir="$HOME/.local/bin" \
  --install-method=copy \
  --overwrite-policy=always
```

Binary lands at `~/.local/bin/haskell-flows-mcp`. Point your MCP client at it. See [`mcp-server-haskell/README.md`](mcp-server-haskell/README.md) for full per-host config shape.

**No rules file needed on your machine.** The MCP handshake's `initialize.instructions` ships the situation→tool table dynamically derived from the live registry. If your host (Claude Code, Cursor) insists on a project-level rules file, call `ghc_bootstrap(host="claude-code", write=true)` — the server writes `.claude/rules/haskell-flows-mcp.md` from content baked into the running binary, always in sync with the tool surface you actually have.

---

## Tool surface (39 tools, 7 phases)

Full catalogue in [`mcp-server-haskell/README.md`](mcp-server-haskell/README.md). The ones that carry the weight:

| Tool | What it does |
|---|---|
| **`ghc_create_project`** | Atomic cabal scaffold. Emits a multi-step `chain` (deps + add_modules + load) the agent can `ghc_batch` in one call. |
| **`ghc_suggest(function_name=…)`** | Multi-engine law proposer with confidence + rationale. Sibling-aware across the module graph. Actively discourages laws unlikely to hold for the given name/shape. |
| **`ghc_quickcheck`** | Run a property, auto-persist on pass. |
| **`ghc_determinism`** | Re-run N times — the tool that catches the flake QuickCheck alone misses. Blocks flaky props from entering the regression set. |
| **`ghc_refactor`** | `rename_local` / `extract_binding` with **snapshot + compile-verify + rollback on type-check failure**. No broken-intermediate states. |
| **`ghc_gate`** | Regression + `cabal test` + `cabal build` in one call, per-step exception-safe. |
| **`ghc_bootstrap`** | Emit host rules file from the running binary — no repo clone required. |

Every response carries a `nextStep` hint pointing at the most probable next call, plus an optional multi-step `chain` the agent can batch via `ghc_batch`.

---

## Architecture at a glance

```
┌──────────────────────────────────────────────────────────────────────┐
│  MCP client (Claude Code / Cursor / any JSON-RPC-over-stdio host)    │
└────────────────────────────┬─────────────────────────────────────────┘
                             │  newline-delimited JSON-RPC 2.0
┌────────────────────────────┴─────────────────────────────────────────┐
│  Transport  (stdio loop, line-buffered, isEOF-terminated)            │
├──────────────────────────────────────────────────────────────────────┤
│  Server  (Mcp.Server)                                                │
│   · dispatch 39 tools                                                │
│   · MVar GhcSession — single-writer to HscEnv                        │
│   · evict-on-exception · per-tool 10-min outer timeout               │
│   · NextStep injection · WorkflowState tracking                      │
├──────────────────────────────────────────────────────────────────────┤
│  Tool handlers  (38 modules, one per tool)                           │
├──────────────────────────────────────────────────────────────────────┤
│  GhcSession  (Ghc.ApiSession)                                        │
│   · runGhc  · compileExpr  · exprType  · getInfo                     │
│   · LogAction hook → structured diagnostics (no output parsing)      │
│   · stanza-flag cache from `cabal v2-repl --with-compiler=<shim>`    │
│   · auto-load src/ + app/ on first call, cached across tool calls    │
├──────────────────────────────────────────────────────────────────────┤
│  PropertyStore  (MVar-locked JSON, per-project)                      │
│  Parsers  (Error / Hole / Type / TypeSignature / QuickCheck / HPC)   │
│  Refactor  (snapshot + compile-verify + rollback primitives)         │
└──────────────────────────────────────────────────────────────────────┘
```

### Security invariants enforced by construction

- **Path traversal impossible by construction.** `ModulePath` smart constructor rejects `..` via segment split (not just a prefix check — `normalise` does not collapse `..`). Every tool accepting a user-supplied path routes through it.
- **No shell interpolation.** Every external subprocess (cabal, hlint, fourmolu) spawned argv-form via `System.Process.proc "cmd" [args]`. Agent input never reaches a shell.
- **Input sanitisation.** `sanitizeExpression` rejects newlines, the historical framing sentinel, and inputs over 64 KiB before any `compileExpr` / `exprType` / `getInfo` call.
- **DoS caps.** 64 KiB output cap on `ghc_eval`; 30 s inner per-eval timeout trips a structured `error_kind: "timeout"` and resets the HscEnv in place; 10-min outer per-tool ceiling as final defence.
- **Session liveness.** Any uncaught exception in a handler evicts the session; the next call boots a fresh `HscEnv`. No call can poison subsequent ones.
- **Refactor atomicity.** `ghc_refactor` snapshots the target files before every edit and compile-verifies after; rollback on any type-check failure.
- **`.cabal` integrity.** `ghc_deps` / `ghc_add_modules` / `ghc_remove_modules` re-parse after every write and refuse to persist a shape that disagrees with the verb.
- **Concurrent saves serialised.** `PropertyStore` writes go through an MVar lock — no torn JSON even under parallel callers.

### A deliberate non-invariant

- **`ghc_eval` is arbitrary code execution by design.** The tool evaluates user-supplied Haskell in the server's own HscEnv via `compileExpr + unsafeCoerce`. It can read/write files the MCP process has permission for, open sockets, exec subprocesses. There is no sandbox, whitelist, or seccomp layer. The trust boundary is the MCP client — anything that can send `tools/call` already has ambient authority equivalent to a shell run by the user that launched the MCP. The `FlowSandboxEscape` scenario documents this contract explicitly. Clients that need sandboxing must layer it below the MCP (container / VM / jail) — not ask the tool layer to enforce it.

---

## Status & development model

- **`v0.1.0`** — first tagged release, experimental but test-covered: **240 unit-test functions** in `test/Spec.hs`, **37 E2E scenarios** driving the full JSON-RPC surface in-process, deterministic across three consecutive runs. All 5 matrix jobs (ubuntu + macos × GHC 9.10.1 + 9.12.2) green on master.
- **Architecture migration complete** — the server ran on a subprocess GHCi framing protocol through Wave 4; Wave 5 moved every tool to the in-process GHC API and the subprocess layer was retired entirely. `Ghc/ApiSession.hs` is now the single source of compile-and-execute state.
- **Iterated with [Claude Code](https://www.anthropic.com/claude-code)** following the spirit of the Haskell [Compact for Responsible Use of AI Tools](https://discourse.haskell.org/t/a-compact-for-responsible-use-of-ai-tools/13923). Maintainer accountability via tests; commits with substantive AI-generated code carry `Co-Authored-By: Claude` trailers.
- Tagged [`vibecoded`](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp) — provenance is on the record, not hidden.

## Platform support

| Platform | Status | Notes |
|---|---|---|
| `darwin-arm64` | ✅ **Supported** | Primary dev target; release binary + SHA256 checksum published per tag |
| `linux-x64` | ✅ **Supported** | Release binary + checksum published per tag |
| `darwin-x64` | ⚠️ Build from source | GitHub retired the `macos-13` runner; use `cabal install exe:haskell-flows-mcp` or run the `linux-x64` asset under Rosetta |
| `linux-arm64` · `win32-*` | ❌ Untested | Not on short-term roadmap |

Resolution chain: `host PATH → bundled → auto-download → unavailable`. Unsupported platforms fall through cleanly.

## Known limitations

- **`ghc_eval` is RCE-by-design.** Intentional — see the architecture section. Not a limitation of *implementation*; a limitation of the *trust model*. If the threat model needs a sandbox, run the MCP in one.
- **Suggestion engines use regex on type strings.** Advanced types (higher-rank, type families, GADTs) silently return zero suggestions. Future work: a proper `parseType` backed by the GHC API.
- **Not an HLS replacement.** `ghc_goto` / `ghc_doc` are thin; keep your native LSP running in parallel for cross-module jump + inline hover.
- **Parallel E2E currently races on CWD.** `HASKELL_FLOWS_E2E_PARALLEL=N` is opt-in; the default remains serial. Tracked in [#43](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/43) with a clean path to thread-safe parallelism.
- **Bus factor of 1.** Single maintainer, no SLA. See [CONTRIBUTING.md](CONTRIBUTING.md).

---

## Trust model

All tools the MCP shells out to (`cabal`, `ghc`, `hlint`, `hpc`, `fourmolu`/`ormolu`, `hoogle`, `hls`) are resolved from the user's own `$PATH`. The recommended install path is [`ghcup`](https://www.haskell.org/ghcup/) — canonical Haskell toolchain, signature-verified upstream. The MCP never downloads binaries on behalf of the user.

Every external invocation uses argv-form (`System.Process.proc "cmd" ["arg1", "arg2"]`), never a shell string — no interpolation path is ever open for agent-supplied input. Boundary validators (`mkModulePath`, `sanitizeExpression`, `validatePackageName`, `validateVersionConstraint`, `parseStanzaSelector`) reject malformed input before it reaches any subprocess.

See [SECURITY.md](SECURITY.md) for full disclosure channels and trust-boundary notes.

---

## Resources

| | |
|---|---|
| 📘 Tool reference PDF | [**docs/haskell-flows-mcp.pdf**](docs/haskell-flows-mcp.pdf) |
| 🧭 Rendered flow diagrams | [**docs/flows.md**](docs/flows.md) |
| 📝 Changelog | [**CHANGELOG.md**](CHANGELOG.md) |
| 🤝 Contributing | [**CONTRIBUTING.md**](CONTRIBUTING.md) |
| 🛡️ Security | [**SECURITY.md**](SECURITY.md) |
| 📜 Code of Conduct | [**CODE_OF_CONDUCT.md**](CODE_OF_CONDUCT.md) |
| 🗂 Agent-workflow dogfood logs | [`docs/dogfood-2026-04-19.md`](docs/dogfood-2026-04-19.md) · [`docs/dogfood-2026-04-19-rle.md`](docs/dogfood-2026-04-19-rle.md) |

---

## License

[BSD-3-Clause](LICENSE). Copyright © 2026 Damián Rafael Lattenero.
