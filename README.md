<div align="center">

# haskell-flows

**An agent-first MCP server for property-driven Haskell development.**

[![Haskell CI](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml)
[![Nix flake](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)
[![Haskell tests](https://img.shields.io/badge/Haskell%20tests-130%20passing-brightgreen)](mcp-server-haskell/test)
[![Haskell MCP tools](https://img.shields.io/badge/Haskell%20tools-36-blue)](mcp-server-haskell/src/HaskellFlows/Tool)
[![GHC](https://img.shields.io/badge/GHC-9.10%20%7C%209.12-8a5aa0)](https://www.haskell.org/ghc/)

</div>

---

## TL;DR (30 seconds)

`haskell-flows` is an MCP server that lets **AI agents** (Claude Code, Cursor, any MCP client) drive a **property-first Haskell workflow** end-to-end: scaffold a project, synthesize `Arbitrary` instances, suggest QuickCheck laws from the module's type shape, persist passing properties into a regression suite, run hlint/fourmolu as real gates, and collapse pre-commit checks into a single call.

It solves the fundamental gap between "LLMs generate Haskell" and "LLMs generate **correct** Haskell": every step is compiler-verified, every property persists, every gate is honest.

```text
ghc_create_project  →  ghc_add_modules / ghc_deps
  →  ghc_load  →  ghc_suggest(function_name=…)
  →  ghc_quickcheck(property=…, module=…)
  →  ghc_regression(action="run")  →  ghc_quickcheck_export
  →  ghc_gate                       # pre-push finalizer
```

See [`docs/flows.md`](docs/flows.md) for rendered Mermaid diagrams of
the four central flows: property-first dev loop, project bootstrap,
refactor snapshot-and-compile, and GHCi session lifecycle.

**Install:** [`docs/install.md`](docs/install.md) — three methods (release
binary / Hackage / source), plus MCP client wiring for Claude Code,
Cursor, etc.

Before pushing: run `scripts/ci-local.sh --fast` to replicate the CI
gates locally (cabal build + test + recursive hlint). The full
pipeline (add `haddock + cabal check + sdist`) takes ~5 min; drop
`--fast` for the complete run that matches `Haskell CI` end-to-end.

---

## What it actually does (2-min scan)

### 1 · Scaffold
One call creates a cabal project: `<name>.cabal`, `cabal.project`, `src/<Module>.hs` per module, `test/Spec.hs`. GHCi session starts automatically.

### 2 · Suggest properties from type signatures
When you load a module with `simplify :: Expr -> Expr` **next to** `eval :: Env -> Expr -> r`, `ghc_suggest(function_name="simplify")` proposes:

```haskell
-- constant-folding soundness · confidence: high
\env x -> eval env (simplify x) == eval env x
```

with rationale and confidence. **Multiple engines** detect shapes: endomorphism (idempotent / involutive), binary-op (associative / commutative / identity), list-endomorphism, roundtrip, evaluator-preservation, constant-folding-soundness, functor-laws. The sibling-aware ones (preservation / soundness) walk the loaded module's other top-level bindings via `:browse` so the right law fires without hand-supplied siblings.

### 3 · Run + persist
`ghc_quickcheck` runs the property, auto-saves passing ones to `.haskell-flows/properties.json`. `ghc_quickcheck_export` materialises the persisted set as a committable `test/Spec.hs` — `cabal test` then replays them in CI without the MCP in the loop. `ghc_determinism` re-runs a property N times to catch flakiness before it enters the regression suite.

### 4 · Quality gates that don't lie
`ghc_lint` (real hlint) and `ghc_format` (real fourmolu/ormolu) run with a layered resolution chain (host PATH → bundled → auto-download). If no real linter is available, the fallback returns `gateEligible: false` so **a degraded pass can never unlock a module-complete gate**.

### 5 · One-call finalizer
```text
ghc_gate()
  → regression · cabal test · cabal build · consolidated JSON
```

Three round-trips collapsed into one, with per-step durations and partial-success semantics. `skip_regression` / `skip_cabal_test` / `skip_cabal_build` let the agent opt out of any step; exceptions are caught per-step so one red gate can't take down the session.

### 6 · Agents know when a session died
The GHCi child is watched by the session layer. If the process exits or its pipes hit EOF, the `SessionStatus` flips to `Dead`, every in-flight command aborts with `SessionExhausted`, and the next call respawns a fresh child. No indefinite hangs, no zombies held behind a lock.

---

## Install + run

**Option A — pre-built binary (fast path).** Each tagged release
publishes stripped binaries + SHA256 checksums for
`darwin-arm64`, `darwin-x64`, `linux-x64`
([Releases](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases)):

```bash
PLATFORM=darwin-arm64   # or darwin-x64 / linux-x64
VERSION=v0.1.0          # pick the tag you want
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

Binary lands at `~/.local/bin/haskell-flows-mcp`. Point your MCP
client at it (`"command": "/path/to/.local/bin/haskell-flows-mcp"`).
See [`mcp-server-haskell/README.md`](mcp-server-haskell/README.md)
for the full per-host config shape.

**No rules file needed on your machine.** The MCP handshake's
`initialize.instructions` ships the situation→tool table dynamically
derived from the live registry. If your host (Claude Code, Cursor)
insists on a project-level rules file, call `ghc_bootstrap(host="claude-code", write=true)`
— the MCP writes `.claude/rules/haskell-flows-mcp.md` from content
baked into the binary, always in sync with the tool surface you have.

---

## Tool surface

38+ MCP tools grouped by workflow phase. The ones that carry the weight:

| Tool | What it does |
|---|---|
| **`ghc_create_project`** | Scaffold a cabal project atomically — one call, no prompts. Emits a multi-step `chain` (deps + add_modules + load) the agent can `ghc_batch`. |
| **`ghc_suggest(function_name=…)`** | Multi-engine law proposer with confidence + rationale. Sibling-aware: `simplify :: Expr -> Expr` next to `eval :: Env -> Expr -> r` auto-proposes evaluator preservation / constant-folding soundness at High confidence. |
| **`ghc_quickcheck`** | Run a property, auto-persist on pass; `ghc_determinism` re-runs N times to catch flakiness before adopting. |
| **`ghc_gate`** | Regression + `cabal test` + `cabal build` in a single call, per-step exception-safe. |
| **`ghc_bootstrap`** | Emit host rules file from the running binary — no external repo clone needed to get Claude Code / Cursor guidance. |

See [`mcp-server-haskell/README.md`](mcp-server-haskell/README.md) for
the full 38-tool catalogue by workflow phase.

---

## Status & development model

- **`v0.1.0`** — first tagged release, **experimental** but test-covered (1166 passing across unit / integration / e2e, deterministic across 3 consecutive runs).
- Iterated with **[Claude Code](https://www.anthropic.com/claude-code)** following the spirit of the Haskell [Compact for Responsible Use of AI Tools](https://discourse.haskell.org/t/a-compact-for-responsible-use-of-ai-tools/13923). Maintainer accountability via tests; commits with substantive AI-generated code carry `Co-Authored-By: Claude` trailers.
- Tagged [`vibecoded`](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp) — provenance is on the record, not hidden.

## Platform support

| Platform | Status | Notes |
|---|---|---|
| `darwin-arm64` | ✅ **Supported** | Primary dev target, bundled binaries pinned by SHA256 |
| `darwin-x64` · `linux-*` | ⚠️ Use host tools | Install via `ghcup install hlint fourmolu ormolu hls` |
| `win32-*` | ❌ Untested | Not on short-term roadmap |

Resolution chain: `host PATH → bundled → auto-download → unavailable`. Unsupported platforms fall through cleanly.

## Known limitations

- **Bus factor of 1** — single maintainer, no SLA. See [CONTRIBUTING.md](CONTRIBUTING.md).
- **Suggestion engines use regex** on type strings — advanced types (higher-rank, type families, GADTs) silently return zero suggestions.
- **Not an HLS replacement** — `ghc_hls` is a thin bridge; keep your native LSP client running in parallel.

---

## Trust model

All tools the MCP shells out to (`cabal`, `ghc`, `hlint`, `hpc`,
`fourmolu`/`ormolu`, `hoogle`, `hls`) are resolved from the user's
own `$PATH`. The recommended install path is
[`ghcup`](https://www.haskell.org/ghcup/) — canonical Haskell
toolchain, signature-verified upstream. The MCP never downloads
binaries on behalf of the user.

Every external invocation uses argv-form
(`System.Process.proc "cmd" ["arg1", "arg2"]`), never a shell
string — no interpolation path is ever open for agent-supplied
input. Boundary validators (`mkModulePath`, `sanitizeExpression`,
`validatePackageName`, `validateVersionConstraint`,
`parseStanzaSelector`) reject malformed input before it reaches any
subprocess.

See [SECURITY.md](SECURITY.md) for full disclosure channels and
trust-boundary notes.

---

## Resources

| | |
|---|---|
| 📘 Tool reference PDF | [**docs/haskell-flows-mcp.pdf**](docs/haskell-flows-mcp.pdf) |
| 📝 Changelog | [**CHANGELOG.md**](CHANGELOG.md) |
| 🤝 Contributing | [**CONTRIBUTING.md**](CONTRIBUTING.md) |
| 🛡️ Security | [**SECURITY.md**](SECURITY.md) |
| 📜 Code of Conduct | [**CODE_OF_CONDUCT.md**](CODE_OF_CONDUCT.md) |
| 🗂 Agent workflow dogfood | [`docs/dogfood-2026-04-19.md`](docs/dogfood-2026-04-19.md) + [`docs/dogfood-2026-04-19-rle.md`](docs/dogfood-2026-04-19-rle.md) — findings from end-to-end sessions |

---

## License

[BSD-3-Clause](LICENSE). Copyright © 2026 Damián Rafael Lattenero.
