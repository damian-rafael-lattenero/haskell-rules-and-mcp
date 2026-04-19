<div align="center">

# haskell-flows

**An agent-first MCP server for property-driven Haskell development.**

[![Haskell CI](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml)
[![Nix flake](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)
[![Haskell tests](https://img.shields.io/badge/Haskell%20tests-34%20passing-brightgreen)](mcp-server-haskell/test)
[![Haskell MCP tools](https://img.shields.io/badge/Haskell%20tools-15-blue)](mcp-server-haskell/src/HaskellFlows/Tool)
[![GHC](https://img.shields.io/badge/GHC-9.10%20%7C%209.12-8a5aa0)](https://www.haskell.org/ghc/)

</div>

---

## TL;DR (30 seconds)

`haskell-flows` is an MCP server that lets **AI agents** (Claude Code, Cursor, any MCP client) drive a **property-first Haskell workflow** end-to-end: scaffold a project, synthesize `Arbitrary` instances, suggest QuickCheck laws from the module's type shape, persist passing properties into a regression suite, run hlint/fourmolu as real gates, and collapse pre-commit checks into a single call.

It solves the fundamental gap between "LLMs generate Haskell" and "LLMs generate **correct** Haskell": every step is compiler-verified, every property persists, every gate is honest.

```text
ghci_create_project  →  ghci_suggest(analyze)  →  ghci_quickcheck(label=…)
  →  ghci_lint  →  ghci_format  →  ghci_workflow(action="gate")
```

---

## What it actually does (2-min scan)

### 1 · Scaffold
One call creates a cabal project: `<name>.cabal`, `cabal.project`, `src/<Module>.hs` per module, `test/Spec.hs`. GHCi session starts automatically.

### 2 · Suggest properties from type signatures
When you load a module with `simplify :: Expr -> Expr` **next to** `eval :: Env -> Expr -> r`, `ghci_suggest(analyze)` proposes:

```haskell
-- constant-folding soundness · confidence: high
\p1 x -> eval p1 (simplify x) == eval p1 (x :: Expr)
```

with rationale and confidence. **Seven engines** detect shapes: endomorphism, binary-op, list-endomorphism, roundtrip, evaluator-preservation, constant-folding-soundness, functor-laws.

### 3 · Run + persist
`ghci_quickcheck` runs the property, auto-saves passing ones to `.haskell-flows/properties.json`, accepts a `label=` so `test/Spec.hs` comes out with `addRightIdentity:` not `property_1:`.

### 4 · Quality gates that don't lie
`ghci_lint` (real hlint) and `ghci_format` (real fourmolu/ormolu) run with a layered resolution chain (host PATH → bundled → auto-download). If no real linter is available, the fallback returns `gateEligible: false` so **a degraded pass can never unlock a module-complete gate**.

### 5 · One-call finalizer
```text
ghci_workflow(action="gate")
  → regression · cabal test · cabal build · consolidated JSON
```

Three round-trips collapsed into one, with per-step durations and partial-success semantics.

### 6 · Agents can hot-reload the MCP itself
Edited TypeScript? `mcp_reload_code(confirm=true)` schedules a graceful restart so Claude Desktop respawns the child with the fresh bundle — no session exit. Staleness-gated + rate-limited (10 s).

---

## Install + run

```bash
git clone https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp
cd haskell-rules-and-mcp

# Option A — Nix (recommended, reproducible)
nix develop

# Option B — host toolchain (ghcup + node 22)
cd mcp-server
npm install
npm run build

# Point your MCP client at the server
cp ../.mcp.example.json ../.mcp.json
```

`.mcp.example.json` is the template; drop it into your Claude Code / Cursor config. See [`.mcp.example.json`](.mcp.example.json) for the exact shape.

---

## Tool surface

30+ MCP tools grouped by workflow phase. The five that carry the weight:

| Tool | What it does |
|---|---|
| **`ghci_create_project`** | Scaffold a cabal project atomically — one call, no prompts. |
| **`ghci_suggest(analyze)`** | Seven engines propose QuickCheck laws with confidence + rationale. |
| **`ghci_quickcheck`** | Run a property, auto-persist on pass, auto-resolve scope errors. |
| **`ghci_workflow(gate)`** | Regression + `cabal test` + `cabal build` in a single call. |
| **`mcp_reload_code`** | Graceful process restart so TS edits take effect without exiting the client. |

Full catalog with "what problem each one solved" → [**docs/haskell-flows-mcp.pdf**](docs/haskell-flows-mcp.pdf).

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
- **Not an HLS replacement** — `ghci_hls` is a thin bridge; keep your native LSP client running in parallel.

---

## Trust model

Every bundled tool binary carries a SHA256 pinned in [`vendor-tools/bundled-tools-manifest.json`](mcp-server/vendor-tools/bundled-tools-manifest.json) with an **invariant test** that fails CI on any future drift. The trust ordering:

1. **`ghcup install <tool>`** — the recommended path; canonical Haskell toolchain.
2. **Upstream direct binaries** — when available (e.g. `fourmolu/fourmolu` on darwin-arm64), `auto-download.ts` tries upstream first.
3. **Personal mirror** — extracted binaries from upstream tarballs, SHA256-pinned, used only when upstream doesn't publish direct executables.

See [SECURITY.md](SECURITY.md) for full disclosure channels and trust-boundary notes.

---

## Resources

| | |
|---|---|
| 📘 Tool reference PDF | [**docs/haskell-flows-mcp.pdf**](docs/haskell-flows-mcp.pdf) |
| 📝 Changelog | [**CHANGELOG.md**](CHANGELOG.md) |
| 🤝 Contributing | [**CONTRIBUTING.md**](CONTRIBUTING.md) |
| 🛡️ Security | [**SECURITY.md**](SECURITY.md) |
| 📜 Code of Conduct | [**CODE_OF_CONDUCT.md**](CODE_OF_CONDUCT.md) |
| 🗂 Agent workflow rules | [`mcp-server/rules/`](mcp-server/rules/) — surfaced as MCP resources |

---

## License

[BSD-3-Clause](LICENSE). Copyright © 2026 Damián Rafael Lattenero.
