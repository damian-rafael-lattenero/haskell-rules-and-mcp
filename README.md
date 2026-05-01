<div align="center">

# 🌀 haskell-flows

### **Property-first Haskell, driven by your AI agent.**

*An MCP server that turns "LLMs that write plausible Haskell" into "LLMs that write correct Haskell" — through QuickCheck laws, in-process GHC, and snapshot-verified refactors.*

[![Haskell CI](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml)
[![Nix flake](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)

[![MCP tools](https://img.shields.io/badge/MCP%20tools-46-blue?logo=anthropic)](mcp-server-haskell/src/HaskellFlows/Tool)
[![Unit tests](https://img.shields.io/badge/unit%20tests-514%20passing-brightgreen)](mcp-server-haskell/test/Spec.hs)
[![E2E scenarios](https://img.shields.io/badge/e2e%20scenarios-72-brightgreen)](mcp-server-haskell/test-e2e/Scenarios)
[![GHC](https://img.shields.io/badge/GHC-9.10%20%7C%209.12-8a5aa0?logo=haskell)](https://www.haskell.org/ghc/)
[![Runtime](https://img.shields.io/badge/runtime-in--process%20GHC%20API-orange)](mcp-server-haskell/src/HaskellFlows/Ghc/ApiSession.hs)
[![Envelope](https://img.shields.io/badge/contract-unified%20envelope%20%23%2390-success)](mcp-server-haskell/src/HaskellFlows/Mcp/Envelope.hs)

</div>

---

## ⚡ The 30-second story

Plug into Claude Code, Cursor, or any MCP host. Your agent gets **46 tools** that share **one in-process GHC session** and **one normative response envelope** — every call answers with the same structured shape, every gate is honest, every refactor verifies-or-rolls-back.

```text
ghc_project(create) ─▶ ghc_modules ─▶ ghc_load
       │
       ├─▶ ghc_suggest        ← multi-engine law proposer w/ confidence + sibling-aware
       ├─▶ ghc_quickcheck     ← runs + auto-persists on pass
       ├─▶ ghc_determinism    ← N runs to catch the flake QC alone misses ⚠️
       ├─▶ ghc_regression     ← replays the whole persisted set
       ├─▶ ghc_refactor       ← snapshot + compile-verify + rollback on red
       └─▶ ghc_gate           ← regression + cabal test + cabal build, one call
```

📘 **Full flows** → [`docs/flows.md`](docs/flows.md) · 🚀 **Install** → [`docs/install.md`](docs/install.md) · 🛡 **Trust model** → [SECURITY.md](SECURITY.md)

---

## 🎯 What makes it different

<table>
<tr>
<td width="33%" valign="top">

### 🧪 Property-first
Your agent doesn't write tests after the fact. It calls `ghc_suggest(function_name="simplify")`, gets **8 engines** worth of laws — endomorphism, binary-op, list, roundtrip, evaluator-preservation, constant-folding, functor, sibling-aware — each with **confidence + rationale**.

> Low-confidence laws come with *"this probably fails because…"* The engine pushes back when the name hints at a normaliser and the caller asked for "involutive."

</td>
<td width="33%" valign="top">

### ⚙️ In-process GHC
No subprocess GHCi. No pipe framing. No prompt parsing. Every tool calls `runGhc` / `compileExpr` / `exprType` against the **server's own HscEnv**. Diagnostics arrive structured via `LogAction` hook — never parsed from output.

> A single MVar guards the HscEnv (GHC API isn't thread-safe inside one env). Any uncaught exception **evicts the session**; the next call boots a fresh one. No poisoned state, no indefinite hangs.

</td>
<td width="33%" valign="top">

### 📦 Unified envelope
Every response is the same shape: `{status, result?, error?, warnings?, nextStep?}`. Status is one of 7 closed values. Errors carry one of **33 closed `ErrorKind`** constructors — `path_traversal`, `compile_error`, `inner_timeout`, etc.

> Just landed: issue #90 closed end-to-end. Every one of the 46 tools routes through `Mcp.Envelope`. The legacy boolean `success`/`error_kind` is gone from the wire — `status` is the discriminator.

</td>
</tr>
</table>

---

## 💡 The "aha" moment

Building a 5-module arithmetic-expression evaluator, the first draft of `simplify` had an annihilation rewrite (`Mul 0 x ⇒ Lit 0`). `ghc_quickcheck` passed it **100/100**. `ghc_determinism` at 5 runs caught the bug:

```haskell
Mul (Lit 0) (Var unbound)
  -- in eval . simplify  →  Right 0
  -- in eval             →  Left (UnboundVar …)
```

The rewrite **swallowed an error**. Without `ghc_determinism`, that broken simplifier ships marked green.

> **That 5-second extra gate is the difference between "passed the tests" and "the property really holds."**

---

## 🛠 The tool surface — 46 in 7 phases

<div align="center">

| Phase | Tools | Marquee |
|---|---|---|
| 🏗 **Scaffold** | 5 | `ghc_project(action=create)` — atomic cabal scaffold + `chain` hint |
| 🔍 **Inspect** | 10 | `ghc_browse` · `ghc_info` · `ghc_eval` · `ghc_hole` |
| 📚 **Deps** | 3 | `ghc_deps` — verb-checked, post-edit re-parse, refuses incoherent writes |
| 🧪 **Property-first** | 7 | `ghc_suggest` · `ghc_quickcheck` · `ghc_determinism` · `ghc_regression` |
| 🛡 **Gates** | 5 | `ghc_check_module` · `ghc_gate` — collapsed pre-push finalizer |
| ✏️ **Refactor** | 5 | `ghc_refactor` — snapshot + compile-verify + rollback |
| 🧠 **Advanced** | 11 | `ghc_lab` · `ghc_witness` · `ghc_explain_error` · `ghc_perf` · `ghc_batch` |

</div>

Every response carries a `nextStep` pointer at the most-likely follow-up call, plus an optional multi-step `chain` your agent can `ghc_batch` in one round-trip.

---

## 🚀 Install

```bash
# Pre-built binary (darwin-arm64 or linux-x64)
PLATFORM=darwin-arm64 VERSION=v0.1.0
curl -L "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/$VERSION/haskell-flows-mcp-$PLATFORM.tar.gz" \
  | tar -xz -C "$HOME/.local/bin/"
```

Or build from source:

```bash
git clone https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp
cd haskell-rules-and-mcp/mcp-server-haskell
cabal install exe:haskell-flows-mcp --installdir="$HOME/.local/bin" \
  --install-method=copy --overwrite-policy=always
```

Point your MCP host at `~/.local/bin/haskell-flows-mcp`. **No rules file needed** — the `initialize.instructions` handshake ships a situation→tool table dynamically derived from the live registry. If your host insists on a project-level rules file, run `ghc_project(action="bootstrap", host="claude-code", write=true)`.

---

## 🏛 Architecture at a glance

```
┌──────────────────────────────────────────────────────────────────────┐
│  MCP client  (Claude Code · Cursor · any JSON-RPC-over-stdio host)   │
└──────────────────────────────┬───────────────────────────────────────┘
                               │  newline-delimited JSON-RPC 2.0
┌──────────────────────────────┴───────────────────────────────────────┐
│  Mcp.Server  ·  46-tool dispatch  ·  10-min outer timeout            │
│  Mcp.Envelope  ·  status × result × error × warnings × nextStep      │
│  Mcp.NextStep  ·  per-tool routing (envelope-aware)                  │
│  Mcp.WorkflowState  ·  history-aware help (BUG-08)                   │
├──────────────────────────────────────────────────────────────────────┤
│  Tool handlers  (46 modules, one per tool, all envelope-emitting)    │
├──────────────────────────────────────────────────────────────────────┤
│  Ghc.ApiSession  ·  MVar-guarded HscEnv  ·  evict-on-exception       │
│   · runGhc · compileExpr · exprType · getInfo                        │
│   · LogAction hook → structured diagnostics                          │
│   · stanza-flag cache from `cabal v2-repl --with-compiler=<shim>`    │
├──────────────────────────────────────────────────────────────────────┤
│  PropertyStore  ·  MVar-locked JSON · per-project regression set     │
│  Refactor       ·  snapshot + compile-verify + rollback primitives   │
│  Parsers        ·  Error · Hole · Type · TypeSignature · QuickCheck  │
└──────────────────────────────────────────────────────────────────────┘
```

**~22,700 lines of Haskell**. Single binary. No daemon, no IPC, no external state — just `stdin`/`stdout` + your project tree + `~/.haskell-flows/`.

---

## 🛡 Security invariants — by construction, not by convention

| Invariant | Mechanism |
|---|---|
| **Path traversal impossible** | `ModulePath` smart-ctor rejects `..` via segment split (not prefix check). Every user-supplied path routes through it. |
| **No shell interpolation** | Every subprocess (cabal, hlint, fourmolu) spawned argv-form via `proc "cmd" [args]`. Agent input never reaches `sh`. |
| **Input sanitised** | `sanitizeExpression` rejects newlines, framing sentinels, and inputs > 64 KiB before any `compileExpr`. |
| **DoS caps** | 64 KiB output cap on `ghc_eval` · 30 s inner per-eval timeout · 10-min outer per-tool ceiling. |
| **Session liveness** | Any uncaught exception evicts the `HscEnv`. Next call boots fresh. No poison carried forward. |
| **Refactor atomicity** | `ghc_refactor` snapshots + compile-verifies + rolls back on type-check failure. No broken intermediates on disk. |
| **`.cabal` integrity** | `ghc_deps` / `ghc_modules` re-parse after every write; refuse to persist a shape that disagrees with the verb. |
| **Concurrent saves serialised** | `PropertyStore` writes go through MVar lock — no torn JSON under parallel callers. |

> **One deliberate non-invariant**: `ghc_eval` is **arbitrary code execution by design**. Anything that can send `tools/call` already has ambient authority equivalent to a shell run by the user that launched the MCP. If your threat model needs a sandbox, run the MCP in one (container / VM / jail).

---

## 📊 Status

<div align="center">

| | |
|---|---|
| 🏷 **Release** | `v0.1.0` — first tagged release, experimental but heavily test-covered |
| 🧪 **Test coverage** | **514** unit-test functions · **72** E2E scenarios driving the full JSON-RPC surface in-process |
| ✅ **CI matrix** | 4 cells: `{ubuntu-latest, macos-latest} × {GHC 9.10.1, 9.12.2}` |
| 🛡 **Closed enum** | `ErrorKind` is **33 constructors** — every error path on the wire is enumerable |
| 📜 **Wire contract** | **#90 closed end-to-end** — single envelope, no dual-shape, structured `status` + nested `error.kind` |
| 🏗 **Platforms** | `darwin-arm64` ✅ · `linux-x64` ✅ · `darwin-x64` ⚠️ build-from-source · others ❌ untested |

</div>

**Iterated with [Claude Code](https://www.anthropic.com/claude-code)** following the spirit of the Haskell [Compact for Responsible Use of AI Tools](https://discourse.haskell.org/t/a-compact-for-responsible-use-of-ai-tools/13923). Maintainer accountability via tests; commits with substantive AI-generated code carry `Co-Authored-By: Claude` trailers. Tagged `vibecoded` — provenance is on the record, not hidden.

### Known limitations

- **`ghc_eval` is RCE-by-design** — see security section. Layer your sandbox below the MCP.
- **Suggestion engines use regex on type strings** — higher-rank, type-families, GADTs return zero suggestions. Future: `parseType` via GHC API.
- **Not an HLS replacement.** `ghc_goto` / `ghc_doc` are thin; keep your LSP for cross-module jump.
- **Bus factor of 1.** Single maintainer, no SLA. See [CONTRIBUTING.md](CONTRIBUTING.md).

---

## 📚 Resources

| | |
|---|---|
| 📘 **Tool reference (PDF)** | [`docs/haskell-flows-mcp.pdf`](docs/haskell-flows-mcp.pdf) |
| 🧭 **Flow diagrams** | [`docs/flows.md`](docs/flows.md) — property loop · bootstrap · refactor · session lifecycle |
| 🚀 **Install guide** | [`docs/install.md`](docs/install.md) — three methods + per-host MCP wiring |
| 📝 **Changelog** | [CHANGELOG.md](CHANGELOG.md) |
| 🤝 **Contributing** | [CONTRIBUTING.md](CONTRIBUTING.md) |
| 🛡 **Security** | [SECURITY.md](SECURITY.md) |
| 🗂 **Dogfood logs** | [`docs/dogfood-2026-04-19.md`](docs/dogfood-2026-04-19.md) · [`-rle`](docs/dogfood-2026-04-19-rle.md) |

---

<div align="center">

[BSD-3-Clause](LICENSE) · Copyright © 2026 Damián Rafael Lattenero
**·** [Releases](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases) **·** [Issues](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues) **·** [Discussions](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/discussions)

</div>
