<div align="center">

# 🌀 haskell-flows

### **Property-first Haskell, driven by your AI agent.**

*An MCP server that turns "LLMs that write plausible Haskell" into "LLMs that write correct Haskell" — through QuickCheck laws, in-process GHC, and snapshot-verified refactors.*

[![Haskell CI](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/haskell-ci.yml)
[![Nix flake](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/nix-flake.yml)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)

[![MCP tools](https://img.shields.io/badge/MCP%20tools-35-blue?logo=anthropic)](mcp-server-haskell/src/HaskellFlows/Tool)
[![Unit tests](https://img.shields.io/badge/unit%20tests-680%20passing-brightgreen)](mcp-server-haskell/test/Spec.hs)
[![E2E scenarios](https://img.shields.io/badge/e2e%20scenarios-72-brightgreen)](mcp-server-haskell/test-e2e/Scenarios)
[![GHC](https://img.shields.io/badge/GHC-9.10%20%7C%209.12-8a5aa0?logo=haskell)](https://www.haskell.org/ghc/)
[![Runtime](https://img.shields.io/badge/runtime-in--process%20GHC%20API-orange)](mcp-server-haskell/src/HaskellFlows/Ghc/ApiSession.hs)
[![Envelope](https://img.shields.io/badge/contract-unified%20envelope%20%23%2390-success)](mcp-server-haskell/src/HaskellFlows/Mcp/Envelope.hs)

</div>

---

## ⚡ The 30-second story

Plug into Claude Code, Cursor, or any MCP host. Your agent gets **35 tools** that share **one in-process GHC session** and **one normative response envelope** — every call answers with the same structured shape, every gate is honest, every refactor verifies-or-rolls-back.

```text
ghc_project(create) ─▶ ghc_modules ─▶ ghc_load
       │
       ├─▶ ghc_suggest             ← multi-engine law proposer w/ confidence + sibling-aware
       ├─▶ ghc_quickcheck          ← runs + auto-persists on pass; runs>=2 catches flakes
       ├─▶ ghc_property_store(run) ← replays the whole persisted set
       ├─▶ ghc_refactor            ← snapshot + compile-verify + rollback on red
       └─▶ ghc_gate                ← regression + cabal test + cabal build, one call
```

📘 **Full flows** → [`docs/flows.md`](docs/flows.md) · 🚀 **Install** → [`docs/install.md`](docs/install.md) · 🛡 **Trust model** → [SECURITY.md](SECURITY.md)

---

## 🎯 What makes it different

| | |
|---|---|
| 🧪 **Property-first** | `ghc_suggest` proposes laws from the function name — 8 engines (endomorphism, binary-op, list, roundtrip, evaluator-preservation, constant-folding, functor, sibling-aware) each with confidence + rationale. Low-confidence suggestions come with *"this probably fails because…"*. |
| ⚙️ **In-process GHC** | No subprocess GHCi, no pipe framing, no prompt parsing. Every tool calls `runGhc`/`compileExpr`/`exprType` against the server's own `HscEnv`. Diagnostics arrive structured via `LogAction` hook. Any uncaught exception evicts the session; next call boots fresh. |
| 📦 **Unified envelope** | Every response: `{status, result?, error?, warnings?, nextStep?}`. Status is one of 7 closed values. Errors carry one of **26 closed `ErrorKind`** constructors. The legacy `success`/`error_kind` booleans are gone — `status` is the discriminator. |

---

## 💡 The "aha" moment

Building a 5-module arithmetic-expression evaluator, a `simplify` annihilation rewrite (`Mul 0 x ⇒ Lit 0`) passed `ghc_quickcheck` 100/100 — but `ghc_quickcheck(runs=5)` caught the bug:

```haskell
Mul (Lit 0) (Var unbound)
  -- eval . simplify  →  Right 0   (error swallowed)
  -- eval             →  Left (UnboundVar …)
```

**5 extra seconds of determinism checking is the difference between "passed the tests" and "the property really holds."**

---

## 🛠 The tool surface — 35 in 7 phases

<div align="center">

| Phase | Tools | Marquee |
|---|---|---|
| 🏗 **Scaffold** | 4 | `ghc_project(action=create)` · `ghc_modules` · `ghc_workflow` · `ghc_toolchain` |
| 🔍 **Inspect** | 9 | `ghc_load` · `ghc_browse` · `ghc_eval` · `ghc_hole` · `ghc_type` · `ghc_info` · `ghc_complete` · `ghc_goto` · `ghc_doc` |
| 📚 **Deps & scope** | 5 | `ghc_deps` · `ghc_add_import` · `ghc_apply_exports` · `ghc_imports` · `hoogle_search` |
| 🧪 **Property-first** | 4 | `ghc_suggest` · `ghc_quickcheck` · `ghc_property_store` (list/run/export/audit) · `ghc_arbitrary` |
| 🛡 **Gates** | 7 | `ghc_check_module` · `ghc_check_project` · `ghc_gate` · `ghc_lint` · `ghc_fix_warning` · `ghc_format` · `ghc_coverage` |
| ✏️ **Refactor** | 1 | `ghc_refactor` — snapshot + compile-verify + rollback |
| 🧠 **Advanced** | 5 | `ghc_lab` · `ghc_witness` · `ghc_explain_error` · `ghc_perf` · `ghc_batch` |

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

## 🏛 Architecture

```
┌──────────────────────────────────────────────────────────────────────┐
│  MCP client  (Claude Code · Cursor · any JSON-RPC-over-stdio host)   │
└──────────────────────────────┬───────────────────────────────────────┘
                               │  newline-delimited JSON-RPC 2.0
┌──────────────────────────────┴───────────────────────────────────────┐
│  Mcp.Server  ·  35-tool dispatch  ·  10-min outer timeout            │
│  Mcp.Envelope  ·  status × result × error × warnings × nextStep      │
│  Mcp.NextStep  ·  per-tool routing (envelope-aware)                  │
│  Mcp.WorkflowState  ·  history-aware help                            │
├──────────────────────────────────────────────────────────────────────┤
│  Tool handlers  (35 modules, one per tool, all envelope-emitting)    │
├──────────────────────────────────────────────────────────────────────┤
│  Ghc.ApiSession  ·  MVar-guarded HscEnv  ·  evict-on-exception       │
│   · runGhc · compileExpr · exprType · getInfo                        │
│   · LogAction hook → structured diagnostics                          │
├──────────────────────────────────────────────────────────────────────┤
│  PropertyStore  ·  MVar-locked JSON · per-project regression set     │
│  Refactor       ·  snapshot + compile-verify + rollback primitives   │
│  Parsers        ·  Error · Hole · Type · TypeSignature · QuickCheck  │
└──────────────────────────────────────────────────────────────────────┘
```

**~25,700 lines of Haskell** (source). Single binary. No daemon, no IPC, no external state — just `stdin`/`stdout` + your project tree + `~/.haskell-flows/`.

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
| 🧪 **Test coverage** | **680** unit-test functions · **72** E2E scenarios driving the full JSON-RPC surface in-process |
| ✅ **CI matrix** | 4 cells: `{ubuntu-latest, macos-latest} × {GHC 9.10.1, 9.12.2}` |
| 🛡 **Closed enum** | `ErrorKind` is **26 constructors** — every error path on the wire is enumerable |
| 📜 **Wire contract** | **#90 closed end-to-end** — single envelope, no dual-shape, structured `status` + nested `error.kind` |
| 🏗 **Platforms** | `darwin-arm64` ✅ · `linux-x64` ✅ · `darwin-x64` ⚠️ build-from-source · others ❌ untested |

</div>

**Iterated with [Claude Code](https://www.anthropic.com/claude-code)** following the spirit of the Haskell [Compact for Responsible Use of AI Tools](https://discourse.haskell.org/t/a-compact-for-responsible-use-of-ai-tools/13923). Maintainer accountability via tests; commits with substantive AI-generated code carry `Co-Authored-By: Claude` trailers. Tagged `vibecoded` — provenance is on the record, not hidden.

### Known limitations

- **`ghc_eval` is RCE-by-design** — see security section. Layer your sandbox below the MCP.
- **Suggestion engines use regex on type strings** — higher-rank, type-families, GADTs return zero suggestions.
- **Not an HLS replacement.** `ghc_goto` / `ghc_doc` are thin; keep your LSP for cross-module jump.
- **Bus factor of 1.** Single maintainer, no SLA. See [CONTRIBUTING.md](CONTRIBUTING.md).

---

## 📚 Resources

| | |
|---|---|
| 📘 **Tool reference (PDF)** | [`docs/haskell-flows-mcp.pdf`](docs/haskell-flows-mcp.pdf) |
| 🧭 **Flow diagrams** | [`docs/flows.md`](docs/flows.md) |
| 🚀 **Install guide** | [`docs/install.md`](docs/install.md) |
| 📝 **Changelog** | [CHANGELOG.md](CHANGELOG.md) |
| 🤝 **Contributing** | [CONTRIBUTING.md](CONTRIBUTING.md) |
| 🛡 **Security** | [SECURITY.md](SECURITY.md) |
| 🗂 **Dogfood logs** | [`docs/dogfood-2026-04-19.md`](docs/dogfood-2026-04-19.md) |

---

<div align="center">

[BSD-3-Clause](LICENSE) · Copyright © 2026 Damián Rafael Lattenero
**·** [Releases](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases) **·** [Issues](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues) **·** [Discussions](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/discussions)

</div>
