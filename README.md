# haskell-flows-mcp

An MCP server for property-first Haskell development. It gives your
agent **12 tools** over one embedded [ghcide](https://github.com/haskell/haskell-language-server)
session: scaffold a project, author whole modules, compile-check,
evaluate expressions, propose and run QuickCheck laws, and gate
before push — every write verifies-or-rolls-back.

**Status: v1.0.0 — stable. Maintenance only: bugfixes and GHC
compatibility; no feature roadmap.**

[![CI](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/ci.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/ci.yml)
[![Release](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/actions/workflows/release.yml/badge.svg)](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)

## The 12 tools

| Tool | What it does |
|---|---|
| `ghc_project` | `create` / `switch` / `list` / `validate` / `bootstrap` — project scaffolding (test-suite ships QuickCheck) |
| `ghc_module` | `add` / `remove`, scratchpad verbs, and `write_module` — whole-module authorship with compile-verify + rollback |
| `ghc_edit` | refactor verbs (snapshot + verify + rollback), `import`, `exports`, `fix_warning`, `format` |
| `ghc_check` | `load` / `module` / `project` / `lint` — compile and typecheck gates |
| `ghc_eval` | evaluate an expression in the loaded module's context |
| `ghc_inspect` | `type` / `hole` / `info` / `browse` / `complete` / `goto` / `doc` |
| `ghc_deps` | `add` / `remove` / `list` / `explain` — dependency surgery on the .cabal |
| `ghc_suggest` | law candidates for the current module (8 engines, confidence-rated) |
| `ghc_property` | QuickCheck run + persist, `run` replay, `export` to test/Spec.hs, `arbitrary` derivation |
| `ghc_gate` | pre-push gate: build + tests + lint + property replay, one call |
| `ghc_batch` | run a prepared chain of calls in one round-trip |
| `ghc_session` | `status` / `help` / `toolchain` / `warmup` / `imports` + workflow verbs |

Retired names map to successors in
[docs/TOOL_TAXONOMY.md](docs/TOOL_TAXONOMY.md).

## Quickstart

1. Prerequisites — GHC 9.10–9.12 + cabal via
   [ghcup](https://www.haskell.org/ghcup/) (see
   [docs/requirements.md](docs/requirements.md) for the full table).
2. Get the binary (darwin-arm64 / linux-x64), or build from source:

   ```bash
   curl -L "https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/download/v1.0.0/haskell-flows-mcp-darwin-arm64.tar.gz" \
     | tar -xz -C "$HOME/.local/bin/"
   # verify: sha256 with the matching .sha256 asset, then:
   haskell-flows-mcp --version && haskell-flows-mcp --doctor

   # or from source:
   git clone https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp
   cd haskell-rules-and-mcp/mcp-server-haskell
   cabal install exe:haskell-flows-mcp --installdir="$HOME/.local/bin"
   ```

3. Wire your MCP host (stdio):

   ```json
   { "mcpServers": { "haskell-flows": {
       "command": "/absolute/path/to/haskell-flows-mcp" } } }
   ```

4. In-session: the `initialize` handshake ships a situation→tool
   table derived from the live registry — no rules file needed.

## The property-first loop

1. `ghc_module(add)` + `write_module` — author a module.
2. `ghc_suggest` — proposed laws with confidence and rationale.
3. `ghc_property(check)` — run a law; passing properties persist to
   the project store automatically.
4. `ghc_gate` — one call: build + `cabal test` (replays the whole
   persisted set) + lint.

## Verification

Every push: `scripts/gate.sh` tiers — t0 build, t1 unit (775 tests)
+ deadlock canary, t2 full e2e (424 checks across ~70 scenarios),
all under a hang-watchdog. Dogfood evidence for the 1.0.0 loop:
[docs/dogfood-2026-10-10.md](docs/dogfood-2026-10-10.md) (three
MCP-only runs, mechanically scored).

## Architecture

One binary, stdio JSON-RPC in/out. `Mcp.Server` dispatches the 12
tools behind a normative envelope (`status` / `result` / `error` /
`warnings` / `nextStep`). All compile/type/eval work routes through
one embedded ghcide `IdeSession` guarded by an `MVar`. Properties
persist per-project under `.haskell-flows/`. External tools (cabal,
hlint, formatters) spawn argv-form only — no shell interpolation,
path traversal rejected by construction.

## Platforms

darwin-arm64 (verified end-to-end) · linux-x64 (CI-built release
binary) · **Windows: not supported.**

## Documentation

| | |
|---|---|
| [Requirements & `--doctor`](docs/requirements.md) | hard vs optional binaries, env knobs |
| [Install guide](docs/install.md) | download, checksums, cabal fallback |
| [Tool taxonomy](docs/TOOL_TAXONOMY.md) | the 12 tools + retired-name successors |
| [Flows](docs/flows.md) · [Testing](docs/testing.md) | end-to-end workflows, tiers |
| [Dogfood evidence](docs/dogfood-2026-10-10.md) | triple run, E4 green |
| [Changelog](CHANGELOG.md) · [Contributing](CONTRIBUTING.md) · [Security](SECURITY.md) | |

BSD-3-Clause — Damián Rafael Lattenero, 2026.
