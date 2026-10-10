# Requirements

What the `haskell-flows-mcp` binary needs at runtime, and how to
verify you have it. The same checks live inside the binary:
`haskell-flows-mcp --doctor` prints the table below with your actual
PATH and exits 0/1 — it can never drift from this document's source
of truth (the probes are shared with the live
`ghc_session(action="toolchain")` tool).

## Hard requirements

The server cannot do useful work without these. `--doctor` fails
(exit 1) if any is missing.

| Binary | Why it is hard | Install |
|---|---|---|
| `ghc` (9.12.x) | The embedded ghcide engine (built against GHC 9.12) compiles and loads your project in-process. | `ghcup install ghc --set` |
| `cabal` (≥ 3.12) | ghcide resolves your project through cabal (hie-bios cradle); scaffolding and the project gate shell out to it. | `ghcup install cabal --set` |
| `hlint` | The project gate (`ghc_gate`) runs the lint step; without it the gate reports red. | `cabal install hlint` |

One-time ghcup bootstrap (macOS + Linux):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.org | sh
```

## Optional, per feature

A missing optional disables exactly one feature — the affected tool
returns `status:"unavailable"` with the install hint. Nothing else
degrades.

| Binary | Feature | Install |
|---|---|---|
| `fourmolu` (or `ormolu`) | `ghc_edit(action="format")` | `cabal install fourmolu` |
| `hoogle` | Import-resolution suggestions (`ghc_edit(action="import")`) | `cabal install hoogle && hoogle generate` |
| `haskell-language-server` | Not required by this server; probed for completeness. | `ghcup install hls --set` |

## QuickCheck

QuickCheck is a **per-project** dependency, not a global one.
Projects scaffolded by `ghc_project(action="create")` ship
`QuickCheck >= 2.14` in the test-suite's `build-depends` (since
1.0.0), so the property loop works with no extra step. For an
existing project: `ghc_deps(action="add", package="QuickCheck")`.

## PATH auto-bootstrap

At boot the server prepends the standard toolchain directories to
`PATH` (idempotent, order-preserving):

```
~/.ghcup/bin : ~/.cabal/bin : ~/.local/bin : /opt/homebrew/bin : /usr/local/bin
```

ghcup's default install locations work without any configuration.
`--doctor` applies the same augmentation before probing, so its
verdict matches what the live server sees.

## Environment variables

All timeout knobs are optional; invalid values silently fall back
to the default.

| Variable | Effect | Default |
|---|---|---|
| `HASKELL_PROJECT_DIR` | Anchor the project root explicitly (otherwise the first `ghc_project` create/switch sets it; relative paths from the client resolve against the server's startup cwd). | — |
| `HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS` | hoogle subprocess budget | 10 |
| `HASKELL_FLOWS_HLINT_TIMEOUT_SECS` | hlint subprocess budget | 60 |
| `HASKELL_FLOWS_FORMAT_TIMEOUT_SECS` | formatter subprocess budget | 30 |
| `HASKELL_FLOWS_CABAL_CHECK_TIMEOUT_SECS` | `cabal check` budget | 30 |
| `HASKELL_FLOWS_VERSION_TIMEOUT_SECS` | `tool --version` probe budget | 3 |
| `HASKELL_FLOWS_CHECK_PROJECT_TIMEOUT_SECS` | full project check budget | 600 |
| `HASKELL_FLOWS_OUTER_CEILING_MINS` | hard ceiling for any single tool call | 10 |
| `HASKELL_FLOWS_AUDIT` | Enable the tool-call audit log (JSON lines). | off |

## Verify in two steps

1. `haskell-flows-mcp --doctor` — hard requirements ✓, exit 0.
2. From your MCP client: `ghc_session(action="toolchain")` — the
   same probes through the live transport, confirming the wiring
   end-to-end.

## Platforms

- **darwin-arm64** — verified end-to-end (development platform).
- **linux-x64** — ships as a CI-built release binary.
- **Windows** — not supported.
