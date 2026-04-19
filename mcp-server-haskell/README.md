# haskell-flows-mcp (Haskell rewrite)

Progressive port of [`mcp-server/`](../mcp-server/) from TypeScript to Haskell.

## Status — Phase 1

Scaffolding + one working tool: `ghci_load`.

What's in place:

- Cabal project, executable + library split.
- JSON-RPC 2.0 envelope types (`initialize`, `tools/list`, `tools/call`).
- Stdio transport (newline-delimited JSON).
- `ProjectDir` / `ModulePath` newtypes — **path traversal is impossible by construction**.
- Persistent GHCi child process with the same sentinel protocol as the TS server (`<<<GHCi-DONE-7f3a2b>>>`), wire-compatible.
- Command queue on STM — no silent races between concurrent `tools/call`.
- Regex parser on `regex-tdfa` — linear time, ReDoS-free.

What's out of scope for Phase 1:

- Dual-pass strict/deferred compile (we run a single `:l` for now).
- Typed hole extraction.
- Warning categorization / auto-fix suggestions.
- Workflow state, property store, laws engines, auto-download, HLS, …

## Build

```bash
cd mcp-server-haskell
cabal build
cabal run haskell-flows-mcp  # waits on stdin for JSON-RPC
```

## Run end-to-end manually

```bash
( echo '{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"smoke","version":"0"}}}'; \
  echo '{"jsonrpc":"2.0","method":"initialized"}'; \
  echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'; \
  echo '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"ghci_load","arguments":{"module_path":"src/Foo.hs"}}}'; \
) | cabal run haskell-flows-mcp 2>/dev/null
```

## Module map

| Module | Mirrors (in TS) | Responsibility |
|---|---|---|
| `HaskellFlows.Types` | — (new invariant) | Traversal-safe `ProjectDir` / `ModulePath` |
| `HaskellFlows.Mcp.Protocol` | `@modelcontextprotocol/sdk` | JSON-RPC envelopes |
| `HaskellFlows.Mcp.Transport` | SDK `StdioServerTransport` | Stdin/stdout loop |
| `HaskellFlows.Mcp.Server` | `src/index.ts` | Tool dispatch |
| `HaskellFlows.Ghci.Sentinel` | `src/ghci-session.ts:6` | Sentinel + init script |
| `HaskellFlows.Ghci.Session` | `src/ghci-session.ts` | Child process lifecycle |
| `HaskellFlows.Parser.Error` | `src/parsers/error-parser.ts` | GHC diagnostic parsing |
| `HaskellFlows.Tool.Load` | `src/tools/load-module.ts` | `ghci_load` tool handler |

Later phases will add sibling modules under `HaskellFlows.Tool.*` per tool, and eventually replace the regex parser with `ghc-lib-parser`.
