# haskell-ghci MCP Server

A Model Context Protocol (MCP) server that provides persistent GHCi session management for AI-assisted Haskell development. Works with Claude Code and any MCP-compatible client.

## Features

**16 Tools** for interactive Haskell development:

| Tool | Description |
|------|-------------|
| `ghci_type` | Get the type of an expression (`:t`) |
| `ghci_info` | Get info about a name (`:i`) |
| `ghci_kind` | Get the kind of a type (`:k`) |
| `ghci_eval` | Evaluate an expression |
| `ghci_load` | Load/reload modules with diagnostics, warning categorization, and typed holes |
| `ghci_batch` | Execute multiple GHCi commands in one call |
| `ghci_quickcheck` | Run QuickCheck properties inline |
| `ghci_hole_fits` | Analyze typed holes with fits and bindings |
| `ghci_session` | Check status or restart GHCi |
| `ghci_switch_project` | List or switch between playground projects |
| `ghci_scaffold` | Create module stubs from .cabal |
| `ghci_check_module` | Browse module exports |
| `ghci_diagnostics` | Full diagnostic check (delegates to ghci_load) |
| `cabal_build` | Run cabal build |
| `hoogle_search` | Search Hoogle by name or type |
| `mcp_restart` | Restart the MCP server to pick up code changes |

**3 MCP Resources** with Haskell development rules:

| Resource URI | Description |
|-------------|-------------|
| `rules://haskell/automation` | Edit-compile-fix loop, warning action table, error resolution |
| `rules://haskell/development` | Type-first development, compilation discipline, typed holes |
| `rules://haskell/project-conventions` | Import style, module structure, naming, testing |

**Multi-project support**: Discovers Haskell projects in `playground/` and lets you switch between them at runtime.

## Quick Start

### Prerequisites
- GHC 9.12+ and Cabal 3.12+ (via [GHCup](https://www.haskell.org/ghcup/))
- Node.js 22+

### Setup

```bash
# Clone the repository
git clone <repo-url>
cd haskell-rules-and-mcp

# Build the MCP server
cd mcp-server
npm install
npm run build
cd ..
```

### Configure Claude Code

The `.mcp.json` in the project root configures the server for Claude Code:

```json
{
  "mcpServers": {
    "haskell-ghci": {
      "command": "node",
      "args": ["mcp-server/dist/index.js"],
      "cwd": "/path/to/haskell-rules-and-mcp",
      "env": {
        "HASKELL_PROJECT_DIR": "/path/to/haskell-rules-and-mcp/playground/hindley-milner",
        "PATH": "/opt/homebrew/bin:~/.ghcup/bin:~/.cabal/bin:/usr/local/bin:/usr/bin:/bin"
      }
    }
  }
}
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `HASKELL_PROJECT_DIR` | Path to the Haskell project to load | `playground/hindley-milner` |
| `HASKELL_LIBRARY_TARGET` | Cabal library target override | Auto-detected from .cabal |

## Playground

The `playground/` directory contains Haskell projects for testing. Each subdirectory with a `.cabal` file is a project.

### Current playground: `hindley-milner`

A Hindley-Milner type inference engine with parser combinators:
- Algorithm W with let-polymorphism, letrec, pairs, lists, annotations
- Parser with operator precedence, multi-arg lambda, multi-binding let, typo hints
- 14 QuickCheck properties testing inference, unification, and parser roundtrip

### Adding a new playground project

```bash
mkdir playground/my-project
cd playground/my-project
cabal init --lib --language=GHC2024
# Add your code...
```

Then use `ghci_switch_project(project="my-project")` to switch to it.

## Rules

The MCP server ships with three rule sets exposed as MCP resources. These provide Claude with Haskell-specific development guidance.

Rules are read from `mcp-server/rules/*.md` at runtime. Edit these files directly — no recompilation needed. If a file is missing, embedded fallback content is used.

You can also place project-specific rules in `.claude/rules/` which Claude Code loads automatically.

## Testing

```bash
cd mcp-server

# Unit tests (parsers, pure functions) — ~67 tests, <1s
npm test

# Integration tests (real GHCi session) — ~9 tests, ~5s
npm run test:integration

# E2E tests (full MCP protocol) — ~6 tests, ~3s
npm run test:e2e

# All tests
npm run test:all
```

Integration and E2E tests require GHC installed. They skip gracefully if GHC is not available.

## Development

### Modifying the MCP server

```bash
cd mcp-server
# Edit TypeScript source in src/
npm run build          # or: npx tsc
# Then in Claude Code:
mcp_restart()          # picks up new code without restarting Claude Code
```

### Modifying rules

Edit files in `mcp-server/rules/` directly. Changes are picked up on the next resource read — no restart needed.

### Project structure

```
haskell-rules-and-mcp/
├── .claude/rules/          # Project-specific Claude rules
├── .mcp.json               # MCP server configuration
├── mcp-server/
│   ├── src/                # TypeScript source
│   │   ├── index.ts        # MCP entry point, tool/resource registration
│   │   ├── ghci-session.ts # GHCi child process management
│   │   ├── project-manager.ts  # Multi-project discovery
│   │   ├── parsers/        # GHC output parsers
│   │   ├── tools/          # Tool handlers
│   │   ├── resources/      # MCP resource handlers
│   │   └── __tests__/      # Unit, integration, and E2E tests
│   ├── rules/              # Editable Haskell rule files (served as MCP resources)
│   ├── dist/               # Compiled JavaScript
│   └── package.json
└── playground/
    └── hindley-milner/     # Example Haskell project
```
