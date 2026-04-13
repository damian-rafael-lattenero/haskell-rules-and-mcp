# MCP-First: All Haskell Operations Go Through the MCP Server

## Mandatory Rule
For **ALL** Haskell operations in this project, use the `haskell-ghci` MCP tools.
**NEVER** run `cabal`, `ghc`, `ghci`, `stack`, or any Haskell toolchain command directly via Bash.

## Tool Mapping

| Operation | MCP Tool | NOT this |
|---|---|---|
| Create project scaffolding | `ghci_scaffold` | ~~manual file creation + Bash cabal~~ |
| Switch between playground projects | `ghci_switch_project` | ~~cd + Bash cabal~~ |
| Build the project | `cabal_build` | ~~Bash: cabal build~~ |
| Load/reload modules | `ghci_load` | ~~Bash: ghci, cabal repl~~ |
| Evaluate expressions | `ghci_eval` | ~~Bash: ghci -e~~ |
| Type-check expressions | `ghci_type` | ~~Bash: ghci :t~~ |
| Get info on names | `ghci_info` | ~~Bash: ghci :i~~ |
| Find definitions | `ghci_goto` | ~~Bash: ghci :i~~ |
| Search by type signature | `hoogle_search` | ~~Bash: hoogle~~ |
| Find references | `ghci_references` | ~~grep~~ |
| Rename across project | `ghci_rename` | ~~sed/find-replace~~ |
| Format code | `ghci_format` | ~~Bash: ormolu/fourmolu~~ |
| Lint code | `ghci_lint` | ~~Bash: hlint~~ |
| Run QuickCheck properties | `ghci_quickcheck` | ~~Bash: cabal test~~ |
| Restart GHCi session | `ghci_session(action="restart")` | ~~kill process + Bash ghci~~ |
| Restart MCP server | `mcp_restart` | ~~manual restart~~ |

## Project Bootstrap Sequence

When creating a new project or switching to an existing one:

1. **New project**: Edit the `.cabal` file (Write tool), then `ghci_scaffold` to create module stubs, then `ghci_session(action="restart")` to pick it up, then `ghci_load(load_all=true)` to verify.
2. **Existing project**: `ghci_switch_project(project="name")` to switch, then `ghci_load(load_all=true)` to verify.
3. **List available projects**: `ghci_switch_project()` with no arguments.

## Why This Rule Exists
The MCP server provides structured, parsed output (errors with codes, warnings with fix actions, types, etc.) that enables the automation loop. Raw Bash output is unstructured text that breaks the edit→compile→fix cycle. Using Bash also bypasses the persistent GHCi session, losing incremental compilation benefits.
