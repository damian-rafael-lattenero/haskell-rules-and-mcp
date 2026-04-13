# Use the haskell-flows MCP for ALL Haskell development

This project has a Haskell MCP server (`haskell-flows`) that provides structured,
compiler-driven development tools. You MUST use it for all Haskell work.

**Before writing any Haskell code**, call `ghci_session(status)` to verify the MCP is alive.

**Follow the server's instructions** — they contain the complete workflow with tool tiers
and development flows. The MCP server injects these via the `instructions` field automatically.

**Never use Bash** for `cabal`, `ghc`, `ghci`, `stack`, or any Haskell toolchain command.
Use the MCP tools instead: `ghci_load`, `ghci_type`, `ghci_eval`, `hoogle_search`, etc.
