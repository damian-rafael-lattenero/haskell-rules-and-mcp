# Use the haskell-flows MCP for ALL Haskell development

This project has a Haskell MCP server (`haskell-flows`) that provides structured,
compiler-driven development tools. You MUST use it for all Haskell work.

**Before writing any Haskell code**, call `ghci_session(status)` to verify the MCP is alive.

**Follow the server's instructions** — they contain the complete workflow with tool tiers
and development flows. The MCP server injects these via the `instructions` field automatically.

**Never use Bash** for `cabal`, `ghc`, `ghci`, `stack`, or any Haskell toolchain command.
Use the MCP tools instead: `ghci_load`, `ghci_type`, `ghci_eval`, `hoogle_search`, etc.

## Mandatory tool usage at key points

| When | Tool | Why |
|------|------|-----|
| New data types in stub phase | `ghci_arbitrary(type_name="...")` | Generate Arbitrary instances — don't write by hand |
| Before implementing functions | `ghci_suggest(module_path="...")` | See hole fits for ALL undefined functions at once |
| After each algebraic function | `ghci_quickcheck(property="suggest", function_name="...")` | Discover testable laws incrementally |
| When a law is testable | `ghci_quickcheck(property, incremental=true)` | Test laws immediately, don't defer to module-end |
| Logic error (types OK, wrong result) | `ghci_trace(expression, trace_points=[...])` | Debug intermediate values |
| Lost track of progress | `ghci_workflow(action="next")` | See what step comes next |
