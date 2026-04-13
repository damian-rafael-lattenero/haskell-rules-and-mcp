# MCP-First: All Haskell Operations Go Through the MCP Server

## PRE-FLIGHT CHECK (MANDATORY — run before ANY development)

Before writing a single line of Haskell, verify the MCP is healthy:

```
1. ghci_session(action="status")
   → alive=true?  Good, continue.
   → alive=false?  → ghci_session(action="restart")
   → restart fails? → mcp_restart → retry from step 1
   → still fails?  → STOP. Ask the user.

2. ghci_switch_project()  (no arguments = list projects)
   → Returns project list? Good, you know what's available.
   → Returns error?  → mcp_restart → retry
```

**Do NOT skip this.** If the session is dead and you start writing code, you will drift into manual mode without the compilation gate. The pre-flight catches this before it happens.

---

## CARDINAL RULE: The Compilation Gate

**EVERY** Write/Edit to a `.hs` file **MUST** be followed by `ghci_load` **BEFORE** any other Write/Edit to a `.hs` file.

No exceptions. No batching. No "I'll compile after I finish this module."

The sequence is ALWAYS: **Edit .hs → ghci_load → read result → act on it → next edit**

This is the single most important rule in this project. If you follow nothing else, follow this.

### Self-Check

**Before every Write/Edit to a `.hs` file, ask yourself:**
- Did I call `ghci_load` after my last `.hs` edit? If NO → **STOP**. Compile first.
- Am I writing more than 1 function body? If YES → **STOP**. Split it.
- Is the MCP session alive? If UNKNOWN → **STOP**. Run pre-flight.

---

## FORBIDDEN PATTERNS

These are **hard failures**. If you catch yourself doing any of these, STOP and correct course.

1. Writing a complete module implementation in one Write call
2. Writing multiple `.hs` files before compiling any of them
3. Writing more than 1 function body between `ghci_load` calls
4. Skipping `ghci_scaffold` when creating a new project
5. Skipping `ghci_session(action="restart")` after changing `.cabal`
6. Using Bash for ANY Haskell toolchain operation (`cabal`, `ghc`, `ghci`, `stack`)
7. Writing implementation code without first compiling type signatures as stubs
8. **Falling back to manual operations when an MCP tool fails** — diagnose and fix, never bypass

---

## MCP TOOL FAILURE PROTOCOL

When ANY MCP tool fails, follow this sequence. **NEVER** fall back to doing the operation manually.

```
MCP tool fails
  → Read the error message carefully
  → Is it a known issue? (missing source files, wrong project, etc.)
    → Fix the root cause (create files, switch project, etc.)
    → Retry the SAME MCP tool
  → Is GHCi dead?
    → ghci_session(action="restart")
    → Retry the tool
  → Is the MCP server unresponsive?
    → mcp_restart
    → Retry from pre-flight check
  → Still failing after 2 retries?
    → STOP. Ask the user. Show the error.
```

**The worst thing you can do is silently switch to Bash/manual mode.** The developer chose MCP-driven development for a reason. If the tool is broken, they need to know.

---

## MANDATORY BOOTSTRAP SEQUENCE

### New Project
Each step MUST complete successfully before the next. No skipping.

1. Run pre-flight check (above)
2. Write the `.cabal` file (Write tool) — this is the ONLY manual Write before scaffold
3. `ghci_scaffold` — creates stub files for all modules listed in `.cabal`
   - If scaffold fails because it's pointing at a different project: the `.cabal` write in step 2 should be in the target project directory. Scaffold reads the `.cabal` from the current `projectDir`.
   - If `projectDir` is wrong: `ghci_switch_project("name")` may fail (no sources yet). In that case, `ghci_session(action="restart")` to re-detect, then retry scaffold.
4. `ghci_session(action="restart")` — picks up the new project and scaffolded stubs
5. `ghci_load(load_all=true)` — verify clean compilation of stubs
6. NOW you may start implementing (one function at a time, per the Compilation Gate)

**If any step fails**: diagnose, fix, retry THAT step. Do NOT skip ahead.

### Existing Project
1. Run pre-flight check
2. `ghci_switch_project(project="name")` — switch context
3. `ghci_load(load_all=true)` — verify current state compiles

### List Available Projects
1. `ghci_switch_project()` — with no arguments

---

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
| Find definitions | `ghci_goto` | ~~grep/find~~ |
| Search by type signature | `hoogle_search` | ~~Bash: hoogle~~ |
| Find references | `ghci_references` | ~~grep~~ |
| Rename across project | `ghci_rename` | ~~sed/find-replace~~ |
| Format code | `ghci_format` | ~~Bash: ormolu/fourmolu~~ |
| Lint code | `ghci_lint` | ~~Bash: hlint~~ |
| Run QuickCheck properties | `ghci_quickcheck` | ~~Bash: cabal test~~ |
| Restart GHCi session | `ghci_session(action="restart")` | ~~kill process + Bash ghci~~ |
| Restart MCP server | `mcp_restart` | ~~manual restart~~ |

## Why This Rule Exists

The MCP server provides structured, parsed output (errors with codes, warnings with fix actions, types, etc.) that enables the automation loop. Raw Bash output is unstructured text that breaks the edit-compile-fix cycle. Using Bash also bypasses the persistent GHCi session, losing incremental compilation benefits.

**The structured output is the point.** `ghci_load` returns `errors[]` with GHC error codes, `warningActions[]` with categories and suggested fixes, `holes[]` with relevant bindings and valid fits. This structured data drives the automated development loop. Bash gives you a wall of text.
