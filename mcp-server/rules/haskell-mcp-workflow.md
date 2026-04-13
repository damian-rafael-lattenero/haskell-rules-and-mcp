# Haskell MCP Workflow

## PRIME DIRECTIVE

MCP-driven development. Every decision goes through an MCP tool.
The compiler's structured output drives development, not pre-existing knowledge.

---

## MODE

Select your mode via `ghci_mode(mode="...")` on first tool use.
Switch at any time with `ghci_mode`.

| Mode | Best for | Mandatory tools |
|------|----------|----------------|
| `guided` | Beginners, unfamiliar codebases, complex type-level code | load, type, hole_fits, suggest, quickcheck |
| `medium` | Intermediate devs, familiar with Haskell but new to this codebase | load, quickcheck, suggest (first pass) |
| `expert` | Experienced Haskell devs who know the types | load, eval, quickcheck |

---

## ALWAYS MANDATORY (all modes)

- `ghci_load` after every `.hs` edit — **no exceptions**
- `ghci_quickcheck` incrementally when laws become testable AND at module-complete
- Zero tolerance for warnings — fix every `warningAction` immediately
- `ghci_arbitrary` for new data types — don't write Arbitrary instances by hand

---

## WHEN → TOOL → WHY

### Session startup
| When | Tool | Why |
|------|------|-----|
| Start of session | `ghci_session(status)` | Verify MCP is alive |
| Switch project | `ghci_switch_project(name)` | Change active project |
| After switch | `ghci_load(load_all=true)` | Verify all modules compile |

### New project / module
| When | Tool | Why |
|------|------|-----|
| Created .cabal | `ghci_scaffold` → `ghci_session(restart)` | Create module stubs, restart GHCi |
| New module with data types | `ghci_arbitrary(type_name="...")` | Generate Arbitrary instances |
| Before implementing functions | `ghci_suggest(module_path="...")` | See hole fits or analyze types [guided/medium first pass] |

### Implementing functions (the core loop)
| When | Tool | Why |
|------|------|-----|
| Wrote/edited a function body | `ghci_load(diagnostics=true)` | Compile, see errors/warnings/holes |
| Type errors | `ghci_type` on subexpressions | Find the type divergence |
| "Not in scope" | `ghci_add_import("name")` | Resolve missing import |
| Need a function by type | `hoogle_search("a -> b -> c")` | Find it in the ecosystem |
| Want to understand a name | `ghci_info("name")` | See definition, instances, module |
| After successful compilation | `ghci_eval("funcName sampleArg")` | Test behavior with sample input |
| A law becomes testable | `ghci_quickcheck(property, incremental=true)` | Test the law immediately |
| Multiple properties to test | `ghci_quickcheck_batch(properties=[...])` | Test all in one call |
| Logic error (types OK, wrong result) | `ghci_trace(expression, trace_points=[...])` | Debug intermediate values |
| Property suggests needed | `ghci_quickcheck(property="suggest", function_name="...")` | Discover testable laws |
| Lost track of progress | `ghci_workflow(action="next")` | See what step comes next |

### Module complete gate (MANDATORY before next module)
| When | Tool | Why |
|------|------|-----|
| All functions implemented | `ghci_quickcheck` / `ghci_quickcheck_batch` | Test COMPLETE algebraic contract |
| After quickcheck passes | `ghci_check_module(module_path="...")` | Review API summary |
| After review | `ghci_lint(module_path="...")` | Code quality pass |
| After lint | `ghci_format(module_path="...", write=true)` | Formatting pass |

### Dependencies / modules
| When | Tool | Why |
|------|------|-----|
| Edited .cabal | `ghci_scaffold` (if new module) | Create stubs |
| After .cabal changes | `ghci_session(restart)` | Pick up new deps |
| Verify clean state | `ghci_load(load_all=true)` | Everything compiles |

---

## ERROR RESOLUTION

| Situation | Tool |
|-----------|------|
| "Not in scope" | `ghci_add_import("name")` |
| Type mismatch | `ghci_type` on subexpressions |
| "No instance" | `ghci_info("Type")` to see instances |
| "No Arbitrary" | `ghci_arbitrary(type_name="Type")` |
| Incomplete patterns | `ghci_info("Type")` for constructors |
| Logic error (types OK) | `ghci_trace(expr, trace_points=[...])` |
| Don't know where to start | `ghci_suggest(module_path="...")` |
| 2+ failed attempts | `= undefined` → `ghci_type` on context → build bottom-up |

## WARNING AUTO-FIX

| Category | Action |
|----------|--------|
| `unused-import` | Remove or narrow the import |
| `missing-signature` | Add the type from `suggestedAction` |
| `incomplete-patterns` | `ghci_info` for constructors, add missing cases |
| `unused-binding` | Prefix with `_` or remove |
| `name-shadowing` | Rename the inner binding |
| `typed-hole` | Read the fits, implement |

---

## GUIDED MODE — Additional Requirements

In guided mode, these steps are also mandatory:
- Replace `= undefined` with `= _` (hole phase) before implementing
- `ghci_load(diagnostics=true)` to read hole analysis before writing code
- `ghci_type("functionName")` after implementation to verify type
- Follow every step in the core loop in order

Fall back to guided from expert when:
- First implementation attempt fails with type errors
- Function involves unfamiliar types or typeclasses
- You've been stuck for 2+ attempts

---

## FORBIDDEN

- Multiple `.hs` edits between `ghci_load` calls — **all modes**
- Using Bash for ANY Haskell toolchain operation — **all modes**
- Moving to next module without `ghci_quickcheck` — **all modes**
- Skipping incremental QuickCheck when a law becomes testable — **all modes**
- Writing Arbitrary instances by hand when `ghci_arbitrary` can generate them — **all modes**
- "I'll fix warnings later" — fix them NOW — **all modes**
- MCP tool fails → falling back to Bash — diagnose → retry → `mcp_restart` → ask user — **all modes**
