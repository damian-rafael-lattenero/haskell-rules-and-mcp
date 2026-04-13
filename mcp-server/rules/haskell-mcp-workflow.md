# Haskell MCP Workflow

## PRIME DIRECTIVE

MCP-driven development. Every decision goes through an MCP tool.
The compiler's structured output drives development, not pre-existing knowledge.

---

## CONTEXTUAL GUIDANCE

The MCP provides automatic `_guidance` in tool responses based on the actual state
of your modules. No setup required — just follow the guidance when it appears.

The `_guidance` array in responses tells you what to do next based on:
- Whether stubs exist (→ run `ghci_suggest`)
- Whether Arbitrary instances are missing (→ run `ghci_arbitrary`)
- Whether functions are untested (→ run `ghci_quickcheck`)
- Whether warnings are pending (→ fix them)
- Whether edits haven't been compiled (→ run `ghci_load`)

---

## ALWAYS MANDATORY

- `ghci_load` after every `.hs` edit — **no exceptions**
- `ghci_quickcheck` incrementally when laws become testable AND at module-complete
- Zero tolerance for warnings — fix every `warningAction` immediately
- `ghci_arbitrary` for new data types — don't write Arbitrary instances by hand
- Follow `_guidance` in tool responses — it's always context-aware

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
| Created .cabal (with types) | `ghci_scaffold(signatures={"Mod": ["f :: T", "data D = ..."]})` → `ghci_session(restart)` | Create typed stubs (data types verbatim, functions with `= undefined`, cross-module imports auto-generated) |
| New module with data types | `ghci_arbitrary(type_name="...")` | Generate Arbitrary instances |
| Before implementing functions | `ghci_suggest(module_path="...")` | See hole fits or analyze types |

### Implementing functions (the core loop)
| When | Tool | Why |
|------|------|-----|
| Wrote/edited a function body | `ghci_load(diagnostics=true)` | Compile, see errors/warnings/holes + `importSuggestions` |
| Type errors | `ghci_type` on subexpressions | Find the type divergence |
| "Not in scope" | Check `importSuggestions` in load response, or `ghci_add_import("name")` | Resolve missing import |
| Need a function by type | `hoogle_search("a -> b -> c")` | Find it in the ecosystem |
| Want to understand a name | `ghci_info("name")` | See definition, instances, module |
| After successful compilation | `ghci_eval("funcName sampleArg")` | Test behavior with sample input |
| A law becomes testable | `ghci_quickcheck(property, incremental=true, module="src/X.hs")` | Test the law immediately (module= for accurate tracking) |
| Multiple properties to test | `ghci_quickcheck_batch(properties=[...], module="src/X.hs")` | Test all in one call |
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

### Regression testing
| When | Tool | Why |
|------|------|-----|
| Start of session on existing project | `ghci_regression(action="run")` | Re-run all saved QC properties |
| After major changes | `ghci_regression(module="src/Mod.hs")` | Verify module contracts still hold |
| Want to see what's tested | `ghci_regression(action="list")` | List all persisted properties |

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
| "Not in scope" | Check `importSuggestions` in load response, or `ghci_add_import("name")` |
| Type mismatch | `ghci_type` on subexpressions |
| "No instance" | `ghci_info("Type")` to see instances |
| "No Arbitrary" | `ghci_arbitrary(type_name="Type")` |
| Incomplete patterns | `ghci_info("Type")` for constructors |
| Logic error (types OK) | `ghci_trace(expr, trace_points=[...])` |
| Don't know where to start | `ghci_suggest(module_path="...")` |
| ghci_suggest empty | Add `= undefined` stubs first, then re-run — check `_nextStep` in response |
| "Not in scope" after load | Already auto-resolved — `ghci_load` brings all deps into scope |
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

## FORBIDDEN

- Multiple `.hs` edits between `ghci_load` calls
- Using Bash for ANY Haskell toolchain operation
- Moving to next module without `ghci_quickcheck`
- Skipping incremental QuickCheck when a law becomes testable
- Writing Arbitrary instances by hand when `ghci_arbitrary` can generate them
- "I'll fix warnings later" — fix them NOW
- MCP tool fails → falling back to Bash — diagnose → retry → `mcp_restart` → ask user
