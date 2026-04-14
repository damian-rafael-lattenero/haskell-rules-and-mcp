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

**Lost? Not sure what to do next?** → `ghci_workflow(action="help")` returns
`suggested_tools`, `reasoning`, and `steps` based on the current session state.

---

## ALWAYS MANDATORY

- `ghci_load` after every `.hs` edit — **no exceptions**
- `ghci_quickcheck` incrementally when laws become testable AND at module-complete
- Zero tolerance for warnings — fix every `warningAction` immediately
- `ghci_arbitrary` for new data types — don't write Arbitrary instances by hand
- `ghci_regression(action="run")` at start of session on existing projects — verify saved properties still pass
- Follow `_guidance` in tool responses — it's context-aware and verified against GHCi state

---

## WHEN → TOOL → WHY

### Session startup
| When | Tool | Why |
|------|------|-----|
| Start of session | `ghci_session(status)` | Verify MCP is alive |
| Switch project | `ghci_switch_project(name)` | Change active project |
| After switch | `ghci_load(load_all=true)` | Verify all modules compile |
| Lost / unsure what to do | `ghci_workflow(action="help")` | Context-aware next steps with `suggested_tools` and `reasoning` |

### New project / module
| When | Tool | Why |
|------|------|-----|
| Starting from scratch | `ghci_init(name, modules, deps)` | Generate .cabal + directory structure |
| Starting with Stack | `ghci_init(name, modules, deps, build_tool="stack")` | Also generates `stack.yaml` with LTS resolver |
| Need to add a dependency | `ghci_deps(action="add", package="containers")` | Edits `.cabal` build-depends — no manual editing |
| Need to add dep with version | `ghci_deps(action="add", package="text", version=">= 2.0")` | Inserts `text >= 2.0` in build-depends |
| Check current dependencies | `ghci_deps(action="list")` | Shows all build-depends with version constraints |
| Remove a dependency | `ghci_deps(action="remove", package="old-pkg")` | Removes from build-depends safely |
| Visualize module imports | `ghci_deps(action="graph")` | Import graph with cycle detection and orphan analysis |
| Created .cabal | `ghci_scaffold` → `ghci_session(restart)` | Create module stubs, restart GHCi |
| Created .cabal (with types) | `ghci_scaffold(signatures={"Mod": ["f :: T", "data D = ..."]})` → `ghci_session(restart)` | Create typed stubs (data types verbatim, functions with `= undefined`, cross-module imports auto-generated) |
| New module with data types | `ghci_arbitrary(type_name="...")` | Generate Arbitrary instances |
| Before implementing functions | `ghci_suggest(module_path="...")` | See hole fits or analyze types |

### Implementing functions (the core loop)
| When | Tool | Why |
|------|------|-----|
| Wrote/edited a function body | `ghci_load(diagnostics=true)` | Compile, see errors/warnings/holes + `importSuggestions` |
| Module has typed holes (`_` or `_name`) | `ghci_hole(module_path="src/X.hs")` | Interactive hole exploration: expected type, valid fits, relevant bindings |
| Explore a specific hole | `ghci_hole(module_path="src/X.hs", hole_name="_result")` | Filter to one hole when there are many |
| Type errors | `ghci_type` on subexpressions | Find the type divergence |
| "Not in scope" | Check `importSuggestions` in load response, or `ghci_add_import("name")` | Resolve missing import |
| Need a function by type | `hoogle_search("a -> b -> c")` | Find it in the ecosystem |
| Want to understand a name | `ghci_info("name")` | See definition, instances, module |
| After successful compilation | `ghci_eval("funcName sampleArg")` | Test behavior (includes result type in output) |
| Multiple eval/type checks | `ghci_batch(commands=[":t f", "f 42", ":i Type"])` | Combine multiple GHCi commands in one call |
| A law becomes testable | `ghci_quickcheck(property, incremental=true, module_path="src/X.hs")` | Test the law immediately (`module_path` is the preferred spelling; `module` also works) |
| Multiple properties to test | `ghci_quickcheck_batch(properties=[...], module_path="src/X.hs")` | Test all in one call |
| Logic error (types OK, wrong result) | `ghci_trace(expression, trace_points=[...])` | Debug intermediate values |
| Property suggests needed | `ghci_quickcheck(property="suggest", function_name="...")` | Discover testable laws |
| Lost track of progress | `ghci_workflow(action="next")` | See what step comes next |
| Need richer context-aware help | `ghci_workflow(action="help")` | Returns `suggested_tools`, `reasoning`, and `steps` |
| Want to rename a binding in a module | `ghci_refactor(action="rename_local", module_path="src/X.hs", old_name="foo", new_name="bar")` | Word-boundary safe rename across entire module |
| Want to extract code to a new function | `ghci_refactor(action="extract_binding", module_path="src/X.hs", new_name="helper", lines=[5,8])` | Lifts a line range to a top-level binding |
| Need to enable a GHC extension | `ghci_flags(action="set", flags="-XOverloadedStrings")` | Sets flag for the current session (not persisted to .cabal) |
| Want to see active language settings | `ghci_flags(action="list")` | Shows base language + active modifiers |
| Need to disable a flag | `ghci_flags(action="unset", flags="-XSomething")` | Removes flag from current session |

### Module complete gate (MANDATORY before next module)
| When | Tool | Why |
|------|------|-----|
| All functions implemented | `ghci_quickcheck` / `ghci_quickcheck_batch` | Test COMPLETE algebraic contract |
| After quickcheck passes | `ghci_check_module(module_path="...")` | Review API summary |
| After review | `ghci_lint(module_path="...")` | Code quality pass |
| After lint | `ghci_format(module_path="...", write=true)` | Formatting pass — fixes trailing whitespace, tabs, missing newline even without fourmolu |

### Regression testing
| When | Tool | Why |
|------|------|-----|
| Start of session on existing project | `ghci_regression(action="run")` | Re-run all saved QC properties |
| After major changes | `ghci_regression(module="src/Mod.hs")` | Verify module contracts still hold |
| Want to see what's tested | `ghci_regression(action="list")` | List all persisted properties |

### Exporting tests
| When | Tool | Why |
|------|------|-----|
| Project done | `ghci_quickcheck_export()` | Generate .hs test file from saved properties |
| For CI/CD | `ghci_quickcheck_export(output_path="test/Spec.hs")` | Persistent test suite |

### Dependencies / modules
| When | Tool | Why |
|------|------|-----|
| Need to add a dependency | `ghci_deps(action="add", package="name")` | Edits .cabal directly — never edit .cabal by hand for deps |
| Need to remove a dependency | `ghci_deps(action="remove", package="name")` | Removes safely (protects `base`) |
| List current deps | `ghci_deps(action="list")` | See all packages with version constraints |
| See module import graph | `ghci_deps(action="graph")` | Detects import cycles and orphan modules |
| After add/remove dep | `ghci_session(restart)` | Pick up the new dependency in GHCi |
| Edited .cabal for modules | `ghci_scaffold` (if new module) | Create stubs |
| After .cabal changes | `ghci_session(restart)` | Pick up new deps |
| Verify clean state | `ghci_load(load_all=true)` | Everything compiles |

### Performance analysis
| When | Tool | Why |
|------|------|-----|
| Code seems slow, want quick hints | `ghci_profile(action="suggest", module_path="src/X.hs")` | Static analysis: detects `String` (++) in loops, naive recursion without accumulator, `head`/`fromJust` partial calls |
| Want GHC time profiling | `ghci_profile(action="time", executable="my-exe")` | Runs with `+RTS -p`, shows top cost centres |
| Want heap profiling | `ghci_profile(action="heap", executable="my-exe")` | Runs with `+RTS -hc` |

### HLS integration
| When | Tool | Why |
|------|------|-----|
| Check if HLS is installed | `ghci_hls(action="available")` | Returns `{ available: bool, version? }` — never crashes |
| Get type info at a position | `ghci_hls(action="hover", module_path="src/X.hs", line=5, character=3)` | LSP hover: exact type at cursor (requires HLS installed: `ghcup install hls`) |

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
| Typed hole in code (`_foo`) | `ghci_hole(module_path="...")` — see expected type and valid fits |
| Need to rename a binding | `ghci_refactor(action="rename_local", ...)` — do NOT use find/replace manually |
| Dependency not found in Cabal | `ghci_deps(action="add", package="...")` then `ghci_session(restart)` |

## WARNING AUTO-FIX

| Category | Action |
|----------|--------|
| `unused-import` | Remove or narrow the import |
| `missing-signature` | Add the type from `suggestedAction` |
| `incomplete-patterns` | `ghci_info` for constructors, add missing cases |
| `unused-binding` | Prefix with `_` or remove |
| `name-shadowing` | Rename the inner binding — use `ghci_refactor(action="rename_local", ...)` |
| `typed-hole` | Run `ghci_hole(module_path="...")` to see fits, then implement |

---

## FORBIDDEN

- Multiple `.hs` edits between `ghci_load` calls
- Using Bash for ANY Haskell toolchain operation
- Moving to next module without `ghci_quickcheck`
- Skipping incremental QuickCheck when a law becomes testable
- Writing Arbitrary instances by hand when `ghci_arbitrary` can generate them
- "I'll fix warnings later" — fix them NOW
- MCP tool fails → falling back to Bash — diagnose → retry → `mcp_restart` → ask user
- Manually editing `.cabal` to add/remove dependencies — use `ghci_deps(action="add/remove")` instead
- Using `module="..."` in `ghci_quickcheck` — prefer the canonical `module_path="..."` spelling

---

## PARAMETER NOTES

### `ghci_quickcheck` / `ghci_quickcheck_batch`
Both `module_path` and `module` are accepted and equivalent.
`module_path` is the **preferred spelling** — it matches the convention used by all other tools.
`module_path` takes precedence when both are provided.

```
ghci_quickcheck(property="\\x -> f x == x", module_path="src/MyModule.hs")   ✅ preferred
ghci_quickcheck(property="\\x -> f x == x", module="src/MyModule.hs")        ✅ also works
```

### `ghci_format` fallback
When neither `fourmolu` nor `ormolu` is installed, `write=true` still works:
it applies automatic fixes (trailing whitespace, tabs→spaces, missing final newline)
and returns `{ written: true, fixesApplied: N }`.

### `ghci_deps` protects `base`
`ghci_deps(action="remove", package="base")` is blocked — `base` is a protected
core dependency. All other packages can be removed freely.

### `ghci_refactor` is text-based
`rename_local` and `extract_binding` work by text substitution (word-boundary aware).
Always run `ghci_load(diagnostics=true)` immediately after to verify the result compiles.

### `ghci_hls hover` requires HLS installed
Check availability first: `ghci_hls(action="available")`.
If not installed: `ghcup install hls` (outside MCP — user runs this once).
For compilation diagnostics without HLS, use `ghci_load(diagnostics=true)`.

### `ghci_flags` is session-only
Flags set with `ghci_flags(action="set", flags="...")` apply only to the current GHCi session.
To persist an extension, add it to `default-extensions` in the `.cabal` file,
then run `ghci_session(restart)` to pick it up.
