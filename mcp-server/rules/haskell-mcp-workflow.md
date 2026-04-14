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
- Whether module-complete gates are missing (→ run `ghci_check_module`, `ghci_lint`, `ghci_format`)
- Whether all modules are gate-complete (→ run `ghci_quickcheck_export`, then `cabal_build`)

**Guidance is state-aware:** once QuickCheck properties pass, the "untested" hint disappears
automatically. Once gates are complete, the session-close hint appears.

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
| Start of session | `ghci_session(status)` | Verify MCP is alive. When no active session, response includes `_hint` listing available projects. |
| Switch project | `ghci_switch_project(project="name")` | Change active project — also auto-scaffolds missing source files on switch. Uses `project=` parameter (not `name=`). Rolls back safely if GHCi fails to start. |
| After switch | `ghci_load(load_all=true)` | Verify all modules compile |
| Lost / unsure what to do | `ghci_workflow(action="help")` | Context-aware next steps with `suggested_tools` and `reasoning` |

### New project / module
| When | Tool | Why |
|------|------|-----|
| Starting from scratch | `ghci_init(name, modules, deps)` | Generate .cabal + directory structure |
| With test target | `ghci_init(name, modules, deps, test_suite=true)` | Also generates `test-suite` stanza in .cabal + `test/Spec.hs` scaffold |
| Starting with Stack | `ghci_init(name, modules, deps, build_tool="stack")` | Also generates `stack.yaml` with LTS resolver |
| Need to add a dependency | `ghci_deps(action="add", package="containers")` | Edits `.cabal` build-depends — no manual editing |
| Need to add dep with version | `ghci_deps(action="add", package="text", version=">= 2.0")` | Inserts `text >= 2.0` in build-depends |
| Check current dependencies | `ghci_deps(action="list")` | Shows all build-depends with version constraints |
| Remove a dependency | `ghci_deps(action="remove", package="old-pkg")` | Removes from build-depends safely |
| Visualize module imports | `ghci_deps(action="graph")` | Import graph with cycle detection and orphan analysis |
| Created .cabal, need typed stubs | `ghci_scaffold(signatures={"Mod": ["f :: T", "data D = ..."]})` → `ghci_session(restart)` | Create typed stubs with `= undefined` bodies for ghci_suggest hole-fit mode. **Note:** `ghci_switch_project` already auto-scaffolds empty files on switch — only call `ghci_scaffold` when you specifically want typed `= undefined` stubs. |
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
| Properties test a different module | `ghci_quickcheck_batch(properties=[...], module_path="src/Syntax.hs", tests_module="src/Eval.hs")` | Set `tests_module` so regression filters by the module being tested, not by where Arbitrary lives |
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
| After quickcheck passes | `ghci_check_module(module_path="...")` | Review API summary — `_guidance` will prompt this automatically |
| After review | `ghci_lint(module_path="...")` | Code quality pass. **Gate completes only when hlint runs** — if hlint is missing it auto-installs in background; retry after 1–5 min. GHC-warnings fallback runs immediately but does NOT complete the gate. |
| After lint | `ghci_format(module_path="...", write=true)` | Formatting pass. **Gate completes only when fourmolu/ormolu runs** — if missing, auto-installs in background. Basic whitespace fallback does NOT complete the gate. |

**Note:** `_guidance` in `ghci_load` responses guides you through each gate individually
once properties pass. Each gate has its own hint that disappears when that gate is complete.

### Session complete gate (after ALL modules pass all gates)
| When | Tool | Why |
|------|------|-----|
| All modules: gates complete | `ghci_quickcheck_export(output_path="test/Spec.hs")` | Generate persistent test file from saved properties |
| After export | `cabal_build` | Verify full GHC compilation (not just GHCi interpreted) |
| After build | Commit | Persist the work |

**Note:** `_guidance` will emit a session-close hint once all tracked modules have all three
gates complete (checkModule, lint, format). Follow it to close the session properly.

### Regression testing
| When | Tool | Why |
|------|------|-----|
| Start of session on existing project | `ghci_regression(action="run")` | Re-run all saved QC properties |
| After major changes | `ghci_regression(module="src/Mod.hs")` | Verify module contracts still hold (uses `tests_module` for filtering) |
| Want to see what's tested | `ghci_regression(action="list")` | List all persisted properties grouped by semantic module |
| Wondering how to save properties | `ghci_regression(action="save")` | Returns explanation: auto-saved on pass, no manual action needed |

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
| Check if HLS is installed | `ghci_hls(action="available")` | Returns `{ available: bool, version? }`. If missing, **auto-installs HLS via ghcup in background**. Returns `{ installing: true }` — retry after 1–5 min. |
| Get type info at a position | `ghci_hls(action="hover", module_path="src/X.hs", line=5, character=3)` | LSP hover: exact type at cursor. Auto-installs HLS if missing. |

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

## AUTO-INSTALLATION

The MCP automatically installs optional tools in the background when they are first needed.
You do **not** need to run `cabal install` or `ghcup install` manually.

| Tool | Trigger | Install command | Wait time |
|------|---------|----------------|-----------|
| `hlint` | `ghci_lint` called without hlint | `cabal install hlint` | 2–5 min |
| `fourmolu` | `ghci_format` called without formatter | `ghcup install fourmolu` (fallback: cabal) | 1–3 min |
| `ormolu` | Only if fourmolu also unavailable | `cabal install ormolu` | 2–5 min |
| `hls` | `ghci_hls(action="available/hover")` | `ghcup install hls` | 2–5 min |

**Response pattern when installing:**
```json
{ "installing": true, "_message": "hlint not found — installing now... Retry in 1–5 min." }
```

**What to do:** Wait and retry the same tool call. State is tracked across calls — no need to restart.

**If installation fails:** Response includes `{ "failed": true, "error": "...", "manualInstallHint": "cabal install hlint" }`.

**Gates and auto-install:** `ghci_lint` and `ghci_format` gates are NOT completed by fallback mode.
They complete only when the real tool (hlint / fourmolu) has run successfully.
Call the tool again after installation to complete the gate.

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

### `tests_module` — semantic tagging for regression filtering

`module_path` / `module` is the **load context** (which module GHCi loads to run the property).
`tests_module` is the **semantic target** (which module the property is actually testing).

**Why this matters:** In multi-module projects, `Arbitrary` instances often live in a
`Syntax` or `Types` module. If you run all properties from `module_path="src/Syntax.hs"`,
every property gets tagged to `Syntax` — and `ghci_regression(module="src/Eval.hs")` returns
zero results even though you tested Eval thoroughly.

**Pattern:**
```
ghci_quickcheck_batch(
  properties=["\\e -> eval [] (Lit n) == Right n", ...],
  module_path="src/Syntax.hs",    -- load context (where Arbitrary lives)
  tests_module="src/Eval.hs"      -- semantic target (what we're testing)
)
```

`ghci_regression(module="src/Eval.hs")` then correctly finds those properties.

### `ghci_init` with `test_suite=true`

Generates a `test-suite` stanza in the `.cabal` file and creates `test/Spec.hs`:

```
ghci_init(name="my-lib", modules=["Lib"], test_suite=true)
```

Use `ghci_quickcheck_export(output_path="test/Spec.hs")` after development to
populate the test file with saved QuickCheck properties.

### `ghci_switch_project` — parameter and behavior

**Correct parameter name: `project=` (not `name=`).**

```
ghci_switch_project(project="expr-eval")   ✅ correct
ghci_switch_project(name="expr-eval")      ❌ wrong — parameter ignored
```

`ghci_switch_project` **automatically creates empty source files** for any module
listed in `.cabal` that doesn't have a source file yet. You do NOT need to call
`ghci_scaffold` after switching — empty stubs are already in place.

Only call `ghci_scaffold(signatures={...})` if you want typed `= undefined` stubs
for use with `ghci_suggest` hole-fit mode.

**Safe rollback:** If GHCi fails to start in the target project (e.g. broken `.cabal`),
the server automatically rolls back to the previous project. No manual recovery needed.
Projects with empty or invalid `.cabal` files are excluded from the project list entirely.

### `ghci_regression(action="save")`

Calling `ghci_regression(action="save")` returns an explanation:
properties are **auto-saved** when they pass via `ghci_quickcheck` or
`ghci_quickcheck_batch`. No manual save action exists or is needed.

### `ghci_format` — auto-install and gate behavior

When fourmolu/ormolu are missing, `ghci_format` **auto-installs fourmolu** in background
(via `ghcup install fourmolu`, falling back to `cabal install fourmolu`).

While installing, a basic whitespace/tabs fallback runs immediately and returns
`{ fallback: true, _formatter_status: "installing" }`. This does **NOT** complete the
format gate — retry after installation to run the real formatter and complete the gate.

The `format_tool` field indicates which formatter ran:
- `"fourmolu"` or `"ormolu"` → gate completed
- absent (fallback mode) → gate NOT completed

### `ghci_load` and `_ghci_quirks`
`ghci_load` isolates GHCi session artifacts (like `GHC-32850 -Wmissing-home-modules`)
into a separate `_ghci_quirks` field with `_quirks_note: "GHCi session artifacts (not real issues)"`.
The `raw` field contains only real compilation output. Ignore `_ghci_quirks` entirely.

**`ghci_check_module` also suppresses GHC-32850** — it no longer appears in the `warnings`
array. If you see `warnings: []` and `summary.warnings: 0`, the module is genuinely clean.

### `ghci_deps` protects `base`
`ghci_deps(action="remove", package="base")` is blocked — `base` is a protected
core dependency. All other packages can be removed freely.

### `ghci_refactor` is text-based
`rename_local` and `extract_binding` work by text substitution (word-boundary aware).
Always run `ghci_load(diagnostics=true)` immediately after to verify the result compiles.

### `ghci_hls` — auto-install

`ghci_hls(action="available")` and `ghci_hls(action="hover")` **auto-install HLS** via
`ghcup install hls` when it is not found. No manual user action needed.

Response when installing: `{ available: false, installing: true }` — retry after 2–5 minutes.
Response when done: `{ available: true, version: "..." }`.

For compilation diagnostics without HLS, use `ghci_load(diagnostics=true)` — it doesn't
require HLS and is always available.

### `ghci_quickcheck_export` — trivial filter and qualified imports

`ghci_quickcheck_export` automatically:

1. **Filters trivially-true properties** — `\x -> True`, `\_ -> True`, `const True` are
   dropped from the exported file. If ALL properties are trivial, the export fails with a
   clear error. The response includes `droppedTrivial: N` when properties were dropped.

2. **Detects and adds qualified imports** — if any property references `Map.*`, `Set.*`,
   `Seq.*`, `Vector.*`, `Text.*`, or `NonEmpty.*`, the corresponding qualified imports are
   added automatically to the generated `Spec.hs`.

**Example:** a property like `\n -> eval Map.empty (Lit n) == Right n` will cause
`import qualified Data.Map.Strict as Map` to be included in the output file.

### `ghci_flags` is session-only
Flags set with `ghci_flags(action="set", flags="...")` apply only to the current GHCi session.
To persist an extension, add it to `default-extensions` in the `.cabal` file,
then run `ghci_session(restart)` to pick it up.
