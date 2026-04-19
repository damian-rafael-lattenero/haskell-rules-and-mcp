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
- Whether all modules are gate-complete (→ run `ghci_quickcheck_export`, then `cabal_test`, `cabal_coverage`, then `cabal_build`)

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

## TOOLCHAIN RESOLUTION POLICY

The MCP resolves optional binaries (`hlint`, `fourmolu`/`ormolu`, `hls`) with:

- Tool resolution order: **host PATH -> bundled binary -> auto-download -> unavailable**
- Bundled-first release scope is currently: `hlint`, `fourmolu`, `ormolu`
- `ghci_lint`, `ghci_format`, and `ghci_hls` responses include:
  - `source` (`host`, `bundled`, or `installed`) when available
  - `binaryPath`
  - `version` (when available)
  - `reason` / `provenance` / `checksumVerified` when relevant
- `ghci_toolchain_status` returns a runtime + release/checksum matrix for reproducible diagnostics.
- `ghci_lint_basic` is a degraded fallback and does **not** satisfy lint gate completion.
- If `ghci_lint` / `ghci_format` are unavailable, `_guidance` is:
  - **recommended/non-blocking** in default mode
  - **blocking** when strict mode is enabled

When triaging issues, always check `source` first to confirm execution path.

---

## WHEN → TOOL → WHY

### Session startup
| When | Tool | Why |
|------|------|-----|
| Start of session | `ghci_session(status)` | Verify MCP is alive. When no active session, response includes `_hint` listing available projects. |
| Switch project | `ghci_switch_project(project="name")` | Change active project — also auto-scaffolds missing source files on switch. Uses `project=` parameter (not `name=`). Rolls back safely if GHCi fails to start. Searches recursively up to depth 3. |
| List all projects | `ghci_switch_project()` | Lists all discoverable projects in workspace (searches recursively). |
| Search in subdirectory | `ghci_switch_project(search_dir="dirnamedbyuser")` | Lists projects only in specific subdirectory. |
| After creating project | `ghci_switch_project(project="name")` | Project cache is auto-refreshed after `ghci_init`, no manual refresh needed. |
| After switch | `ghci_load(load_all=true)` | Verify all modules compile |
| Lost / unsure what to do | `ghci_workflow(action="help")` | Context-aware next steps with `suggested_tools` and `reasoning` |
| Diagnose toolchain issues | `ghci_toolchain_status(include_matrix=true)` | Runtime + release/checksum matrix for lint/format/HLS availability |

### Session Health
| When | Tool | Why |
|------|------|-----|
| Session feels slow/stuck | Check session health | Session may be degraded |
| After timeout error | Auto-recovery | Session auto-restarts on next tool call |
| Suspicious behavior | Manual intervention | Restart if needed |

### New project / module
| When | Tool | Why |
|------|------|-----|
| Starting from scratch | `ghci_init(name, modules, deps, target_path="path/to/project")` | Generate .cabal + directory structure. Includes `containers` and QuickCheck defaults. Use `target_path` to specify location. |
| In workspace root | `ghci_init(name, modules, deps)` | Creates project in workspace root if no `target_path` specified and no current project. |
| In subdirectory | `ghci_init(name, modules, deps, target_path="subdirectory/my-project")` | Creates project in subdirectory. Automatically discoverable by `ghci_switch_project` (searches recursively up to depth 3). |
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
| QuickCheck returned a counterexample | `ghci_trace(...)` | `_guidance` now points to trace-first debugging instead of blind manual evals |
| Property suggests needed | `ghci_quickcheck(property="suggest", function_name="...")` | Discover testable laws |
| Lost track of progress | `ghci_workflow(action="next")` | See what step comes next |
| Need richer context-aware help | `ghci_workflow(action="help")` | Returns `suggested_tools`, `reasoning`, and `steps` |
| Want to rename a binding in a module | `ghci_refactor(action="rename_local", module_path="src/X.hs", old_name="foo", new_name="bar")` | Word-boundary safe rename across entire module |
| Want to extract code to a new function | `ghci_refactor(action="extract_binding", module_path="src/X.hs", new_name="helper", lines=[5,8])` | Lifts a line range to a top-level binding |
| Want to apply suggested export list | `ghci_apply_exports(module_path="src/X.hs")` | Materialize the export list suggested by `ghci_check_module` |
| Want a parser no-crash smoke test | `ghci_fuzz_parser(parser="...")` | Run malformed inputs through a parser and detect crashes |
| Need to enable a GHC extension | `ghci_flags(action="set", flags="-XOverloadedStrings")` | Sets flag for the current session (not persisted to .cabal) |
| Want to see active language settings | `ghci_flags(action="list")` | Shows base language + active modifiers |
| Need to disable a flag | `ghci_flags(action="unset", flags="-XSomething")` | Removes flag from current session |
| Want always-on toolchain from MCP | `ghci_lint`, `ghci_format`, `ghci_hls` | These tools resolve `host -> bundled` and report executable provenance in responses |

### Module complete gate (MANDATORY before next module)
| When | Tool | Why |
|------|------|-----|
| All functions implemented | `ghci_quickcheck` / `ghci_quickcheck_batch` | Test COMPLETE algebraic contract |
| After quickcheck passes | `ghci_check_module(module_path="...")` | Review API summary — `_guidance` will prompt this automatically |
| After review | `ghci_lint(module_path="...")` | Code quality pass. **Gate completes only when hlint runs** (host/bundled/installed). If unavailable, use `ghci_lint_basic` for degraded hints. |
| After lint | `ghci_format(module_path="...", write=true)` | Formatting pass. **Gate completes only when fourmolu/ormolu runs** (host/bundled/installed). In strict mode, unavailable formatter remains blocking. |

**Note:** `_guidance` in `ghci_load` responses guides you through each gate individually
once properties pass. Each gate has its own hint that disappears when that gate is complete.

### Session complete gate (after ALL modules pass all gates)
| When | Tool | Why |
|------|------|-----|
| All modules: gates complete | `ghci_quickcheck_export(output_path="test/Spec.hs")` | Generate persistent test file from saved properties and validate it with `cabal_test` by default |
| After export | `cabal_test` | Verify the exported test-suite compiles and executes |
| Coverage verification | `cabal_coverage(min_percent=80)` | Run HPC coverage and parse percentages into structured output |
| After tests | `cabal_build` | Verify full GHC compilation (not just GHCi interpreted) |
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
| Project done | `ghci_quickcheck_export()` | Generate .hs test file from saved properties and validate it with `cabal_test` |
| For CI/CD | `ghci_quickcheck_export(output_path="test/Spec.hs")` | Persistent test suite |
| Want to re-run package tests later | `cabal_test` | Structured wrapper around `cabal test` |

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
| Check if HLS is installed | `ghci_hls(action="available")` | Returns `{ available: bool, version?, source?, binaryPath? }`. If missing from host and bundled, reports unavailable. |
| Get type info at a position | `ghci_hls(action="hover", module_path="src/X.hs", line=5, character=3)` | LSP hover: exact type at cursor. Requires available HLS binary (host or bundled). |

---

## ERROR RESOLUTION (UPDATED)

| Situation | Tool |
|-----------|------|
| Session timeout | Auto-recovery on next call |
| Session corrupted | Session restarts automatically |
| Unused-matches warning | `ghci_fix_warning` with code GHC-40910 |
| Unused-import warning | `ghci_fix_warning` with code GHC-38417 |
| Need semantic comparison | `ghci_equiv(e1, e2)` |
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

## TOOL AVAILABILITY

The MCP does **not** auto-install optional tools.

| Tool | Resolution policy |
|------|-------------------|
| `hlint` | use host PATH first, else bundled binary |
| `fourmolu` / `ormolu` | use host PATH first, else bundled binary |
| `hls` | use host PATH first, else bundled binary |

If no binary is available, the tool returns unavailable with a structured `reason`.
For `ghci_lint` / `ghci_format`, `_guidance` downgrades the step to recommended
when the tool cannot run in the current environment.

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
Properties are validated before execution/persistence; unused lambda binders are rejected to avoid ambiguous exported tests.

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

### `ghci_format` — availability and gate behavior

`ghci_format` requires an available formatter binary (`fourmolu` or `ormolu`)
from host PATH or bundled toolchain. If unavailable, it returns an unavailable
error and does not run style-only fallback formatting.

The `format_tool` field indicates which formatter ran:
- `"fourmolu"` or `"ormolu"` → gate completed
- absent (tool unavailable) → gate NOT completed

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

### `ghci_hls` — availability

`ghci_hls(action="available")` and `ghci_hls(action="hover")` require an HLS
binary in host PATH or bundled toolchain. If neither exists, responses report
unavailable.

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
3. **Blocks invalid persisted properties** — export fails fast when the property store contains unsafe entries (for example, unused binders), with guidance to clean via `ghci_property_lifecycle`.

**Example:** a property like `\n -> eval Map.empty (Lit n) == Right n` will cause
`import qualified Data.Map.Strict as Map` to be included in the output file.

### `ghci_flags` is session-only
Flags set with `ghci_flags(action="set", flags="...")` apply only to the current GHCi session.
To persist an extension, add it to `default-extensions` in the `.cabal` file,
then run `ghci_session(restart)` to pick it up.

---

## Agent gotchas (read these once, save a debug loop)

### `ghci_load` scope semantics
`ghci_load(module_path="src/A.hs")` issues GHCi's `:l` which **drops** previously
loaded modules from scope. If a property in module B references symbols from
module C, pass `mode="additive"` to use `:add` instead:
```
ghci_load(module_path="src/C.hs", mode="additive")
```
For a full project load prefer `load_all=true`.

### GHC2024 defaulting ambiguity
`(`elem` "0123456789")` fails under GHC2024 with an ambiguous `Foldable t`
constraint. Use a monomorphic predicate like `Data.Char.isDigit` instead.

### Partial Prelude functions
`read`, `head`, `tail`, `fromJust`, `(!!)` throw on malformed input. Prefer
`readMaybe`, pattern matching, or `listToMaybe`. The basic-lint fallback
warns on these even when hlint is unavailable.

### Degraded gates never unlock `module-complete`
When `hlint`/`fourmolu`/`ormolu` are unavailable, the fallback responses
carry `gateEligible: false`. The workflow state will NOT mark the format or
lint gate as complete until a real tool runs — do not mistake a `success:true`
fallback for a passed gate.

### `ghci_workflow(action="gate")` single-shot finalizer
When you think a module is done, run `ghci_workflow(action="gate")` — it
orchestrates regression + cabal_test + cabal_build and returns a consolidated
JSON. Saves three round-trips and guarantees you see every step's status.
Use `skip_cabal_test` / `skip_cabal_build` for fast iteration.

### `label` on `ghci_quickcheck`
Pass `label="descriptiveName"` to get meaningful names in the exported
`test/Spec.hs` instead of `property_1..N`. The exporter sanitizes and
deduplicates labels automatically.

### Hot-reload after editing the MCP itself (`mcp_reload_code`)
If you edited a TypeScript file under `mcp-server/src/` and ran
`npm run build`, the running MCP process still holds the OLD bundle in
memory. `mcp_restart` only restarts GHCi — not the Node process. To pick
up your edits WITHOUT exiting Claude Desktop:

```
mcp_reload_code()             # dry-run: check if a restart would help
mcp_reload_code(confirm=true)  # actually reload
```

The tool is staleness-gated (refuses to exit if dist/index.js isn't newer
than the running process) and rate-limited (one per 10s). On success the
client respawns the child automatically. GHCi session and workflow state
are reset by design; property store survives on disk.
