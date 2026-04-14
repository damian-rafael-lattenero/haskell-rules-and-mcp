# Use the haskell-flows MCP for ALL Haskell development

This project has a Haskell MCP server (`haskell-flows`) that provides structured,
compiler-driven development tools. You MUST use it for all Haskell work.

**Before writing any Haskell code**, call `ghci_session(status)` to verify the MCP is alive.

**Follow the server's instructions** — they contain the complete workflow with tool tiers
and development flows. The MCP server injects these via the `instructions` field automatically.

**Never use Bash** for `cabal`, `ghc`, `ghci`, `stack`, or any Haskell toolchain command.
Use the MCP tools instead: `ghci_load`, `ghci_type`, `ghci_eval`, `hoogle_search`, etc.

---

## Mandatory tool usage at key points

| When | Tool | Why |
|------|------|-----|
| New data types in stub phase | `ghci_arbitrary(type_name="...")` | Generate Arbitrary instances — don't write by hand |
| Before implementing functions | `ghci_suggest(module_path="...")` | See hole fits for ALL undefined functions at once |
| After each algebraic function | `ghci_quickcheck(property="suggest", function_name="...")` | Discover testable laws incrementally |
| When a law is testable | `ghci_quickcheck(property, incremental=true, module_path="src/X.hs")` | Test laws immediately, don't defer to module-end |
| Logic error (types OK, wrong result) | `ghci_trace(expression, trace_points=[...])` | Debug intermediate values |
| Lost track of progress | `ghci_workflow(action="next")` | See what step comes next |
| Confused / need guidance | `ghci_workflow(action="help")` | Returns `suggested_tools`, `reasoning`, and `steps` |

---

## New tools — use these, don't work around them

### Dependency management (NEVER edit .cabal manually for deps)

```
ghci_deps(action="add", package="containers")           # add dep
ghci_deps(action="add", package="text", version=">= 2.0")  # add with version
ghci_deps(action="remove", package="old-pkg")           # remove dep
ghci_deps(action="list")                                # inspect current deps
ghci_deps(action="graph")                               # visualize import graph, detect cycles
```

After add/remove: always `ghci_session(restart)` to reload.

### Typed hole exploration

```
ghci_hole(module_path="src/MyModule.hs")               # all holes: type + fits + bindings
ghci_hole(module_path="src/MyModule.hs", hole_name="_x")  # filter to one hole
```

Use this BEFORE implementing a function stub — it shows exactly what fits the hole.

### Refactoring

```
ghci_refactor(action="rename_local", module_path="src/X.hs", old_name="foo", new_name="bar")
ghci_refactor(action="extract_binding", module_path="src/X.hs", new_name="helper", lines=[5,8])
```

Always `ghci_load(diagnostics=true)` after to verify compilation.
Never use `sed`, manual find/replace, or other text tools for Haskell refactoring.

### Format fallback

`ghci_format(module_path="...", write=true)` now works even without `fourmolu`/`ormolu`.
It fixes trailing whitespace, converts tabs to spaces, and adds missing final newlines.
Always run it as part of the module-complete gate.

### Session flags

```
ghci_flags(action="set", flags="-XOverloadedStrings")  # enable for session
ghci_flags(action="unset", flags="-XSomething")         # disable
ghci_flags(action="list")                               # see active settings
```

To persist: add `default-extensions: SomeExtension` in `.cabal`, then `ghci_session(restart)`.

### Performance hints

```
ghci_profile(action="suggest", module_path="src/X.hs")  # static analysis — no GHC needed
ghci_profile(action="time", executable="my-app")        # GHC time profiling
```

### HLS integration

```
ghci_hls(action="available")                            # check installation — never crashes
ghci_hls(action="hover", module_path="...", line=5, character=3)  # type at position
```

### Stack projects

`ghci_init(name="...", modules=[...], build_tool="stack")` generates both `.cabal`
and `stack.yaml`. For cabal projects (default), omit `build_tool`.

### `module_path` in quickcheck

```
ghci_quickcheck(property="...", module_path="src/X.hs")   ✅ preferred spelling
ghci_quickcheck(property="...", module="src/X.hs")        ✅ also accepted
```

Both work identically. Use `module_path` for consistency with all other tools.

---

## Decision tree for common situations

```
Need to add a library?
  → ghci_deps(action="add") → ghci_session(restart)

See a typed hole warning after ghci_load?
  → ghci_hole(module_path="...") to explore fits

Need to rename a function across a module?
  → ghci_refactor(action="rename_local") → ghci_load to verify

Module complete but not sure what next?
  → ghci_workflow(action="help") for contextual steps

Code works but feels slow?
  → ghci_profile(action="suggest") for quick static hints
```
