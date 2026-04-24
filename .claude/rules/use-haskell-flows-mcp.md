# Use the haskell-flows MCP for ALL Haskell development

This project ships an MCP server (`haskell-flows`) that owns the Haskell
dev loop end-to-end: a persistent GHCi session, compiler-driven refactors,
property-first testing, cabal-safe edits, and HPC coverage. The MCP is the
authoritative Haskell toolchain wrapper for this repo.

**You MUST use it for all Haskell work.** Do not shell out to `cabal`,
`ghc`, `ghci`, or `hlint` via Bash — the MCP already does it with structured
output, proper sandboxing, and invariant checks the ad-hoc commands miss.

**Exceptions** (allowed via Bash because they replicate CI gates or the MCP
itself cannot run them):

- `scripts/ci-local.sh --fast` or `scripts/ci-local.sh` before pushing.
- `cabal install exe:haskell-flows-mcp ...` to rebuild the MCP binary.

---

## Start-of-session handshake

Before writing any Haskell code, always:

1. `ghc_workflow(action="status")` — verify MCP is alive, confirm
   `projectDir`, confirm `toolsActive` lists all 25 tools. If it lists
   fewer, the binary is stale and the next reinstall will pick up new
   tools.
2. `ghc_toolchain_status()` — confirm `cabal`, `ghc`, `hlint` are on
   PATH (blocking gates). `fourmolu`/`ormolu`/`hoogle`/`hls` are
   optional — tools that depend on them degrade gracefully.

If either fails, stop and report — do not attempt to work around a dead
MCP by writing code directly.

---

## The 25 tools (canonical inventory)

### Session + inventory

| Tool                    | What it does                                                                         |
|-------------------------|--------------------------------------------------------------------------------------|
| `ghc_workflow`         | `status` (server inventory + GHCi liveness), `help` (next-step advice), `next`.      |
| `ghc_toolchain_status` | Availability + version of every external binary the MCP delegates to.                |
| `ghc_validate_cabal`   | `cabal check` + duplicate-dep / missing-field heuristics.                            |

### Read / inspect

| Tool                | What it does                                                               |
|---------------------|----------------------------------------------------------------------------|
| `ghc_load`         | `:l <module>`; `diagnostics=true` enables a deferred-pass for typed holes. |
| `ghc_type`         | `:t <expr>`.                                                               |
| `ghc_info`         | `:i <name>`.                                                               |
| `ghc_eval`         | Single-line expression eval (cap: 64 KiB).                                 |
| `ghc_hole`         | Every typed hole in a module, with type + hole fits + in-scope bindings.   |
| `ghc_complete`     | `:complete repl "<prefix>"`.                                               |
| `ghc_goto`         | Parses "Defined at" marker; returns file + line for a name.                |
| `ghc_doc`          | `:doc <name>`; Haddock text if `-haddock` built.                           |
| `hoogle_search`     | Search by name or type signature (needs local `hoogle`).                   |

### Write / refactor

| Tool                  | What it does                                                                    |
|-----------------------|---------------------------------------------------------------------------------|
| `ghc_create_project` | Scaffold `<name>.cabal` + `cabal.project` + `src/<Module>.hs` + `test/Spec.hs`. |
| `ghc_deps`           | `list` / `add` / `remove` build-depends. Honours `stanza="library"` /
                          `"test-suite"` / `"test-suite:NAME"` / `"executable[:NAME]"` /
                          `"benchmark[:NAME]"` / `"foreign-library[:NAME]"`.                         |
| `ghc_refactor`       | `rename_local` + `extract_binding`. Snapshot-and-compile-verify: if the rewrite
                          fails to type-check, the file is restored from snapshot.                   |
| `ghc_arbitrary`      | Generate `instance Arbitrary T` template from `:i` output. Polymorphic types
                          get the correct `(Arbitrary a, Arbitrary b) =>` context.                   |
| `ghc_format`         | fourmolu (preferred) or ormolu; check-only by default, `write=true` rewrites.
                          Reports `unavailable` cleanly if neither formatter is on PATH.             |

### Quality gates

| Tool                 | What it does                                                                 |
|----------------------|------------------------------------------------------------------------------|
| `ghc_lint`          | HLint. `path` (recursive, matches CI) or `module_path` (fast inner loop).    |
| `ghc_check_module`  | Compile + warnings + holes + property-store replay, aggregated per module.   |
| `ghc_check_project` | `ghc_check_module` over every exposed-module + other-module in `.cabal`.    |
| `ghc_coverage`      | `cabal test --enable-coverage` + `hpc report` — 8 metrics (expressions,
                         boolean, alternatives, …) parsed from the HPC text summary.                   |

### Property-first testing

| Tool              | What it does                                                                      |
|-------------------|-----------------------------------------------------------------------------------|
| `ghc_suggest`    | Given a function name, propose QuickCheck properties its signature implies. Each
                      suggestion has a confidence score; `[a] -> [a]` is dampened to `Low` unless the
                      name hints at canonicalisation.                                                   |
| `ghc_quickcheck` | Run a property; on pass, auto-persist it to `.haskell-flows/properties.json`.
                      Accepts `module=` so the regression suite knows which file to reload.             |
| `ghc_regression` | `list` or `run` the auto-persisted property set.                                  |

### Composition

| Tool          | What it does                                                                     |
|---------------|----------------------------------------------------------------------------------|
| `ghc_batch`  | N tool invocations sequentially in one request. Accepts both `{tool, args}` and
                  `{name, arguments}` shapes. `fail_fast=true` (default) stops on first error;
                  `fail_fast=false` runs every action.                                                |

---

## Mandatory tool choices by situation

| Situation                                     | Use this                                                   |
|-----------------------------------------------|------------------------------------------------------------|
| New `data T = ...` declared                   | `ghc_arbitrary(type_name="T")`                            |
| Function has a `_` hole or an empty stub      | `ghc_hole(module_path="src/X.hs")`                        |
| Want properties from a function's signature   | `ghc_suggest(function_name="f")`                          |
| Checking a law holds                          | `ghc_quickcheck(property="…", module_path="src/X.hs")`    |
| Renaming a local identifier                   | `ghc_refactor(action="rename_local", scope_line_start=…)` |
| Adding a dep (to the right stanza!)           | `ghc_deps(action="add", package="X", stanza="…")`         |
| Not sure what to do next                      | `ghc_workflow(action="help")`                             |
| Pushing soon                                  | `ghc_check_project()` + `scripts/ci-local.sh --fast`      |

**Never** `sed`/`awk`/`find-and-replace` across Haskell files. Use
`ghc_refactor`. The snapshot-and-compile-verify invariant rolls the file
back atomically on any failure.

**Never** edit `.cabal` by hand for deps. Use `ghc_deps`. The post-edit
invariant check refuses to persist a write whose re-parsed dep list
disagrees with the verb (added/removed).

---

## The dogfood-fix-in-place flow

When an MCP tool returns a wrong result, a hang, an unexpected error, or a
clear bug:

1. **Log the finding** inline (F-##).
2. **Fix the MCP code** at `mcp-server-haskell/src/HaskellFlows/` via
   `Edit`/`Write`.
3. **Add a regression test** at `mcp-server-haskell/test/Spec.hs`.
4. **`scripts/ci-local.sh --fast`** must be green — this replicates CI
   exactly (build + all tests + recursive hlint).
5. **Commit + push** directly to master with a descriptive message + a
   `Co-Authored-By: Claude …` trailer.
6. **Keep working** with the stale binary. No `cabal install`, no Claude
   Desktop relaunch. CI + tests are sufficient validation; the fix lands
   in-vivo on the next natural reinstall.

This is the established workflow — see
`~/.claude/projects/.../memory/feedback_dogfood_fix_flow.md`.

---

## What CANNOT hang (post-F-12)

After Phase 11c, the session layer is provably liveness-safe:

- `SessionStatus = Alive | Overflowed | Dead`. When GHCi dies,
  `drainHandle` flips to `Dead`; every in-flight `executeNoLock` wakes via
  STM and throws `SessionExhausted`.
- `executeNoLock` honours its `timeoutMicros` via `registerDelay`.
- `Server.runTool` wraps every handler in a 10-minute
  `System.Timeout.timeout` as defence-in-depth.

If a tool call hangs for more than ~10 minutes, that's a real regression —
report it. It is no longer the expected degenerate mode.

---

## Every tool response carries `nextStep`

Every successful tool call returns a `nextStep` object inside its
payload:

```json
{
  "success": true,
  "files_written": ["…"],
  "nextStep": {
    "tool":  "ghc_deps",
    "why":   "Your scaffold has only `base`. Add deps before wiring up modules.",
    "example": { "action": "add", "package": "QuickCheck", "stanza": "test-suite" }
  }
}
```

`nextStep.tool` is the MCP's push about what's most likely useful
next, `nextStep.why` explains the rationale, `nextStep.example` —
when present — is a canonical arguments object you can use
verbatim.

The hint is informational. Follow it when it fits; ignore it and
pick your own path when it doesn't. It replaces the need to call
`ghc_workflow(action="next")` after every successful tool.

Errors suppress the hint — when `success: false`, read the error
and decide, don't look for a nextStep that is not there.

---

## Decision tree for common situations

```
Need to add a library?
  → ghc_deps(action="add", package="X", stanza="library"|"test-suite"|...)

See a typed hole warning after ghc_load?
  → ghc_hole(module_path="src/X.hs")

Need to rename a local binding?
  → ghc_refactor(action="rename_local",
                  scope_line_start=.., scope_line_end=..)
  → If compile-verify fails, the file is restored automatically.

Want properties for a new function?
  → ghc_suggest(function_name="f")    # get candidate laws
  → ghc_quickcheck(property="…", module_path="src/X.hs")
  → ghc_regression(action="run")      # replay the saved set later

Module complete?
  → ghc_check_module(module_path="src/X.hs")

Whole project ready?
  → ghc_check_project() + scripts/ci-local.sh --fast
  → ghc_coverage()  # 8 HPC metrics

MCP tool misbehaves?
  → Find the bug → Edit mcp-server-haskell/ → add regression test →
    ci-local.sh → commit+push → KEEP GOING (no reinstall mid-flow).
```
