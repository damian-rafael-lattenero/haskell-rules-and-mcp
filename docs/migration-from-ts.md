# Migration from the TypeScript MCP

If you've been using the original `mcp-server/` (TypeScript) and are
switching to `mcp-server-haskell/`, this doc maps every tool name
1:1 and flags behaviour changes so nothing surprises you.

**TL;DR:**

- Every tool you used has a Haskell equivalent.
- Names are identical — no agent re-training needed.
- 3 bugs from the retrospective are fixed by design in the Haskell
  port (see [Behaviour changes](#behaviour-changes)).
- 2 new tools with no TS counterpart (`ghci_batch`, `ghci_check_project`).

---

## Install the Haskell MCP

Follow [`docs/install.md`](install.md). The shortest path:

```bash
cabal install haskell-flows-mcp
```

Then update `.mcp.json` to point at the new binary:

```diff
 {
   "mcpServers": {
     "haskell-flows": {
-      "command": "node",
-      "args": ["./mcp-server/dist/index.js"],
+      "command": "haskell-flows-mcp",
       "env": {
         "HASKELL_PROJECT_DIR": "./"
       }
     }
   }
 }
```

Restart your MCP client. That's it.

---

## Tool name map

All 26 tools exposed by the Haskell port:

### Core GHCi loop

| TS name               | Haskell name          | Notes                           |
|-----------------------|-----------------------|---------------------------------|
| `ghci_load`           | `ghci_load`           | Dual-pass diagnostics supported |
| `ghci_type`           | `ghci_type`           | — |
| `ghci_info`           | `ghci_info`           | — |
| `ghci_eval`           | `ghci_eval`           | **Innovation:** output capped at 64 KiB |
| `ghci_complete`       | `ghci_complete`       | — |
| `ghci_doc`            | `ghci_doc`            | — |
| `ghci_goto`           | `ghci_goto`           | — |
| `ghci_session`        | `ghci_session`        | — |

### Property-first workflow

| TS name                  | Haskell name             | Notes                             |
|--------------------------|--------------------------|-----------------------------------|
| `ghci_hole`              | `ghci_hole`              | Reports expected type + bindings + valid fits |
| `ghci_arbitrary`         | `ghci_arbitrary`         | Emits pasteable Arbitrary template |
| `ghci_quickcheck`        | `ghci_quickcheck`        | Auto-persists on pass |
| `ghci_regression`        | `ghci_regression`        | Replays the persisted property store |
| `ghci_check_module`      | `ghci_check_module`      | — |
| `ghci_coverage`          | `ghci_coverage`          | Parses HPC report into structured metrics |

### External tool wrappers

| TS name                | Haskell name           | Notes                             |
|------------------------|------------------------|-----------------------------------|
| `hoogle_search`        | `hoogle_search`        | — |
| `ghci_format`          | `ghci_format`          | fourmolu → ormolu fallback |
| `ghci_lint`            | `ghci_lint`            | **Bug fix:** recursive on directory by default |

### Project management

| TS name                 | Haskell name             | Notes                             |
|-------------------------|--------------------------|-----------------------------------|
| `ghci_create_project`   | `ghci_create_project`    | — |
| `ghci_deps`             | `ghci_deps`              | **Bug fix:** no longer destructive |
| `ghci_validate_cabal`   | `ghci_validate_cabal`    | Structured severity output |
| `ghci_toolchain_status` | `ghci_toolchain_status`  | Single call for every binary's availability + version |

### Code editing

| TS name                | Haskell name           | Notes                             |
|------------------------|------------------------|-----------------------------------|
| `ghci_refactor`        | `ghci_refactor`        | Snapshot-and-compile: rewrite reverts on compile failure |

### Meta / workflow

| TS name                | Haskell name           | Notes                             |
|------------------------|------------------------|-----------------------------------|
| `ghci_workflow`        | `ghci_workflow`        | — |

### New in Haskell port (no TS counterpart)

| Haskell name             | What it does                                                  |
|--------------------------|---------------------------------------------------------------|
| `ghci_batch`             | Multi-tool pipelining in one request. `fail_fast: true` by default. |
| `ghci_check_project`     | Enumerates modules from `.cabal`, runs `check_module` on each. |

---

## Behaviour changes

Three bugs from [`docs/ts-mcp-retrospective.md`](ts-mcp-retrospective.md)
are fixed by design in the Haskell port. Check the retrospective for
the full incident history; here's the user-visible delta:

### 1. `ghci_deps(action="add")` no longer deletes the existing deps

The TS tool rewrote the entire `build-depends:` list, deleting every
dep it didn't discover at serialize time. The Haskell port does a
comma-prefixed insert that preserves the rest of the list
byte-for-byte. See [issue #6](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/6).

**Impact for users:** no change to how you call the tool; just safer.

### 2. `ghci_lint` accepts a directory by default

The TS tool only accepted a single `module_path`. CI runs
`hlint mcp-server-haskell/` recursively, which lets style drift into
`test/` without being caught locally. The Haskell port takes
`path="dir/"` as first-class and matches CI exactly. See [issue #8](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/8).

**Impact for users:** new default. Old invocations with `module_path`
still work.

### 3. Session no longer silently re-routes between projects

The TS MCP occasionally re-scanned for `.cabal` files and picked a
different one than the active project. The Haskell port only changes
the active project when you explicitly call `ghci_switch_project`.
See [issue #7](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/7).

**Impact for users:** no change to the happy path; the silent
re-routing bug is gone.

---

## Differences in tool output shape

JSON shapes are deliberately kept compatible with the TS versions so
prompts / tool-use code don't need changes. Where the Haskell port
adds fields, they're always additive — existing fields keep the same
names and types.

Two specific additions worth knowing:

### `ghci_hole`

The Haskell port's `ghci_hole` adds a `validFits` array to every
hole, parsed from GHC's `Valid hole fits include:` section. Each entry
is `{name, type, source}`. If your prompt didn't use this field, no
change. If it does, now you can rely on it.

### `ghci_quickcheck`

On `QcPassed`, the Haskell port auto-persists the property to
`.haskell-flows/properties.json`. The response adds no new fields —
the persistence is side-effect-only. Use `ghci_regression(action="list")`
to see the store's contents.

---

## Gotchas

### Config env vars

The Haskell port reads `HASKELL_PROJECT_DIR`, same as TS. No
additional env var is required.

If `.mcp.json` doesn't set `HASKELL_PROJECT_DIR`, the server falls
back to the process's CWD. In Claude Code that's typically the repo
root — usually what you want.

### Property store location

Both ports write to `.haskell-flows/properties.json` under the
project directory. **Do not share this file across projects** —
the properties are scoped to the module paths of the owning package.
Adding it to `.gitignore` is the safe default.

### Bundled hlint / fourmolu

The TS port auto-downloaded `hlint` / `fourmolu` to
`mcp-server/vendor-tools/`. The Haskell port does **not**
auto-download: it resolves via `findExecutable` on `PATH` only. Set
those tools up via GHCup or `cabal install hlint fourmolu` before
you depend on them.

Tracking issue: [#16](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/16).

---

## Rolling back

If anything breaks:

```bash
rm $(which haskell-flows-mcp)   # remove the Haskell binary
```

…then restore your previous `.mcp.json` pointing at the TS MCP. The
two ports can't both run simultaneously under the same name without
config changes.

But: if you hit a bug, please
[file an issue](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/new/choose)
first. Every surprise encountered during migration helps harden the
Haskell port for everyone else.
