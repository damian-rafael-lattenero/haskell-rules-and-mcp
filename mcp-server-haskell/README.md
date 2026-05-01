# haskell-flows-mcp

**An MCP server for property-first Haskell development.** Compile, suggest QuickCheck laws, run + persist properties, gate before push — all through structured MCP tool calls. One binary, one handshake, everything the agent needs shipped in-band.

---

## Install

```bash
# From source (requires ghcup + cabal)
git clone https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp
cd haskell-rules-and-mcp/mcp-server-haskell
cabal install exe:haskell-flows-mcp \
  --installdir="$HOME/.local/bin" \
  --install-method=copy \
  --overwrite-policy=always
```

The binary lands at `~/.local/bin/haskell-flows-mcp`. No build artifacts
live outside `dist-newstyle/`; no global config files are touched.

## Wire to your MCP client

Point the client at the binary. Shape varies by host:

**Claude Desktop / Claude Code** — add to `~/.claude.json`:

```json
{
  "mcpServers": {
    "haskell-flows": {
      "type": "stdio",
      "command": "/absolute/path/to/.local/bin/haskell-flows-mcp",
      "args": [],
      "env": {
        "HASKELL_PROJECT_DIR": "/absolute/path/to/your/haskell/project",
        "PATH": "/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"
      }
    }
  }
}
```

**Cursor / other MCP hosts** — any stdio-MCP config pointing at the
same binary works. The server reads its project root from
`HASKELL_PROJECT_DIR` (default: current working directory).

## Zero-config guidance (no rules file to clone)

The MCP handshake's `InitializeResult.instructions` now ships the full
situation→tool table + invariants + dogfood-fix flow, all derived
dynamically from the live tool registry. Every successful tool
response carries a `nextStep` field pointing at the most probable
next call, plus an optional multi-step `chain` the agent can batch
via `ghc_batch`.

You do **not** need to clone the repo to get rules. If your host
(Claude Code, Cursor) also wants a project-level rules file, use:

```
ghc_project(action="bootstrap", host="claude-code", write=true)
```

That writes `.claude/rules/haskell-flows-mcp.md` from content baked
into the running binary — always in sync with the tool surface you
actually have.

## Tool surface (35 tools)

Grouped by workflow phase. Every one of these is dispatchable through
`tools/call`; `tools/list` returns the authoritative registry.

### Session + inventory

| Tool                    | Purpose |
|-------------------------|---------|
| `ghc_workflow`         | `status` (inventory + liveness + staleness + phase), `help` (state-aware nudges + phase hint), `next` (single tool) |
| `ghc_toolchain`        | `action=status` (cabal/ghc/hlint/optional binaries), `action=warmup` (probe ahead of time) |
| `ghc_project`          | Project lifecycle: `action=create` scaffolds, `action=switch` repoints, `action=validate` runs `cabal check`, `action=bootstrap` emits host-rules |

### Read / inspect

| Tool             | Purpose |
|------------------|---------|
| `ghc_load`      | `:load` one or more modules; parses errors + warnings + typed holes |
| `ghc_type`      | `:type` a subexpression |
| `ghc_info`      | `:info` a name |
| `ghc_eval`      | Single-line expression eval (cap: 64 KiB) |
| `ghc_hole`      | Every typed hole in a module with expected type + hole fits |
| `ghc_complete`  | `:complete repl "<prefix>"` |
| `ghc_goto`      | Parses "Defined at" marker; returns file + line for a name |
| `ghc_browse`    | `:browse <module>`; list every top-level binding with its type |
| `ghc_doc`       | `:doc <name>`; Haddock text if built with `-haddock` |
| `ghc_imports`   | Current in-scope imports in the GHCi session |
| `hoogle_search`  | Search by name or type signature (needs local `hoogle`) |

### Write / refactor

| Tool                  | Purpose |
|-----------------------|---------|
| `ghc_modules`        | `action=add` registers + scaffolds; `action=remove` de-registers (opt-in `delete_files`). |
| `ghc_deps`           | `list` / `add` / `remove` build-depends, stanza-aware |
| `ghc_add_import`     | Insert a missing `import X` to resolve a scope error |
| `ghc_apply_exports`  | Rewrite a module's export list idempotently |
| `ghc_refactor`       | `rename_local` + `extract_binding`; snapshot-and-compile-verify — rollback on failure |
| `ghc_arbitrary`      | Generate `instance Arbitrary T` template (sized form for recursive types) |
| `ghc_format`         | fourmolu / ormolu; `write=true` to persist |
| `ghc_fix_warning`    | Plan + apply per-warning fixes |

### Quality gates

| Tool                 | Purpose |
|----------------------|---------|
| `ghc_lint`          | HLint; `path=` matches CI, `module_path=` for fast inner loop |
| `ghc_check_module`  | Per-module: compile + warnings + holes + property-store replay |
| `ghc_check_project` | `ghc_check_module` across every exposed + other-module in the .cabal |
| `ghc_coverage`      | `cabal test --enable-coverage` + `hpc report` — 8 metrics |
| `ghc_gate`          | Pre-push finalizer: regression + `cabal test` + `cabal build` in one call |

### Property-first testing

| Tool                       | Purpose |
|----------------------------|---------|
| `ghc_suggest`             | QuickCheck laws a function's signature implies (incl. sibling-aware evaluator preservation + constant-folding soundness) |
| `ghc_quickcheck`          | Run a property; auto-persist on pass |
| `ghc_determinism`         | Re-run N times to catch flakiness before adopting a property |
| `ghc_regression`          | `list` / `run` the persisted property set |
| `ghc_quickcheck_export`   | Materialise `test/Spec.hs` from the regression store |
| `ghc_property_lifecycle`  | Inspect / drop entries in the property store |

### Composition

| Tool         | Purpose |
|--------------|---------|
| `ghc_batch` | N tool invocations in one request (accepts the `chain` field emitted by `nextStep`) |

## Invariants

Every tool call is bounded and sandboxed:

- **Path traversal impossible by construction** — `ModulePath` smart
  constructor rejects `..` + absolute overrides at the boundary.
- **No shell interpolation** — every external subprocess spawned
  argv-form via `System.Process.proc "cmd" [args]`. Agent input never
  reaches a shell.
- **Input sanitisation** — `sanitizeExpression` rejects newline +
  internal-framing sentinel characters + inputs over 64 KiB before any
  `compileExpr` / `exprType` / `getInfo` call reaches the GHC API.
- **DoS cap** — 64 KiB output cap on `ghc_eval`; 30 s inner per-eval
  timeout trips a structured `error_kind=timeout` response and resets
  the HscEnv in place; 10-minute outer per-tool timeout as final
  defence.
- **Session liveness** — in-process GHC API session held behind an
  `MVar` (single-writer to the `HscEnv`); any uncaught exception in a
  tool handler evicts the session and the next call boots a fresh one,
  so no call can poison subsequent ones.
- **Refactor atomicity** — `ghc_refactor` snapshots before every edit
  and compile-verifies after; rollback on any type-check failure.
- **`.cabal` integrity** — `ghc_deps` / `ghc_modules` re-parse after
  every write and refuse to persist a shape that disagrees with the
  verb.
- **Property store durability** — `createDirectoryIfMissing` before
  every write; MVar-lock serialises concurrent saves.

## Build, test, dogfood

```bash
cd mcp-server-haskell
cabal build
cabal test
```

The full CI gate (including hlint + format + coverage) lives at
`scripts/ci-local.sh --fast` in the repo root — it replicates the
GitHub Actions workflow deterministically.

When a tool misbehaves, follow the **dogfood-fix-in-place** flow:

1. Edit `mcp-server-haskell/src/HaskellFlows/...`
2. Add a regression test in `mcp-server-haskell/test/Spec.hs`
3. Run `scripts/ci-local.sh --fast` — green is required.
4. `git commit + push`. Keep dogfooding with the stale running binary;
   CI validates; the fix lands on next natural `cabal install`.

## Layout (library)

| Module                                 | Responsibility |
|----------------------------------------|----------------|
| `HaskellFlows.Types`                   | Traversal-safe `ProjectDir` / `ModulePath` smart constructors |
| `HaskellFlows.Mcp.Protocol`            | JSON-RPC 2.0 + MCP envelope types |
| `HaskellFlows.Mcp.Transport`           | Stdin / stdout loop |
| `HaskellFlows.Mcp.Server`              | Tool dispatch + shared state |
| `HaskellFlows.Mcp.Guidance`            | Canonical `initialize.instructions` + resource markdown (dynamic from registry) |
| `HaskellFlows.Mcp.NextStep`            | `nextStep` decision table + multi-step chain support |
| `HaskellFlows.Mcp.WorkflowState`       | Session counters + history-pattern nudges + phase classifier |
| `HaskellFlows.Mcp.Resources`           | MCP `resources/list` advertising |
| `HaskellFlows.Mcp.Staleness`           | Binary-vs-boot mtime diff surfaced on `ghc_workflow(status)` |
| `HaskellFlows.Ghc.ApiSession`          | In-process GHC API session (HscEnv lifecycle, MVar single-writer, diagnostic capture via log hook) |
| `HaskellFlows.Ghc.CabalBootstrap`      | Per-target stanza-flag capture via a `cabal v2-repl --with-compiler` shim |
| `HaskellFlows.Ghc.Sanitize`            | Pure boundary sanitisation (newline / sentinel / size caps) |
| `HaskellFlows.Parser.{Error,Hole,Type,TypeSignature,QuickCheck,Coverage}` | Structured parsing of GHC diagnostics + subprocess tool output |
| `HaskellFlows.Refactor.{Rename,Extract}` | Snapshot-and-compile-verify primitives |
| `HaskellFlows.Suggest.Rules`           | 11+ law engines (sibling-aware) |
| `HaskellFlows.Data.PropertyStore`      | JSON-backed regression store (MVar-serialised) |
| `HaskellFlows.Tool.*`                  | One module per registered tool (38 tools) |

## License

BSD-3-Clause.
