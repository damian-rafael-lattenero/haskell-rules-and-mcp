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
via `ghci_batch`.

You do **not** need to clone the repo to get rules. If your host
(Claude Code, Cursor) also wants a project-level rules file, use:

```
ghci_bootstrap(host="claude-code", write=true)
```

That writes `.claude/rules/haskell-flows-mcp.md` from content baked
into the running binary — always in sync with the tool surface you
actually have.

## Tool surface (38 tools)

Grouped by workflow phase. Every one of these is dispatchable through
`tools/call`; `tools/list` returns the authoritative registry.

### Session + inventory

| Tool                    | Purpose |
|-------------------------|---------|
| `ghci_workflow`         | `status` (inventory + liveness + staleness + phase), `help` (state-aware nudges + phase hint), `next` (single tool) |
| `ghci_toolchain_status` | Availability + version of cabal / ghc / hlint / optional binaries |
| `ghci_toolchain_warmup` | Probe every optional binary up-front so later calls don't pay the lookup |
| `ghci_validate_cabal`   | `cabal check` + duplicate-dep / missing-field heuristics |
| `ghci_bootstrap`        | Emit host-specific rules file from the binary (no repo clone needed) |

### Read / inspect

| Tool             | Purpose |
|------------------|---------|
| `ghci_load`      | `:load` one or more modules; parses errors + warnings + typed holes |
| `ghci_type`      | `:type` a subexpression |
| `ghci_info`      | `:info` a name |
| `ghci_eval`      | Single-line expression eval (cap: 64 KiB) |
| `ghci_hole`      | Every typed hole in a module with expected type + hole fits |
| `ghci_complete`  | `:complete repl "<prefix>"` |
| `ghci_goto`      | Parses "Defined at" marker; returns file + line for a name |
| `ghci_browse`    | `:browse <module>`; list every top-level binding with its type |
| `ghci_doc`       | `:doc <name>`; Haddock text if built with `-haddock` |
| `ghci_imports`   | Current in-scope imports in the GHCi session |
| `hoogle_search`  | Search by name or type signature (needs local `hoogle`) |

### Write / refactor

| Tool                  | Purpose |
|-----------------------|---------|
| `ghci_create_project` | Scaffold `<name>.cabal`, `cabal.project`, `src/<Module>.hs`, `test/Spec.hs` |
| `ghci_add_modules`    | Register new modules in `.cabal` exposed-modules + scaffold empty stubs |
| `ghci_remove_modules` | Symmetric de-registration; opt-in `delete_files` |
| `ghci_deps`           | `list` / `add` / `remove` build-depends, stanza-aware |
| `ghci_add_import`     | Insert a missing `import X` to resolve a scope error |
| `ghci_apply_exports`  | Rewrite a module's export list idempotently |
| `ghci_refactor`       | `rename_local` + `extract_binding`; snapshot-and-compile-verify — rollback on failure |
| `ghci_arbitrary`      | Generate `instance Arbitrary T` template (sized form for recursive types) |
| `ghci_format`         | fourmolu / ormolu; `write=true` to persist |
| `ghci_fix_warning`    | Plan + apply per-warning fixes |

### Quality gates

| Tool                 | Purpose |
|----------------------|---------|
| `ghci_lint`          | HLint; `path=` matches CI, `module_path=` for fast inner loop |
| `ghci_check_module`  | Per-module: compile + warnings + holes + property-store replay |
| `ghci_check_project` | `ghci_check_module` across every exposed + other-module in the .cabal |
| `ghci_coverage`      | `cabal test --enable-coverage` + `hpc report` — 8 metrics |
| `ghci_gate`          | Pre-push finalizer: regression + `cabal test` + `cabal build` in one call |

### Property-first testing

| Tool                       | Purpose |
|----------------------------|---------|
| `ghci_suggest`             | QuickCheck laws a function's signature implies (incl. sibling-aware evaluator preservation + constant-folding soundness) |
| `ghci_quickcheck`          | Run a property; auto-persist on pass |
| `ghci_determinism`         | Re-run N times to catch flakiness before adopting a property |
| `ghci_regression`          | `list` / `run` the persisted property set |
| `ghci_quickcheck_export`   | Materialise `test/Spec.hs` from the regression store |
| `ghci_property_lifecycle`  | Inspect / drop entries in the property store |

### Composition

| Tool         | Purpose |
|--------------|---------|
| `ghci_batch` | N tool invocations in one request (accepts the `chain` field emitted by `nextStep`) |

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
- **DoS cap** — 64 KiB output cap on `ghci_eval`; 30 s inner per-eval
  timeout trips a structured `error_kind=timeout` response and resets
  the HscEnv in place; 10-minute outer per-tool timeout as final
  defence.
- **Session liveness** — in-process GHC API session held behind an
  `MVar` (single-writer to the `HscEnv`); any uncaught exception in a
  tool handler evicts the session and the next call boots a fresh one,
  so no call can poison subsequent ones.
- **Refactor atomicity** — `ghci_refactor` snapshots before every edit
  and compile-verifies after; rollback on any type-check failure.
- **`.cabal` integrity** — `ghci_deps` / `ghci_add_modules` /
  `ghci_remove_modules` re-parse after every write and refuse to
  persist a shape that disagrees with the verb.
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
| `HaskellFlows.Mcp.Staleness`           | Binary-vs-boot mtime diff surfaced on `ghci_workflow(status)` |
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
