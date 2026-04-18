# haskell-flows MCP Server

A Model Context Protocol (MCP) server that gives AI coding agents (Claude Code, Cursor, any MCP-capable client) a persistent, strict, test-first workflow for Haskell development: GHCi session, QuickCheck with property persistence, arbitrary generator synthesis, typed-hole suggestions, cabal orchestration, and a "dead-simple" project scaffolding flow.

The server is designed to be **consumed by agents, not humans directly**. Every design decision optimizes for agent reliability: strict Zod schemas reject typoed parameters instead of silently ignoring them, suggestions that would emit tautologies are removed rather than downgraded to low-confidence, and optional toolchain gates degrade explicitly rather than block.

---

## Tool surface

### Scaffolding (dead-simple, strict, single-call)
| Tool | What it does |
|---|---|
| `ghci_create_project` | Create a brand-new project in a fresh directory: writes `<name>.cabal` + `cabal.project` + `src/<Module>.hs` per module + minimal `test/Spec.hs`, then activates the GHCi session on it. Fails cleanly if a `.cabal` already exists at the target — no `force` flag, no prompts. |
| `ghci_add_modules` | Extend the **active** project with new modules: appends to `exposed-modules` (preserving indentation) and scaffolds stubs, optionally with typed `= undefined` signatures for `ghci_suggest` hole-fit mode. Fails if no active `.cabal`. |
| `ghci_switch_project` | List projects or switch to an existing one. Unknown parameter names are rejected at the SDK layer (`unrecognized_keys` error). |

### Editing / inspection
| Tool | What it does |
|---|---|
| `ghci_load` | Load or reload a module with structured diagnostics: errors, categorized warnings with suggested actions, typed holes. |
| `ghci_type` / `ghci_info` / `ghci_kind` | Type, info, kind of an expression (`:t`, `:i`, `:k`). |
| `ghci_eval` | Evaluate a Haskell expression; supports multi-line `:{ :}` blocks via `statements`. |
| `ghci_imports` / `ghci_add_import` | Inspect and add imports persistently across reloads. |
| `ghci_hole` / `ghci_hole_fits` | Typed-hole analysis with fits and relevant bindings. |
| `ghci_goto` / `ghci_references` / `ghci_rename` | Navigation and safe rename. |
| `ghci_refactor` | Small refactorings (extract, rename-local). |
| `ghci_batch` | Run multiple GHCi commands atomically. |
| `ghci_check_module` | `:browse` with header-aware suggestion of an export list — **only when one does not already exist** — and `kind`-aware discrimination between `Name(..)` for data/newtype and bare `Name` for type synonyms. |

### Testing / QuickCheck
| Tool | What it does |
|---|---|
| `ghci_arbitrary` | Synthesize an `Arbitrary` instance for a type, with `sized` generators and per-constructor `resize` for recursive shapes. |
| `ghci_suggest` | `mode="suggest"`: find `= undefined` stubs and show typed-hole fits. `mode="analyze"`: suggest QuickCheck properties based on type shape (endomorphism, binary op, list endo, roundtrip). **Never emits tautologies** (`f x == f x` was removed in Fase 1). |
| `ghci_quickcheck` / `ghci_quickcheck_batch` | Run a property (or many), with automatic scope-error recovery (`_autoResolved: true`). Passing properties are persisted to `.haskell-flows/properties.json` with a `passCount`. |
| `ghci_quickcheck_export` | Materialize all saved passing properties into a runnable `test/Spec.hs` and auto-run `cabal test`. |
| `ghci_regression` | Re-run all saved properties — cheap guard at session start or before a commit. |
| `ghci_property_lifecycle` | Deprecate / replace / audit stored properties. |

### Quality gates
| Tool | What it does |
|---|---|
| `ghci_lint` | Run hlint. When hlint is unavailable (missing + auto-download failed), **automatically falls back** to `basic-lint-rules` heuristics with `degraded: true`, `gateEligible: false`, and a `_primary_failure` block pointing at the root cause. The degraded response surfaces issues but does NOT unlock the module-complete lint gate. |
| `ghci_format` | Run fourmolu / ormolu. Returns `unavailable: true` with an actionable hint when neither is present. No false "formatted" signal. |
| `ghci_fix_warning` | Auto-fix common GHC warnings (`-Wunused-imports`, `-Wmissing-signatures`, etc.) with preview or apply. |

### Build / test / coverage
| Tool | What it does |
|---|---|
| `cabal_build` / `cabal_test` | Thin JSON-structured wrappers. |
| `cabal_coverage` | `cabal test --enable-coverage`, with an `hpc report` fallback against the latest `.tix` AND an HTML-report parser for `hpc_index.html` as a third fallback. Reports `reportSource: "cabal-test" \| "hpc-report" \| "hpc-html"`. When no source works, returns an actionable hint suggesting `-fhpc` in the test-suite's ghc-options. |

### Toolchain / workflow
| Tool | What it does |
|---|---|
| `ghci_toolchain_status` | Runtime availability + cross-platform release matrix. Propagates availability to workflow state so `_guidance` reflects reality. |
| `ghci_workflow` | Flow state (`status` / `help` / `checklist` / `next` / `progress`) — tells the agent what to do next given the actual state of loaded modules, warnings, tested properties, and gate completion. |
| `ghci_hls` | HLS integration: `available`, `hover`, `diagnostics`. |
| `hoogle_search` | Search Hoogle by name or type. |
| `ghci_doc` | Haddock docs for a name. |
| `ghci_session` / `mcp_restart` | Session lifecycle (restart GHCi — `mcp_restart` does NOT restart the Node process; TS code changes require a new Claude Code session). |
| `ghci_setup` | Install development rules into `.claude/rules/`. |
| `ghci_validate_cabal` / `ghci_deps` | Cabal file validation and dependency management. |
| `ghci_complete` | Completion candidates for a prefix. |

---

## Typical pipeline (new project, agent-driven)

```
ghci_create_project({name: "expr-eval", modules: ["Expr.Syntax", "Expr.Eval"]})
ghci_load({module_path: "src/Expr/Syntax.hs"})
ghci_arbitrary({type_name: "Expr"})                                        # paste into source
ghci_load({module_path: "src/Expr/Syntax.hs"})                             # reload
ghci_load({module_path: "src/Expr/Eval.hs"})                               # after implementing
ghci_quickcheck({property: "\\e -> eval empty (simplify e) == eval empty e", module_path: "src/Expr/Eval.hs"})
# ... more properties
ghci_check_module({module_path: "src/Expr/Eval.hs"})                       # export audit
ghci_lint({module_path: "src/Expr/Eval.hs"})                               # auto-degrades if hlint absent
ghci_quickcheck_export()                                                    # writes test/Spec.hs + runs cabal test
cabal_test()
```

---

## Quick start

### Prerequisites
- GHC 9.12+ and Cabal 3.12+ (via [GHCup](https://www.haskell.org/ghcup/))
- Node.js 22+

### Setup

```bash
git clone <repo-url>
cd haskell-rules-and-mcp
cp .mcp.example.json .mcp.json      # local, git-ignored — tweak if you need custom paths
cd mcp-server
npm install
npm run build                        # produces dist/index.js (gitignored)
```

After rebuilding the MCP (any `.ts` change under `mcp-server/src/`), restart the
server from your MCP client so it picks up the new binary. In Claude Code:
`/mcp` → restart `haskell-flows`.

### Configure Claude Code (`.mcp.json`)

The repo ships a **portable template** at [`.mcp.example.json`](./.mcp.example.json):

```json
{
  "mcpServers": {
    "haskell-flows": {
      "command": "node",
      "args": ["./mcp-server/dist/index.js"],
      "env": {
        "HASKELL_PROJECT_DIR": "./playground/hindley-milner"
      }
    }
  }
}
```

Why relative paths:
- Claude Code resolves relative `args` against the `.mcp.json` directory, so
  **each worktree runs its own `dist/index.js`** (no cross-contamination).
- `HASKELL_PROJECT_DIR` is resolved against the launching `cwd` (the config's
  directory), so `"./playground/foo"` points at this worktree's playground.
- No `PATH` baked in → inherits from your shell. If `ghc`/`cabal` are not on
  PATH after login, add a minimal `PATH` override in your **local** `.mcp.json`
  (not committed) rather than the example.

**Why `.mcp.json` is gitignored:** it may drift with personal paths, env
overrides, or (never) secrets. Keep the shared template in
`.mcp.example.json`; put anything machine-specific in your local copy. Do NOT
put tokens/credentials in either file — the template is committed.

### Environment variables

| Variable | Description | Default |
|---|---|---|
| `HASKELL_PROJECT_DIR` | Path to the Haskell project to load on startup | `process.cwd()` |
| `HASKELL_LIBRARY_TARGET` | Cabal library target override | auto-detected |
| `HASKELL_FLOWS_TELEMETRY` | Set to `1` to opt into **local-only** tool-usage telemetry (written to `.haskell-flows/telemetry.json` in the active project — never sent over the network). | `0` (off) |

---

## Toolchain auto-download

On the first tool call of any kind, the server kicks off background downloads of `hlint`, `fourmolu`, `hls` through a **host PATH → bundled → GitHub release → upstream fallback** pipeline. In-flight promises are cached per tool so concurrent callers share the same download. Every fetched binary is verified against the SHA256 declared in `src/tools/auto-download.ts` when present.

Tools that need a binary (e.g. `ghci_lint` needs `hlint`) call `awaitTool("hlint")` which either awaits the in-flight warmup or starts a fresh `ensureTool()` if none is pending.

**Security:** downloads only happen through the pre-configured resolution ladder. The server never executes a downloaded binary for lookup — it runs them only when a concrete tool (`ghci_lint`, `ghci_format`, `ghci_hls`) explicitly invokes them with `execFile`. The opt-in telemetry writes to a local file only — no network calls.

---

## Publishing toolchain assets

The repo's GitHub release `tools-v1.0` hosts the pre-verified binaries consumed by `auto-download.ts`. See [docs/PUBLISH_ASSETS.md](docs/PUBLISH_ASSETS.md) for the full operator runbook. TL;DR:

```bash
cd mcp-server
./scripts/publish-release-assets.sh hlint darwin-arm64 ./downloads/hlint
```

---

## Testing

```bash
cd mcp-server
npm install
npm run build               # required before test:e2e (compiles dist/index.js)

npm test                    # Unit (~855 tests, ~17s, pure TS — no GHC)
npm run test:integration    # Integration (~89 tests, ~17s, real GHCi — forks, 4 workers)
npm run test:e2e            # E2E (~130 tests, ~42s, full MCP + cabal — forks, 2 workers)
npm run test:all            # sequential: unit → integration → e2e
```

- Integration / E2E skip gracefully if GHC is not available.
- E2E requires `npm run build` first; a `pretest:e2e` hook enforces this.
- Each worker gets its own isolated fixture copy (`setupIsolatedFixture()`), so suites are safe under parallel execution; worker caps (4 / 2) cover cabal-cache contention.

---

## Project structure

```
haskell-rules-and-mcp/
├── .claude/rules/          # Project-specific Claude rules
├── .mcp.json               # MCP server configuration
├── docs/PUBLISH_ASSETS.md  # Operator runbook for tools-v1.0 release
├── mcp-server/
│   ├── src/
│   │   ├── index.ts                 # MCP entry, tool registration
│   │   ├── ghci-session.ts          # GHCi child process management
│   │   ├── workflow-state.ts        # State + contextual _guidance
│   │   ├── property-store.ts        # .haskell-flows/properties.json
│   │   ├── project-manager.ts       # Project discovery
│   │   ├── telemetry.ts             # Opt-in local tool-usage counters
│   │   ├── parsers/                 # GHC/HPC/browse parsers
│   │   ├── tools/                   # One file per MCP tool
│   │   │   ├── registry.ts          # registerStrictTool wrapper (+ warmup hook)
│   │   │   ├── toolchain-warmup.ts  # Background download coordinator
│   │   │   ├── create-project.ts    # ghci_create_project
│   │   │   ├── add-modules.ts       # ghci_add_modules
│   │   │   └── ...
│   │   ├── laws/                    # Property-suggestion engines
│   │   ├── resources/               # MCP resource handlers
│   │   └── __tests__/               # Unit / integration / e2e
│   ├── rules/                       # Markdown rule files served as resources
│   ├── scripts/
│   │   └── publish-release-assets.sh
│   ├── vendor-tools/                # Bundled binaries (+ manifest with SHA256)
│   └── dist/                        # Compiled JavaScript
└── playground/
    └── hindley-milner/              # Example project
```

---

## Gotchas

Agent-facing quirks surfaced during real usage. Reading these up front saves a
debug loop.

### GHC2024 + `` `elem` "literal" `` ambiguity
```haskell
-- ✗ Fails under GHC2024 with "Ambiguous type variable"
digits <- munch1 (`elem` "0123456789")

-- ✓ Use Data.Char.isDigit (or any monomorphic predicate)
import Data.Char (isDigit)
digits <- munch1 isDigit
```
Root cause: `elem :: Foldable t => a -> t a -> Bool`. GHC2024 enables
extended defaulting that does NOT resolve `t ~ []` automatically, so the
string literal's container type stays ambiguous. Use named predicates.

### `ghci_load` scope semantics — `:l` vs `:add`
By default `ghci_load(module_path="src/A.hs")` issues `:l A.hs`, which is
GHCi's native **replace** semantics: any previously loaded module not
transitively reachable from A is dropped from scope. If you need to keep
prior scope (e.g. a property in module B references a function from module
C), pass `mode="additive"`:
```
ghci_load(module_path="src/C.hs", mode="additive")  -- preserves prior scope
```
`load_all=true` is still preferred when you want the whole project in
scope for a batch of properties.

### Partial Prelude functions and `read`
`read`, `head`, `tail`, `fromJust`, `(!!)` are partial — they throw on
malformed input. The basic-lint fallback (active when hlint is unavailable)
flags these as warnings. Prefer `readMaybe`, pattern matching, or
`listToMaybe`.

### Formatter and lint availability
When `hlint`/`fourmolu`/`ormolu` are not on PATH and the bundled download
fails, `ghci_lint` and `ghci_format` return degraded responses with
`gateEligible: false`. The module-complete gate stays locked until a real
formatter/linter runs — by design; never trust a degraded "clean" signal.

---

## Changelog

- **Fase 4 (hyper-stabilization)** — (a) centralized release manifest
  `vendor-tools/bundled-tools-manifest.json` as single source of truth for
  tool URLs/versions; `auto-download.ts` now reads from it. (b) `tools-v1.0`
  GitHub release assets renamed to match platform-suffixed names expected by
  the MCP. (c) `npm run tools:validate:urls` + CI gate (`.github/workflows/ci.yml`).
  (d) `ghci_format` degraded fallback (trailing whitespace / CRLF / missing
  newline / tabs) with `gateEligible: false`. (e) `basic-lint-rules` reduced
  to lexically-safe rules; false positives removed. (f) `ghci_load(mode="additive")`
  for cross-module property tests. (g) `LawEngine` interface + 3 new engines
  (evaluator-preservation, constant-folding-soundness, functor-laws) exposed
  via `ghci_suggest(analyze)`. (h) `label` field on `ghci_quickcheck`,
  propagated to the exported `test/Spec.hs` with sanitization and dedup.
  (i) `ghci_workflow(action="gate")` orchestrates regression + cabal_test +
  cabal_build in a single consolidated call.
- **Fase 3** — upstream fallback URLs for auto-download, opt-in local telemetry, operator runbook for `tools-v1.0`, `ghci_suggest(analyze)` cross-module browse fix, `cabal_coverage` HTML report parser as a third fallback, README refresh.
- **Fase 2** — toolchain warmup (`toolchain-warmup.ts`), global strict Zod validation via `registerStrictTool`, mass migration of ~42 tools, removal of 8 peripheral tools (`init`, `scaffold`, `fuzz_parser`, `watch`, `profile`, `flags`, `equiv`, `trace`), `cabal_coverage` tix fallback, publish-release script.
- **Fase 1** — tautology law removal in `function-laws`, `ghci_check_module` export-list awareness, `ghci_toolchain_status` state propagation, `ghci_create_project` + `ghci_add_modules` (replacing `ghci_init` + `ghci_scaffold`), `ghci_lint` degraded fallback (Plan B).
