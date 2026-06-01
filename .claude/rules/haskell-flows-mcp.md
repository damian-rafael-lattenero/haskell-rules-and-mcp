# haskell-flows — agent workflow rules

You are connected to the `haskell-flows` MCP (35 tools). Use it for ALL Haskell work;
do not shell out to cabal/ghc/ghci/hlint directly.

## Session handshake

1. `ghc_workflow(action="status")` — confirm alive + 35 tools
2. `ghc_toolchain(action="status")` — external-binary gates
3. `ghc_workflow(action="help")` — state-aware nudges

## Situation → tool

| Situation | Tool | Example |
|---|---|---|
| new data T declared | `ghc_arbitrary` | `type_name="T"` |
| typed hole or empty stub | `ghc_hole` | `module_path="src/X.hs"` |
| want QuickCheck laws from a signature | `ghc_suggest` | `function_name="f"` |
| check a property | `ghc_quickcheck` | `property="\\x -> ...", module="src/X.hs"` |
| check property stability | `ghc_quickcheck` | `property="...", runs=5` |
| replay persisted properties | `ghc_property_store` | `action="run"` |
| materialize test/Spec.hs | `ghc_property_store` | `action="export"` |
| list persisted property store | `ghc_property_store` | `action="list"` |
| audit property store for contradictions | `ghc_property_store` | `action="audit"` |
| rename a local binding | `ghc_refactor` | `action="rename_local", scope_line_start=, scope_line_end=` |
| add a dependency | `ghc_deps` | `action="add", package="X", stanza="library"|"test-suite"` |
| register new modules | `ghc_modules` | `action="add", modules=["Foo.Bar"]` |
| de-register modules | `ghc_modules` | `action="remove", modules=["Foo.Old"], delete_files=false` |
| add a missing import | `ghc_add_import` | `name="Data.Map"` |
| apply a module export list | `ghc_apply_exports` | `module_path="src/X.hs", exports=["foo"]` |
| list live imports in GHCi | `ghc_imports` | `(no args)` |
| browse a module | `ghc_browse` | `module="Foo.Bar"` |
| fix a GHC warning | `ghc_fix_warning` | `module_path="src/X.hs"` |
| coverage report | `ghc_coverage` | `(no args, 8 HPC metrics)` |
| lint (matches CI) | `ghc_lint` | `path="src/"` |
| format source | `ghc_format` | `module_path="src/X.hs", write=true` |
| module gate | `ghc_check_module` | `module_path="src/X.hs"` |
| project-wide gate | `ghc_check_project` | `(no args)` |
| pre-push finalizer | `ghc_gate` | `(regression + cabal test + cabal build)` |
| scaffold a new project | `ghc_project` | `action="create", name="my-pkg"` |
| validate .cabal | `ghc_project` | `action="validate"` |
| toolchain gates (cabal/ghc/hlint) | `ghc_toolchain` | `action="status"` |
| toolchain warmup (probe optional bins) | `ghc_toolchain` | `action="warmup"` |
| batch N tool calls | `ghc_batch` | `actions=[{tool,args},...]` |
| install host rules (no repo clone) | `ghc_project` | `action="bootstrap", host="claude-code"|"cursor"|"generic", write=false` |
| switch active project root | `ghc_project` | `action="switch", path="/abs/path/to/project"` |
| what should I do next | `ghc_workflow` | `action="help"` |

## Per-response push

Every successful tool call carries a `nextStep` field:

```json
{ "success": true,
  "nextStep": {
    "tool":  "<next tool>",
    "why":   "<one-line rationale>",
    "example": { "<arg>": "<value>" },
    "chain": [ { "tool": "...", "args": {...} }, ... ]
  }
}
```

The optional `chain` lets you batch multi-step plans via
`ghc_batch(actions=chain)` in a single round-trip.

## Invariants

- **Never** edit `.cabal` by hand for deps — use `ghc_deps`.
- **Never** sed/awk `.hs` files — use `ghc_refactor`.
- **Never** shell out to `cabal`/`ghc`/`ghci`/`hlint`.

## Liveness + safety

- In-process GHC API session, single-writer per `HscEnv` via `MVar`.
- Any uncaught exception in a tool evicts the session — the next
  call boots a fresh `HscEnv` automatically.
- `ghc_eval` / `ghc_quickcheck` / `ghc_property_store` have a 30 s
  inner per-call budget; a trip is reported as
  `error_kind: "timeout"` with `resetHscEnvInPlace`.
- `Server.runTool` wraps every handler in a 10-min outer ceiling
  as defence-in-depth.
- Path traversal impossible by construction (`ModulePath` smart
  constructor).
- All external subprocesses argv-form — no shell interpolation.

## Full tool inventory (35)

- `ghc_load`
- `ghc_type`
- `ghc_info`
- `ghc_eval`
- `ghc_quickcheck`
- `ghc_hole`
- `ghc_arbitrary`
- `hoogle_search`
- `ghc_workflow`
- `ghc_check_module`
- `ghc_coverage`
- `ghc_complete`
- `ghc_format`
- `ghc_gate`
- `ghc_deps`
- `ghc_doc`
- `ghc_goto`
- `ghc_refactor`
- `ghc_lab`
- `ghc_explain_error`
- `ghc_perf`
- `ghc_witness`
- `ghc_batch`
- `ghc_lint`
- `ghc_toolchain`
- `ghc_check_project`
- `ghc_suggest`
- `ghc_add_import`
- `ghc_apply_exports`
- `ghc_fix_warning`
- `ghc_imports`
- `ghc_browse`
- `ghc_modules`
- `ghc_project`
- `ghc_property_store`

## Dogfood-fix-in-place

Tool misbehaves → edit `mcp-server-haskell/src/...` → add a
regression test in `test/Spec.hs` → `scripts/ci-local.sh
--fast` → commit+push. Keep dogfooding with the stale
running binary; CI validates, the fix lands on the next
natural reinstall.
