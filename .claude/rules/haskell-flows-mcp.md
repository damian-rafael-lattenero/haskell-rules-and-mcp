# haskell-flows — agent workflow rules

You are connected to the `haskell-flows` MCP (12 tools). Use it for ALL Haskell work;
do not shell out to cabal/ghc/ghci/hlint directly.

## Session handshake

1. `ghc_session(action="status")` — confirm alive + 12 tools
2. `ghc_session(action="toolchain")` — external-binary gates
3. `ghc_session(action="help")` — state-aware nudges

## Situation → tool

| Situation | Tool | Example |
|---|---|---|
| new data T declared | `ghc_property` | `action="arbitrary", type_name="T"` |
| typed hole or empty stub | `ghc_inspect` | `action="hole", module_path="src/X.hs"` |
| want QuickCheck laws from a signature | `ghc_suggest` | `function_name="f"` |
| check a property | `ghc_property` | `property="\\x -> ...", module="src/X.hs"` |
| check property stability | `ghc_property` | `property="...", runs=5` |
| replay persisted properties | `ghc_property` | `action="run"` |
| materialize test/Spec.hs | `ghc_property` | `action="export"` |
| list persisted property store | `ghc_property` | `action="list"` |
| audit property store for contradictions | `ghc_property` | `action="audit"` |
| rename a local binding | `ghc_edit` | `action="rename_local", scope_line_start=, scope_line_end=` |
| add a dependency | `ghc_deps` | `action="add", package="X", stanza="library"|"test-suite"` |
| register new modules | `ghc_module` | `action="add", modules=["Foo.Bar"]` |
| de-register modules | `ghc_module` | `action="remove", modules=["Foo.Old"], delete_files=false` |
| add a missing import | `ghc_edit` | `action="import", name="Data.Map"` |
| apply a module export list | `ghc_edit` | `module_path="src/X.hs", exports=["foo"]` |
| list live imports in the interactive session | `ghc_session` | `action="imports"` |
| browse a module | `ghc_inspect` | `action="browse", module="Foo.Bar"` |
| fix a GHC warning | `ghc_edit` | `action="fix_warning", module_path="src/X.hs", line=12, code="GHC-66111"` |
| lint (matches CI) | `ghc_check` | `action="lint", path="src/"` |
| format source | `ghc_edit` | `action="format", module_path="src/X.hs", write=true` |
| module gate | `ghc_check` | `action="module", module_path="src/X.hs"` |
| project-wide gate | `ghc_check` | `(no args)` |
| pre-push finalizer | `ghc_gate` | `(regression + cabal test + cabal build)` |
| scaffold a new project | `ghc_project` | `action="create", name="my-pkg"` |
| validate .cabal | `ghc_project` | `action="validate"` |
| toolchain gates (cabal/ghc/hlint) | `ghc_session` | `action="toolchain"` |
| toolchain warmup (probe optional bins) | `ghc_session` | `action="warmup"` |
| batch N tool calls | `ghc_batch` | `actions=[{tool,args},...]` |
| install host rules (no repo clone) | `ghc_project` | `action="bootstrap", host="claude-code"|"cursor"|"generic", write=false` |
| switch active project root | `ghc_project` | `action="switch", path="/abs/path/to/project"` |
| what should I do next | `ghc_session` | `action="help"` |

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
- **Never** sed/awk `.hs` files — use `ghc_edit`.
- **Never** shell out to `cabal`/`ghc`/`ghci`/`hlint`.

## Liveness + safety

- ghcide engine: one `IdeSession`, single-writer via `MVar`.
- Any uncaught exception in a tool drops the session — the next
  call boots a fresh one automatically.
- `ghc_eval` / `ghc_property` have a 30 s inner per-call budget;
  a trip is reported as `error_kind: "timeout"` and drops the
  session for a clean reboot.
- `Server.runTool` wraps every handler in a 10-min outer ceiling
  as defence-in-depth.
- Path traversal impossible by construction (`ModulePath` smart
  constructor).
- All external subprocesses argv-form — no shell interpolation.

## Full tool inventory (12)

- `ghc_check`
- `ghc_eval`
- `ghc_property`
- `ghc_session`
- `ghc_edit`
- `ghc_module`
- `ghc_inspect`
- `ghc_gate`
- `ghc_deps`
- `ghc_batch`
- `ghc_suggest`
- `ghc_project`

## Dogfood-fix-in-place

Tool misbehaves → edit `mcp-server-haskell/src/...` → add a
regression test in `test/Spec.hs` → `scripts/gate.sh t1` →
commit+push. Keep dogfooding with the stale
running binary; CI validates, the fix lands on the next
natural reinstall.
