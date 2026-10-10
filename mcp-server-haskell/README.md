# haskell-flows-mcp

Operational README for the MCP server. User-facing overview lives in
the [repo root](../README.md); this page covers install, wiring, the
tool reference, and the development loop.

## Install

```bash
# From source (requires ghcup + cabal — see docs/requirements.md)
git clone https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp
cd haskell-rules-and-mcp/mcp-server-haskell
cabal install exe:haskell-flows-mcp \
  --installdir="$HOME/.local/bin" \
  --install-method=copy \
  --overwrite-policy=always
```

Pre-built binaries (darwin-arm64, linux-x64) + sdist ship on every
`v*` tag — see [Releases](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases).
Verify any install with:

```bash
haskell-flows-mcp --version   # single-sourced from the .cabal
haskell-flows-mcp --doctor    # toolchain table, exit 0/1
```

## Wire to your MCP client

Point the client at the binary (stdio). Shape varies by host:

**Claude Desktop / Claude Code** — add to `~/.claude.json`:

```json
{
  "mcpServers": {
    "haskell-flows": {
      "type": "stdio",
      "command": "/absolute/path/to/.local/bin/haskell-flows-mcp",
      "args": [],
      "env": {
        "HASKELL_PROJECT_DIR": "/absolute/path/to/your/haskell/project"
      }
    }
  }
}
```

Any stdio-MCP host works. The server reads its project root from
`HASKELL_PROJECT_DIR` (default: first create/switch anchors it;
relative paths resolve against the server's startup cwd). No rules
file needed — the `initialize` handshake ships a situation→tool
table derived from the live registry.

## Tool reference (12 tools)

| Tool | Actions |
|---|---|
| `ghc_project` | `create` `switch` `list` `validate` `bootstrap` |
| `ghc_module` | `add` `remove` · scratchpad `write` `check` `list` `show` `clear` `promote` · `write_module` (whole-module authorship; dotted names derive `src/…` paths) |
| `ghc_edit` | refactor verbs (snapshot + compile-verify + rollback) · `import` `exports` `fix_warning` `format` |
| `ghc_check` | `load` `module` `project` `lint` |
| `ghc_eval` | evaluate an expression in module context |
| `ghc_inspect` | `type` `hole` `info` `browse` `complete` `goto` `doc` |
| `ghc_deps` | `add` `remove` `list` `explain` |
| `ghc_suggest` | property-law candidates (8 engines) |
| `ghc_property` | `check` (run + auto-persist) · `run` `list` `export` `audit` · `arbitrary` |
| `ghc_gate` | pre-push gate: build + tests + lint + property replay |
| `ghc_batch` | multi-tool macro in one round-trip |
| `ghc_session` | `status` `help` `toolchain` `warmup` `imports` + workflow verbs |

Retired names (e.g. `ghc_modules`, `ghc_quickcheck`,
`ghc_property_store`, `ghc_refactor`, `ghc_load`) map to successors
in [docs/TOOL_TAXONOMY.md](../docs/TOOL_TAXONOMY.md).

## Development loop

```bash
./scripts/gate.sh t0   # build all three targets
./scripts/gate.sh t1   # + unit suite (775) + deadlock canary
./scripts/gate.sh t2   # + full e2e (424 checks), hangwatch-guarded
./scripts/probe.sh "Scenario title"   # one scenario, fresh-binary law
```

No commit without `t1` green; no push without `t2`. The e2e suite
spawns real fixtures under `$TMPDIR` and talks real JSON-RPC over
stdio — it exercises the transport, the engine, and the file
system together.

## Layout

```
src/HaskellFlows/
  Mcp/        envelope, transport, server, workflow state, doctor
  Tool/       one module per tool family + shared routing
  Ghc/        IdeSession — the embedded ghcide engine (only backend)
  Parser/     Error/Hole/Type/TypeSignature/Cabal parsers
  Data/       property store, scratchpad
app/          Main — flags (--version/--doctor/--help), stdio boot
test/         unit suite (Spec.hs + Spec/ domain modules)
test-e2e/     scenario suite + fixtures
scripts/      gate.sh, probe.sh, hangwatch.py, dogfood/
```

BSD-3-Clause. Windows is not supported.
