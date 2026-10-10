# Use the haskell-flows MCP for ALL Haskell development

This project ships an MCP server (`haskell-flows`) that owns the Haskell
dev loop end-to-end: an embedded ghcide session, compiler-driven edits,
property-first testing, and cabal-safe project management. The MCP is
the authoritative Haskell toolchain wrapper for this repo.

**You MUST use it for all Haskell work.** Do not shell out to `cabal`,
`ghc`, `ghci`, `hlint`, `fourmolu`, `ormolu`, or `hoogle` via Bash —
the MCP already does it with structured output, proper sandboxing, and
invariant checks the ad-hoc commands miss.

**Bash exceptions** (allowed because they replicate CI gates or the MCP
itself cannot run them):

- `scripts/gate.sh t0|t1|t2` — the tiered quality gate. T1 before every
  commit; T2 before every push.
- `cabal install exe:haskell-flows-mcp ...` and `scripts/install-mcp.sh`
  — to rebuild the MCP binary.

---

## §1 — Start-of-session handshake

Before writing any Haskell code, always run these three:

1. `ghc_session(action="status")` — confirms the MCP process is alive,
   reports `projectDir`, `staleness`, and the current `toolsActive` set.
   Compare the returned tool count against `docs/TOOL_TAXONOMY.md` —
   that file is the canonical inventory (12 tools). If they disagree,
   the on-disk binary is newer than the running one; reinstall +
   relaunch before trusting tool behaviour.
2. `ghc_session(action="toolchain")` — probes `cabal`, `ghc`, `hlint`
   (blocking gates) plus `fourmolu`/`ormolu`/`hoogle`/`hls` (degrade
   gracefully). Any blocking gate down → stop and report.
3. `ghc_session(action="help")` — state-aware nudges based on phase
   (the `phase` field is one of PhasePreScaffold / PhaseBootstrap /
   PhaseDeveloping / PhaseTestingLaws / PhaseReadyToPush). Replaces
   guessing "what do I do next?" with a runtime-grounded suggestion.

If any of the three fails, stop and report — do not work around a
broken MCP by writing Haskell code by hand.

---

## §2 — Reference: where to look up tool details

The rules file does not duplicate per-tool documentation. The sources
of truth, in order:

- **`docs/TOOL_TAXONOMY.md`** — the canonical inventory of every
  registered tool (12), classified as Primitive / Composite / Gate /
  Control-plane. CI-enforced via `testCategoryCountsMatchTaxonomy` in
  `test/Spec.hs`; any added/removed/renamed tool must update this file.
- **`docs/TOOL_DESCRIPTION_TEMPLATE.md`** — the 6-field shape
  (PURPOSE / WHEN TO USE / WHEN NOT TO USE / PREREQUISITES / OUTPUT
  SHAPE / SEE ALSO) every tool's `description` follows. New tools
  must conform; the description-shape lint in `Spec.hs` enforces it.
- **The tool's own `description`** in the schema (visible via
  `tools/list`) is the runtime source of truth — read it before
  inventing usage. The `nextStep` field in every successful response
  (see §4) tells you which tool the MCP thinks you should reach for
  next.

If a wire name appears in this file but not in `TOOL_TAXONOMY.md`,
the rules file is stale — fix the rules, not the taxonomy.

---

## §3 — Decision matrix: situation → tool

Cover every common situation. When two tools could fit, the matrix
picks one and the "why-this-not-that" column explains the
displacement.

| Situation | Use this | Why this, not that |
|---|---|---|
| New `data T = ...` declared | `ghc_property(action="arbitrary", type_name="T")` | Generates the `Arbitrary` template; manual write skips the polymorphic-context heuristic. |
| Function has a `_` hole or empty stub | `ghc_inspect(action="hole", module_path="src/X.hs")` | Single call returns every hole + type + in-scope fits + bindings; reading errors by hand misses the fits. |
| Want properties from a function's signature | `ghc_suggest(function_name="f")` | Confidence-scored law candidates from the signature; eyeballing a sig misses non-obvious laws. |
| Checking a law holds | `ghc_property(property="…", module="src/X.hs")` | Auto-persists on pass to `.haskell-flows/properties.json`; raw QC in the REPL drops the regression. |
| Replay all persisted properties | `ghc_property(action="run")` | Loads the store + reloads the right module per property; replaying by hand drifts. |
| Inspect / export / audit the store | `ghc_property(action="list" \| "export" \| "audit")` | One tool, four verbs; the legacy split was retired in #94 Phase C. |
| Renaming a local identifier | `ghc_edit(action="rename_local", scope_line_start=…)` | Snapshot-and-compile-verify; `sed` cannot roll back on type-error. |
| Moving a top-level definition between modules | `ghc_edit(action="move_symbol", …)` | Same snapshot guarantee as rename. |
| Extracting a binding | `ghc_edit(action="extract_binding", …)` | Compiler-verified; hand-cut risks scope capture. |
| Adding a dependency | `ghc_deps(action="add", package="X", stanza="library" \| "test-suite[:NAME]" \| …)` | Post-edit invariant rejects writes whose re-parsed dep list disagrees; hand-edit risks malformed cabal. |
| Explaining where a dep is used | `ghc_deps(action="explain", package="X")` | Grounded in the parsed cabal + source imports. |
| Adding / removing exposed-modules | `ghc_module(action="add" \| "remove", modules=[…])` | Scaffolds source + registers in cabal in one call; manual edit splits the two and drifts. |
| Adding a missing import | `ghc_edit(action="import", name="X")` | Hoogle-resolves the module; hand-importing means guessing the package. |
| Rewriting a module's export list | `ghc_edit(action="exports", module_path="…", exports=[…])` | Idempotent header rewrite + reserved-keyword validation. |
| Auto-fixing a GHC warning | `ghc_edit(action="fix_warning", module_path="…", line=N, code="GHC-NNNNN")` | Patches the common GHC codes (66111, 40910, missing-sigs); needs the line+code from a prior `ghc_check(action="module")` diagnostic. |
| Listing imports currently in scope | `ghc_session(action="imports")` | Reads the live session; checking source files misses MCP-injected preloads. |
| Listing names exported by a module | `ghc_inspect(action="browse", module="Foo.Bar")` | Resolves against the loaded module graph. |
| Quick type / info / completion | `ghc_inspect(action="type" \| "info" \| "complete", …)` | Same answers as the GHCi commands, cheaper than reloading. |
| Jumping to a name's definition | `ghc_inspect(action="goto", name="…")` | Returns file + line from "Defined at"; faster than grep when you have a session. |
| Eval a single Haskell expression | `ghc_eval(expression="…")` | 64 KiB output cap; not for mutating state. Parameter is `expression`, not `expr`. |
| Checking a single module is clean | `ghc_check(action="module", module_path="…")` | Aggregates compile + warnings + holes + property replay. |
| Checking the whole project | `ghc_check(action="project")` or `ghc_check()` | Same gates over every exposed-module + other-modules; parse errors included. |
| HLint over the project | `ghc_check(action="lint", path="mcp-server-haskell")` | Recursive, matches CI. |
| Pre-push finalizer | `ghc_gate()` | One-shot composite: regression + tests + build. Use just before commit/push. |
| New project from scratch | `ghc_project(action="create", …)` | Replaces retired `ghc_create_project`. Pass `path=` to scaffold into a fresh directory (auto-switches). |
| Switching the active project | `ghc_project(action="switch", …)` | Replaces retired `ghc_switch_project`. Reopens the property store against the new root. |
| Cabal sanity check | `ghc_project(action="validate")` | Replaces retired `ghc_validate_cabal`. |
| Bootstrap host rules | `ghc_project(action="bootstrap")` | Replaces retired `ghc_bootstrap`. |
| Toolchain probe / warmup | `ghc_session(action="toolchain" \| "warmup")` | Replaces retired `ghc_toolchain_status` + `ghc_toolchain_warmup`. |
| Format check / write | `ghc_edit(action="format", module_path=…, write=true \| false)` | fourmolu (preferred) or ormolu; reports `unavailable` if neither is on PATH. |
| Sequence multiple tools in one round-trip | `ghc_batch(actions=[…])` | Accepts both `{tool, args}` and `{name, arguments}`; `fail_fast=true` is default. |
| Lost / unsure | `ghc_session(action="help")` | State-aware nudge based on current phase + history; pairs with `nextStep` (§4). |
| Verify a snippet before touching real source | `ghc_module(action="write", code="…")` then `(action="check", id="…")` | Persistent + visible to the user; `ghc_eval` drops the state and offers no rollback path. |
| Promote a verified hypothesis into a real module | `ghc_module(action="promote", id="…", target_module="src/Foo.hs", target_line=N)` | Atomic snapshot-and-compile-verify; rolls back on compile fail. |
| Write a COMPLETE module body (new or overwrite) | `ghc_module(action="write_module", module="src/Foo.hs", code="module Foo where …")` | Same discipline as promote but whole-file. Register the module first (`action="add"`) — the cradle resolves by the .cabal's module list, so unregistered writes are unverifiable and roll back. |
| Inspect the reasoning trail | `ghc_module(action="list")` then `(action="show", id="…")` | Shows every hypothesis carried this session — readable mid-session by the user. |

---

## §4 — `nextStep` is the primary post-call navigation

Every successful tool call returns a `nextStep` object inside its
payload:

```json
{
  "success": true,
  "files_written": ["…"],
  "nextStep": {
    "tool":  "ghc_deps",
    "why":   "Your scaffold has only `base`. Add deps before wiring up modules.",
    "example": { "action": "add", "package": "QuickCheck", "stanza": "test-suite" },
    "chain":   [ /* optional multi-step plan agent can pass to ghc_batch */ ]
  }
}
```

- `tool` — the MCP's pick for what's most likely useful next.
- `why` — one-line rationale.
- `example` (optional) — canonical args you can pass verbatim.
- `chain` (optional) — multi-step plan; the shape is `ghc_batch`-ready,
  pass it as `ghc_batch(actions=chain)` to execute the whole sequence
  in one round-trip.

**Rules**:

- Follow `nextStep` when it fits. Ignore it (and pick your own path)
  when you have stronger context.
- It supersedes the retired `ghc_workflow(action="next")` (removed in
  #269): the per-response hint fires automatically after every tool, and
  `ghc_session(action="discover")` covers on-demand "what should I
  reach for next" by ranking unused tools.
- A few tools deliberately suppress `nextStep`: `ghc_session` (would
  loop on itself) and `ghc_batch` (the sub-actions carry their own
  hints).

---

## §5 — Hard rules / anti-patterns

These are non-negotiable. Violating them risks correctness regressions
the MCP's invariants would otherwise catch.

- **Never** `sed` / `awk` / `perl -i` over Haskell sources. Use
  `ghc_edit`. The snapshot-and-compile-verify invariant rolls the
  file back atomically on any type-check failure.
- **Never** edit `.cabal` build-depends by hand. Use `ghc_deps`. The
  post-edit invariant rejects writes whose re-parsed dep list
  disagrees with the verb (added / removed).
- **Never** edit `exposed-modules` / `other-modules` by hand. Use
  `ghc_module`. Scaffolding the file and registering it in cabal go
  together; doing them separately drifts.
- **Never** rewrite a module's export list by hand. Use
  `ghc_edit(action="exports")`. It validates against reserved
  keywords and re-checks the module after.
- **Never** shell out to `cabal` / `ghc` / `ghci` / `hlint` /
  `fourmolu` / `ormolu` / `hoogle` directly. Every one of those is
  wrapped by an MCP tool with structured output. The Bash exceptions
  in the file header apply.

---

## §6 — Project gotchas + dogfood flow

These are repo-specific lessons that an LLM cold-starting in this
codebase needs. Consolidates feedback that previously lived in user
memory.

### PATH dance

`hlint`, `fourmolu`, `ormolu`, `hoogle` install under
`~/.cabal/bin`; `cabal`, `ghc`, `ghc-pkg`, `hls` live in
`~/.ghcup/bin`. macOS non-login shells (Dock / Finder launches) skip
`.zprofile` and inherit a minimal PATH. The server augments PATH
internally so MCP tools work, but Bash one-liners need it explicitly:

```sh
PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH" \
  hlint mcp-server-haskell/
```

### Binary pickup: every MCP config must launch the cabal symlink

Claude Code reads MCP server defs from **two** places, and the global
one wins at launch:

1. the project `.mcp.json` (repo root), and
2. the global `~/.claude.json` — top-level `mcpServers` **and** any
   per-project `projects[<dir>].mcpServers`.

The canonical command for both is the cabal-install-managed **symlink**:

```
~/.cabal/bin/haskell-flows-mcp   →  ../store/.../<hash>/bin/haskell-flows-mcp
```

`cabal install exe:haskell-flows-mcp` repoints that symlink atomically
on every build, so **install + restart follows the symlink to the
newest binary with zero per-release config edits**. This only works if
the config launches the *symlink*, not a copy.

**The drift that bit us (2026-05-31):** the global `~/.claude.json`
entry pointed at a frozen **copy** in `~/.local/bin/haskell-flows-mcp`
(an old install step `cp`'d it there). `cabal install` refreshed the
symlink, but Claude kept launching the stale copy — so new actions
showed up as `"unknown action"` right after a green install.
`getExecutablePath` inside `ghc_eval` is the smoking-gun probe.

**The fix is automated in `scripts/install-mcp.sh`:**

- the default run rebuilds **and** reconciles both configs to the
  symlink (surgical string-replace, timestamped backup);
- `scripts/install-mcp.sh --reconcile` fixes configs **without** a
  rebuild — run it with Claude fully quit if a restart still launches a
  stale binary (a live session can rewrite `~/.claude.json` on quit);
- `scripts/install-mcp.sh --check` exits non-zero if the binary is
  stale **or** either config's command ≠ the symlink.

Staleness caveat: `ghc_session(action="status")` reports `stale=false`
by comparing on-disk mtimes — it does **not** know the *running
process* is an old binary (tracked as #280). The authoritative check
is `ghc_session(action="discover")`: if it succeeds, the new binary is
live; `"unknown action"` means the client is still on the old one →
restart (and `--reconcile` if needed).

### Run hlint full-repo before every push

GitHub's CI runs `hlint mcp-server-haskell/` recursively
and treats any hint as a hard fail. As the very last pre-push gate,
run the explicit command above. On a clean tree it prints
`No hints` in <2 s; any output is a blocker. Common slips:
`Redundant bracket` around `(["X" :: Text])`, `Use isJust` for
`expr /= Nothing`, `Use ++` vs `<>` confusion.

### `scripts/gate.sh` is the commit discipline

No commit without `scripts/gate.sh t1` green (build + unit + dogfood
canary). `t2` (full e2e under hangwatch) before every push. Small
commits per item; every e2e/probe run wrapped in a watchdog
(`scripts/hangwatch.py`) so a hang is a verdict, not a blocked
terminal.

### Push direct to master, no PRs

All work in this repo lands directly on `master`. Do not open PRs,
do not leave commits living on feature branches for review.

After committing on whatever branch (Claude-operated or personal):
fast-forward-merge into `master`, then `git push origin master`. The
work branch can stay on remote as a historical pointer. Do not call
`gh pr create`. Do not ask whether to open a PR — just land on master.

### Targeted E2E iteration

Never iterate on the full E2E suite when multiple scenarios fail. The
full serial suite is minutes; a single scenario is seconds. Filter:

```sh
HASKELL_FLOWS_E2E_ONLY="<substring>" \
  cabal test test-e2e --test-show-details=direct
```

Substring match is case-insensitive against the scenario label.
Combine with `HASKELL_FLOWS_E2E_SKIP_SLOW=1` for the fast lane. Run
the full suite only as the final gate, not in the loop. Avoid
`HASKELL_FLOWS_E2E_PARALLEL=N` for debugging — it interleaves
output and can race on tmp-dir CWD under load.

When a code change couples multiple failing scenarios, batch the
diagnosis: run the suite once, `tee` to a log, grep for failures,
plan fixes for ALL together. Never re-run the suite to see output
already captured — re-read the prior log with `grep -a`.

### Dogfood-fix-in-place flow

When an MCP tool returns a wrong result, a hang, an unexpected error,
or a clear bug:

1. **Log the finding** inline (`F-##` marker).
2. **Fix the MCP code** at `mcp-server-haskell/src/HaskellFlows/` via
   `Edit`/`Write`.
3. **Add a regression test** at `mcp-server-haskell/test/` (unit) or
   `test-e2e/Scenarios/` (e2e).
4. **Commit + push directly to master** with a descriptive message
   (T1 green first).
5. **Keep working** with the stale running binary. Do NOT pause for
   `cabal install`, do NOT relaunch the client. CI + unit tests are
   sufficient validation; in-vivo verification happens organically the
   next time the user restarts.

Per-commit quality signal is the targeted regression test, not the
full gate. The user runs deeper gates on their own cadence.

**Exception**: if a bug blocks ALL further dogfooding, fall back to
the reinstall + relaunch cycle (`scripts/install-mcp.sh` + quit +
relaunch). Default is "keep going."

When working on the MCP itself (`projectDir` is `mcp-server-haskell/`
or this repo's root), the MCP detects this and surfaces a `dogfood`
hint in `nextStep` after every successful write-tool call. Follow it.
