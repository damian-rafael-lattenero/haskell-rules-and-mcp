# Use the haskell-flows MCP for ALL Haskell development

This project ships an MCP server (`haskell-flows`) that owns the Haskell
dev loop end-to-end: a persistent GHC-API session, compiler-driven
refactors, property-first testing, cabal-safe edits, and HPC coverage.
The MCP is the authoritative Haskell toolchain wrapper for this repo.

**You MUST use it for all Haskell work.** Do not shell out to `cabal`,
`ghc`, `ghci`, `hlint`, `fourmolu`, `ormolu`, or `hoogle` via Bash —
the MCP already does it with structured output, proper sandboxing, and
invariant checks the ad-hoc commands miss.

**Bash exceptions** (allowed because they replicate CI gates or the MCP
itself cannot run them):

- `scripts/ci-local.sh --fast` or `scripts/ci-local.sh` — **on-demand
  only**, when the user explicitly asks. See §6.
- `cabal install exe:haskell-flows-mcp ...` and `scripts/install-mcp.sh`
  — to rebuild the MCP binary.

---

## §1 — Start-of-session handshake

Before writing any Haskell code, always run these three:

1. `ghc_workflow(action="status")` — confirms the MCP process is alive,
   reports `projectDir`, `staleness`, and the current `toolsActive` set.
   Compare the returned tool count against `docs/TOOL_TAXONOMY.md` —
   that file is the canonical inventory. If they disagree, the on-disk
   binary is newer than the running one; reinstall + relaunch before
   trusting tool behaviour.
2. `ghc_toolchain(action="status")` — probes `cabal`, `ghc`, `hlint`
   (blocking gates) plus `fourmolu`/`ormolu`/`hoogle`/`hls` (degrade
   gracefully). Any blocking gate down → stop and report.
3. `ghc_workflow(action="help")` — state-aware nudges based on phase
   (PreScaffold / Scaffolded / Loaded / Tested). Replaces guessing
   "what do I do next?" with a runtime-grounded suggestion.

If any of the three fails, stop and report — do not work around a
broken MCP by writing Haskell code by hand.

---

## §2 — Reference: where to look up tool details

The rules file does not duplicate per-tool documentation. The sources
of truth, in order:

- **`docs/TOOL_TAXONOMY.md`** — the canonical inventory of every
  registered tool, classified as Primitive / Composite / Gate /
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
| New `data T = ...` declared | `ghc_arbitrary(type_name="T")` | Generates the `Arbitrary` template; manual write skips the polymorphic-context heuristic. |
| Function has a `_` hole or empty stub | `ghc_hole(module_path="src/X.hs")` | Single call returns every hole + type + in-scope fits + bindings; reading errors by hand misses the fits. |
| Want properties from a function's signature | `ghc_suggest(function_name="f")` | Confidence-scored law candidates from the signature; eyeballing a sig misses non-obvious laws. |
| Checking a law holds | `ghc_quickcheck(property="…", module="src/X.hs")` | Auto-persists on pass to `.haskell-flows/properties.json`; raw QC in the REPL drops the regression. |
| Replay all persisted properties | `ghc_property_store(action="run")` | Loads the store + reloads the right module per property; replaying by hand drifts. |
| Inspect / export / audit the store | `ghc_property_store(action="list" \| "export" \| "audit")` | One tool, four actions; legacy split (`ghc_regression`, `ghc_quickcheck_export`, etc.) was retired in #94 Phase C. |
| Renaming a local identifier | `ghc_refactor(action="rename_local", scope_line_start=…)` | Snapshot-and-compile-verify; `sed` cannot roll back on type-error. |
| Moving a top-level definition between modules | `ghc_refactor(action="move_symbol", …)` | Replaces the retired `ghc_move`; same snapshot guarantee. |
| Adding a dependency | `ghc_deps(action="add", package="X", stanza="library" \| "test-suite[:NAME]" \| …)` | Post-edit invariant rejects writes whose re-parsed dep list disagrees; hand-edit risks malformed cabal. |
| Explaining where a dep is used | `ghc_deps(action="explain", package="X")` | Replaces the retired `ghc_deps_explain`. |
| Adding / removing exposed-modules | `ghc_modules(action="add" \| "remove", modules=[…])` | Scaffolds source + registers in cabal in one call; manual edit splits the two and drifts. |
| Adding a missing import | `ghc_add_import(name="X")` | Hoogle-resolves the module; hand-importing means guessing the package. |
| Rewriting a module's export list | `ghc_apply_exports(module_path="…", exports=[…])` | Idempotent header rewrite + reserved-keyword validation. |
| Auto-fixing a GHC warning | `ghc_fix_warning(module_path="…")` | Patches the common GHC codes (66111, 40910, missing-sigs); not a substitute for `ghc_explain_error` on type errors. |
| Decoding a confusing GHC error | `ghc_explain_error(error_text="…")` | Structured error analysis + verifiable patch suggestion; `ghc_fix_warning` is for warnings only. |
| Listing imports currently in scope | `ghc_imports()` | Reads the live GHC session; checking source files misses MCP-injected preloads. |
| Listing names exported by a module | `ghc_browse(module="Foo.Bar")` | Resolves against the loaded module graph; off-graph modules → `hoogle_search`. |
| Searching upstream Hackage | `hoogle_search(query="…")` | Off-graph; for in-project lookups use `ghc_browse` or `ghc_info`. |
| Quick `:t expr` / `:i name` / `:doc name` / `:complete prefix` | `ghc_type` / `ghc_info` / `ghc_doc` / `ghc_complete` | Same as the GHCi commands; cheaper than reloading. |
| Jumping to a name's definition | `ghc_goto(name="…")` | Returns file + line from "Defined at"; faster than grep when you have a session. |
| Eval a single Haskell expression | `ghc_eval(expr="…")` | 64 KiB output cap; not for mutating state. |
| Checking a single module is clean | `ghc_check_module(module_path="…")` | Aggregates compile + warnings + holes + property replay. |
| Checking the whole project | `ghc_check_project()` | Same gates over every exposed-module + other-module. |
| HLint over the project | `ghc_lint(path="mcp-server-haskell")` | Recursive, matches CI; prefer over module-only `ghc_lint(module_path=…)` for the pre-push gate. |
| Coverage report | `ghc_coverage()` | `cabal test --enable-coverage` + 8 HPC metrics parsed. |
| Pre-push finalizer | `ghc_gate()` | One-shot composite: regression + tests + build. Use just before commit/push. |
| Module-wide property discovery | `ghc_lab(module_path="…")` | Browses the module + suggests laws + runs them + persists. |
| Performance regression | `ghc_perf(expr="…")` | Wall-clock harness with baseline save/compare; not a Criterion replacement (yet). |
| Distribution sanity for a property | `ghc_witness(property="…")` | Reports input-shape distribution; reveals trivial-input bias QC alone misses. |
| New project from scratch | `ghc_project(action="create", …)` | Replaces retired `ghc_create_project`. Pair with `ghc_modules(action="add")` next. |
| Switching the active project | `ghc_project(action="switch", …)` | Replaces retired `ghc_switch_project`. Reopens the property store against the new root. |
| Cabal sanity check | `ghc_project(action="validate")` | Replaces retired `ghc_validate_cabal`. |
| Bootstrap host rules | `ghc_project(action="bootstrap")` | Replaces retired `ghc_bootstrap`. |
| Toolchain probe / warmup | `ghc_toolchain(action="status" \| "warmup")` | Replaces retired `ghc_toolchain_status` + `ghc_toolchain_warmup`. |
| Format check / write | `ghc_format(write=true \| false)` | fourmolu (preferred) or ormolu; reports `unavailable` if neither is on PATH. |
| Sequence multiple tools in one round-trip | `ghc_batch(actions=[…])` | Accepts both `{tool, args}` and `{name, arguments}`; `fail_fast=true` is default. |
| Lost / unsure | `ghc_workflow(action="help")` | State-aware nudge based on current phase + history; pairs with `nextStep` (§4). |

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
- It replaces the need to call `ghc_workflow(action="next")` after
  every successful tool.
- Errors suppress the hint — when `success: false`, read the error and
  decide. Don't look for a `nextStep` that isn't there.
- A few tools deliberately suppress `nextStep`: `ghc_workflow` (would
  loop on itself) and `ghc_batch` (the sub-actions carry their own
  hints).

---

## §5 — Hard rules / anti-patterns

These are non-negotiable. Violating them risks correctness regressions
the MCP's invariants would otherwise catch.

- **Never** `sed` / `awk` / `perl -i` over Haskell sources. Use
  `ghc_refactor`. The snapshot-and-compile-verify invariant rolls the
  file back atomically on any type-check failure.
- **Never** edit `.cabal` build-depends by hand. Use `ghc_deps`. The
  post-edit invariant rejects writes whose re-parsed dep list
  disagrees with the verb (added / removed).
- **Never** edit `exposed-modules` / `other-modules` by hand. Use
  `ghc_modules`. Scaffolding the file and registering it in cabal go
  together; doing them separately drifts.
- **Never** rewrite a module's export list by hand. Use
  `ghc_apply_exports`. It validates against reserved keywords and
  re-checks the module after.
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
`.zprofile` and inherit a minimal PATH. `serverForRaw` augments PATH
internally so MCP tools work, but Bash one-liners need it explicitly:

```sh
PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH" \
  hlint mcp-server-haskell/
```

### Run hlint full-repo before every push

GitHub's `Haskell CI` job runs `hlint mcp-server-haskell/` recursively
and treats any hint as a hard fail. `scripts/ci-local.sh --fast`
replicates this in step `[9/9]`, but the step has silently no-op'd in
the past when `~/.cabal/bin` wasn't on PATH. The cheap belt-and-
suspenders: as the very last pre-push gate, run the explicit command
above. On a clean tree it prints `No hints` in <2 s; any output is a
blocker. Common slips: `Redundant bracket` around `(["X" :: Text])`,
`Use isJust` for `expr /= Nothing`, `Use ++` vs `<>` confusion.

### `scripts/ci-local.sh` is on-demand only

**Never** run `scripts/ci-local.sh` (with or without `--fast`)
proactively. Not before push, not after a batch of edits, not as a
"safety check." The user runs it themselves on their own cadence.
This supersedes any older guidance to "always run before push."

The per-file `ghc_lint(module_path=…)` and per-module
`ghc_check_module` are still fine to run when they fit the work
naturally — the rule is specifically about `scripts/ci-local.sh`
orchestration.

### Push direct to master, no PRs

All work in this repo lands directly on `master`. Do not open PRs,
do not leave commits living on feature branches for review.

After committing on whatever branch (Claude-operated or personal):
fast-forward-merge into `master`, then `git push origin master`. The
work branch can stay on remote as a historical pointer. Do not call
`gh pr create`. Do not ask whether to open a PR — just land on master.

### Targeted E2E iteration

Never iterate on the full E2E suite when multiple scenarios fail. The
full serial suite is ~200 s; a single scenario like `Arbitrary` is
~18 s. Filter:

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
3. **Add a regression test** at `mcp-server-haskell/test/Spec.hs`.
4. **Commit + push directly to master** with a descriptive message.
5. **Keep working** with the stale running binary. Do NOT pause for
   `cabal install`, do NOT relaunch Claude. CI + unit tests are
   sufficient validation; in-vivo verification happens organically the
   next time the user restarts Claude Desktop.

Per-commit quality signal is the targeted regression test, not
`scripts/ci-local.sh`. The user runs ci-local on their own cadence
(see above).

**Exception**: if a bug blocks ALL further dogfooding, fall back to
the reinstall + relaunch cycle (`scripts/install-mcp.sh` + Cmd+Q +
relaunch). Default is "keep going."

When working on the MCP itself (`projectDir` is `mcp-server-haskell/`
or this repo's root), the MCP detects this and surfaces a `dogfood`
hint in `nextStep` after every successful write-tool call. Follow it.
