# TypeScript MCP — retrospective from the Haskell port

Findings gathered while porting the original TypeScript MCP server
(`mcp-server/`) to Haskell (`mcp-server-haskell/`) across Phase 1-8.
The port gave me a forced 1:1 read of every tool's semantics — the
incidents below are all things I actually hit while *using* the TS MCP
to drive the Haskell work, not hypothetical concerns.

Each entry cites the phase where it bit me, the concrete impact, and
a concrete fix. Items that became tracked issues on GitHub link to
them at the bottom of their section.

## Table of contents

- [🐛 Bugs — reproduced, needs fix](#-bugs--reproduced-needs-fix)
- [🔳 Gaps — missing features](#-gaps--missing-features)
- [📉 Redundant — candidates for removal](#-redundant--candidates-for-removal)
- [💡 Game-changing additions](#-game-changing-additions)
- [🏆 What works well — preserve](#-what-works-well--preserve)
- [Issue tracker cross-reference](#issue-tracker-cross-reference)

---

## 🐛 Bugs — reproduced, needs fix

### B1. `ghci_deps(action="add")` wipes the library `build-depends` list
**Phase:** 2 **Severity:** critical (silent data loss)

Called `ghci_deps(action="add", package="QuickCheck", version=">= 2.14 && < 2.16")`
during Phase 2 to add QuickCheck to the test suite. The tool rewrote
the library stanza's `build-depends:` block, deleting 9 deps (aeson,
async, bytestring, directory, filepath, process, regex-tdfa, stm,
text) and replacing them with just `base + QuickCheck`. The
system-reminder that surfaced the linter's output in that turn was
the first warning; the tool itself reported `"success":"added"`.

**Impact.** Without catching this, the next `cabal build` would have
failed with 30+ unknown-symbol errors, and the recovery path (manually
restoring 9 deps from memory or git) is expensive. For an agent
driving in a fresh checkout, it's a guaranteed broken state.

**Fix.** Insert a comma-prefixed entry instead of rewriting the whole
block, and preserve the existing list byte-for-byte. The Haskell port's
`HaskellFlows.Tool.Deps` does this — line-oriented parser splits on
top-level commas, appends without rewriting unrelated entries.

### B2. Session silently re-routes to a different project
**Phase:** 5, 6, 8 **Severity:** medium (data integrity, not corruption)

Three times during the session, `ghci_session(status)` returned
`projectDir: /playground/hindley-milner` when I had been working in
`mcp-server-haskell/` for dozens of calls. No event visible to the
agent precipitated the switch. Recovery required
`ghci_switch_project(project="haskell-flows-mcp")` and losing the
state of 25+ already-loaded modules (subsequent `ghci_load` calls
warm-restart the cache, so not expensive, but confusing).

**Impact.** A tool call made against the wrong project directory could
edit the wrong files — e.g. a `ghci_load` of `src/Foo.hs` resolves
relative to the active project, so the agent operates on
`/playground/hindley-milner/src/Foo.hs` thinking it's
`mcp-server-haskell/src/Foo.hs`. Path-traversal guards don't catch
this because the path IS in the active project — it's just the wrong
active project.

**Fix.** Don't auto-scan for `.cabal` files on tool entry; only update
the active project on explicit `ghci_switch_project` or the initial
`HASKELL_PROJECT_DIR` resolution. Emit a warning (not a silent
switch) if an event tries to route.

### B3. `ghci_lint` is per-file; CI is recursive → author-time drift
**Phase:** 5, 7, 8 **Severity:** medium (workflow hazard)

`ghci_lint(module_path="src/Foo.hs")` is file-scoped. The natural
pattern is to lint the file you just touched. CI runs
`hlint mcp-server-haskell/` recursively, including `test/`. The same
anti-pattern (`parseX "..." == Nothing`) slipped into the test suite
**three times** (Phases 5, 7, 8) because I never lint-scanned
`test/Spec.hs`. CI caught it each time; each fix required a second
push.

**Impact.** Author-time → CI gap is a reliability tax. Every slip is
a push-fix-push cycle that wouldn't have happened with parity.

**Fix.** Teach `ghci_lint` to accept a directory: `ghci_lint(path="mcp-server-haskell/")`
recurses and returns per-file results. Keep the single-file form as
a fast inner-loop escape hatch. Workaround shipped:
[`scripts/ci-local.sh`](../scripts/ci-local.sh) replicates the CI
command locally.

---

## 🔳 Gaps — missing features

### G1. No batch / pipeline tool
**Severity:** high (UX friction)

Per-module dev loop is `ghci_load → ghci_lint → ghci_format → ghci_check_module`:
4 round-trips per module. A session touching 10 modules = 40 tool
calls. That's a lot of context for the agent to manage (each call has
JSON parse, state reconciliation, next-action decision).

**Proposed.** `ghci_batch(actions: [...], fail_fast: true)` that takes
a list of tool invocations, runs them sequentially in one request,
and returns a list of results. 75% fewer round-trips on the common
dev loop.

### G2. No project-level gate
**Severity:** medium

`ghci_check_module(module_path)` aggregates gates per module. For a
25-module project, that's 25 calls to know if the whole package is
green, OR fall back to `cabal_test` (which doesn't run HLint / format
/ property regression).

**Proposed.** `ghci_check_project` that enumerates library +
test-suite modules from `.cabal`, runs `check_module` on each, and
returns a summary plus per-module breakdown.

### G3. `ghci_arbitrary` is one-per-type
**Severity:** low (convenience)

A module with 5 data types needs 5 invocations. Each query re-runs
`:info`, re-parses, rebuilds scope state.

**Proposed.** `ghci_arbitrary(module_path=...)` that scans the module
for data/newtype declarations, filters out ones that already have an
`Arbitrary` instance, and emits templates for the remainder in a
single response.

### G4. No `ghci_find_references`
**Severity:** high (refactor enabler)

`ghci_goto(name)` reports "defined at X:Y". But there's no complement
to answer "who uses this". For a safe cross-module rename, the agent
needs both. Without it, the Phase-8 `ghci_refactor(rename_local)`
is scoped to a line range the agent picks by hand — not AST-accurate.

**Proposed.** `ghci_find_references(name)` that lexes every source
file in the project and returns `[{file, line, column}]` for every
occurrence, honoring the same comment / string-literal exclusions as
the rename engine.

### G5. No structured `ghci_apply_edit` primitive
**Severity:** medium (architectural gap)

`ghci_refactor` returns previews as free-text diffs in `dry_run=true`.
There's no primitive that accepts a structured patch and applies it
atomically with compile-verify. Every refactor tool reimplements the
snapshot + write + loadModule + restore dance.

**Proposed.** `ghci_apply_edit(edits: [{file, line_start, line_end, replacement}], verify: true)`
as the building block all future editing tools compose on top of.

---

## 📉 Redundant — candidates for removal

### R1. `_guidance` appears in every successful response
**Severity:** low (noise)

After literally every `ghci_load` success, the response includes:

```
"_guidance": ["No Arbitrary instances in any module — run ghci_arbitrary for data types before QuickCheck"]
```

...even when the module has no data types, or when I already have
Arbitrary instances for every data type in the project. Context bloat
adds up over 50+ tool calls per session.

**Fix.** Emit `_guidance` only when the workflow state *changed* in a
way the hint is newly relevant. If the agent ignored the same hint 5
times, stop sending it. Track "hints shown" server-side.

### R2. `ghci_hls` exists but never works
**Severity:** low (dead weight in `tools/list`)

The tool shows up in `tools/list` but every invocation returns
"HLS unavailable". Occupies a tool slot the agent considers when
choosing what to call next.

**Fix.** Gate its visibility on `hls` being resolvable at server
start; if unavailable, omit from `tools/list` and surface as a
note in `ghci_workflow(status)` instead.

### R3. Auto-download of vendor tools in CI
**Severity:** medium (supply chain + CI fragility)

The server auto-downloads `hlint`, `fourmolu`, `ormolu`, `hls` to
`vendor-tools/` on first use. In CI, this:

1. Adds network as a required dependency of the build.
2. Bypasses the GitHub-managed action (`haskell-actions/hlint-setup`)
   which has its own supply-chain guarantees.
3. Re-downloads on every cache miss.

**Fix.** Local-only bundling is fine. In CI, detect `$CI=true` and
refuse auto-download — require the caller (workflow YAML) to have
provisioned the binary through an action.

---

## 💡 Game-changing additions

### A1. `ghci_batch` — explicit action list
Covered in G1 but repeated here because it's the single highest-ROI
addition. One request = N actions. The agent goes from 40 turns to 10
on a typical module session.

### A2. `ghci_pipeline(stage="module_complete")` — declarative gate composition
Higher-level than batch: takes a named stage, the server knows the
sequence. `stage="module_complete"` expands to load+lint+format+
holes+regression with fail-fast by default.

### A3. `ghci_watch` — subscription for hot-reload
Open a long-lived subscription: "notify me when `src/Foo.hs` changes
on disk". Server does file-watch + auto-load + pushes diagnostics to
the agent. This is the LSP pattern applied to MCP — the only way to
support a real hot-reload dev loop.

### A4. `ghci_rename_symbol` backed by HLS
Phase-8's textual rename is scoped to a line range. HLS can do
cross-module AST-accurate rename. Wrapping `haskell-language-server`
under `ghci_rename_symbol(qualified_name, new_name)` would deliver
refactor-at-the-speed-of-LSP.

### A5. `ghci_explain_error(code="GHC-83865")` — curated diagnostics
GHC emits `[GHC-83865]` in errors. A tool that maps the code to a
curated explanation + link + common-fix list would save 5-10 min per
unfamiliar error. The knowledge base can grow incrementally — start
with the top-50 most-hit codes.

### A6. Explicit workspace routing in `.mcp.json`
Today `HASKELL_PROJECT_DIR` is a single path; `ghci_switch_project`
discovers via recursive scan, sometimes picking unrelated playground
projects. Better:

```json
{
  "workspaces": {
    "lib": "./mcp-server-haskell",
    "playground": "./playground/*"
  },
  "default": "lib"
}
```

Explicit routing eliminates the B2 silent-switch class of bugs by
making project selection declarative.

---

## 🏆 What works well — preserve

### W1. Persistent GHCi session
The biggest perf win. `:reload` instead of cold-start transforms the
dev loop from seconds-per-call to milliseconds. Every tool that warms
up the session (load, type, info, hole, quickcheck) benefits
downstream.

### W2. `ghci_load(diagnostics=true)` dual-pass
Strict pass surfaces real compile errors; deferred pass surfaces
holes + inferred types. The union in a single response is the most
valuable single tool call in the server — one invocation, agent sees
everything blocking its next step.

### W3. Bundled hlint with fixed version
Local-only. Matches CI's `latest` tag (as of today both resolve to
3.10). Zero-config from the agent's perspective, and tighter than
relying on `PATH`.

### W4. `ghci_check_module` as a gate aggregator
Exactly the right abstraction for "is this module done?". Gates
(compile / warnings / holes / properties) have clear independent
semantics but converge on a single pass/fail. Phase-6's Haskell port
copied the semantics verbatim because the design is already right.

### W5. Auto-persist of passing QuickCheck properties
`ghci_quickcheck` saves to the property store only on `QcPassed`.
Failed runs don't pollute the regression suite. Phase 6 of the
Haskell port copied this and it's been flawless.

### W6. `_guidance` when relevant
When the hint tracks a real state transition ("you just added a data
type, consider running `ghci_arbitrary`"), it's gold. The problem is
noise (R1), not the mechanism.

### W7. Wire format: JSON string inside a text content block
MCP-idiomatic. Ugly but universally parseable; no client can
misinterpret an `application/json` content type. Retained verbatim
in the port.

---

## Issue tracker cross-reference

Every actionable item above is mirrored into a GitHub issue. Filter
by label: <https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues?q=label%3Aretrospective>

| Item | Issue | Label | Priority |
|------|-------|-------|----------|
| B1. `ghci_deps` wipes build-depends | [#6](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/6) | bug | high |
| B2. Session silent re-routing | [#7](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/7) | bug | medium |
| B3. `ghci_lint` per-file vs CI recursive | [#8](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/8) | bug | medium |
| G1 / A1. `ghci_batch` | [#9](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/9) | enhancement | high |
| G2. `ghci_check_project` | [#10](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/10) | enhancement | medium |
| G3. `ghci_arbitrary(module_path)` | [#11](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/11) | enhancement | low |
| G4. `ghci_find_references` | [#12](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/12) | enhancement | high |
| G5. `ghci_apply_edit` primitive | [#13](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/13) | enhancement | medium |
| R1. `_guidance` noise | [#14](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/14) | cleanup | low |
| R2. Hide `ghci_hls` when absent | [#15](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/15) | cleanup | low |
| R3. Refuse auto-download in CI | [#16](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/16) | cleanup | medium |
| A2. `ghci_pipeline(stage)` | [#17](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/17) | enhancement | medium |
| A3. `ghci_watch` subscription | [#18](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/18) | enhancement | medium |
| A4. `ghci_rename_symbol` via HLS | [#19](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/19) | enhancement | medium |
| A5. `ghci_explain_error` | [#20](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/20) | enhancement | low |
| A6. Workspace routing in `.mcp.json` | [#21](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/21) | enhancement | medium |

Note: A1 (`ghci_batch` as game-changing addition) folds into G1 — it's
the same tool in two frames. Tracked as a single issue.

High-priority starting set: #6 (destructive bug), #9 (batch — biggest
UX win), #12 (find_references, enabling safer refactor).

---

## Meta: when to update this document

Add new entries when any of the following happens:

- A tool call produces surprising behaviour that costs recovery time.
- A pattern of friction appears across multiple sessions (like the
  `== Nothing` lint slip — one incident is random, three is a gap).
- A concrete idea emerges that would change the agent's leverage by
  an order of magnitude, not 10%.

Remove entries once the underlying issue is closed or the feature
ships. Keep the history via git — don't preserve stale guidance.
