# Comprehensive dogfood session 2026-05-01

## Executive summary

**The honest take.** This MCP has one tool (`ghc_hole`) that is
flat-out gold-standard, ~20 that work cleanly the first time, and
**eight bugs** spread across the surface that range from polish to
data-loss-on-first-call. With 4–6 fixes (the `priority:high`
issues filed below), this MCP becomes legitimately recommendable to
the Haskell community. **Today, it isn't yet** — but the gap is
smaller than I expected.

### Quick verdict by tier

| Tier | Tools | Verdict |
|------|-------|---------|
| ⭐ Gold | `ghc_hole` | The single best feature. Returns expected type + valid hole fits + relevant bindings, structured. No language-agnostic tool can replicate this. |
| ✅ Solid | `ghc_workflow(next)`, `ghc_toolchain`, `ghc_project(switch/validate)`, `ghc_modules`, `ghc_load`, `ghc_type`, `ghc_info`, `ghc_eval` (pure), `ghc_complete`, `ghc_browse`, `ghc_goto`, `ghc_imports`, `ghc_arbitrary`, `ghc_format`, `ghc_apply_exports`, `ghc_fix_warning`, `ghc_refactor(rename_local)`, `ghc_lint`, `ghc_check_module`, `ghc_check_project`, `ghc_quickcheck`, `ghc_witness`, `ghc_property_store(list/run/audit)`, `ghc_batch`, `ghc_gate`, `hoogle_search` | Worked first-try. ~20 tools. |
| 🟡 Has rough edges | `ghc_workflow(status/help)`, `ghc_deps(list/explain)`, `ghc_doc`, `ghc_suggest`, `ghc_explain_error`, `ghc_perf`, `ghc_eval` (IO), `ghc_refactor(extract_binding)` | Works for the happy path; details misfire. |
| 🐛 Broken or destructive | `ghc_project(create)`, `ghc_add_import`, `ghc_property_store(export)`, `ghc_lab`, `ghc_refactor(move_symbol)`, `ghc_coverage` (chain-broken via export bug) | Real bugs, the priority queue. |

### Issues opened from this session

| Finding | Issue | Severity |
|---------|-------|----------|
| F-05 — `ghc_project(create)` data-loss | [#102](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/102) | 🚨 priority:high |
| F-13 — `ghc_doc` misses local Haddocks | [#103](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/103) | priority:medium |
| F-28/29/30 — `property_store(export)` + `lab` chain | [#104](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/104) | 🚨 priority:high |
| F-18 — `ghc_add_import` no_match for basic names | [#105](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/105) | priority:medium |
| F-01..04, F-06..12, F-14..17, F-19..27, F-31..34 polish bundle | [#106](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/106) (filed below) | priority:low / mixed |

### Verdict

**Is this MCP useful for the Haskell community today?** Conditionally
yes. A Haskell-fluent user willing to read the failure modes will
get immediate value (`ghc_hole`, `ghc_check_module`'s gate output,
the snapshot-and-compile-verify on `rename_local`). A new Haskell
user running `ghc_project(create)` on day one has a 30-second path
to losing their work — and that's the user this MCP is *most*
intended to help.

**Is it premature?** No. The architecture is sound, the
property-first idea is genuine innovation, and most of the bugs
are local and small (~30 LOC fixes). What's missing is the polish
pass that turns an internal tool into a public-release tool: error
shape consistency, schema-vs-code parity, the destructive-by-default
guards on `ghc_project(create)`, and the export/lab chain
correctness.

**Realistic ship target.** Close [#102](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/102),
[#104](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/104),
[#105](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/105),
plus the path-mangling sub-finding from #106 (move_symbol), and this
MCP can go on the Haskell community's recommended-tool list. Add
[#103](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/103)
and the polish bundle for a full release.

---

## Goal

Exercise **every one of the 35 tools** in the haskell-flows MCP against
fresh, representative scenarios. Capture qualitative remarks ("this is
good / surprised me / clunky / broken"). Open issues for each finding
worth tracking. End with a verdict: **is this MCP ready for the wider
Haskell community, or premature?**

## Method

- Scratch project at `/tmp/dogfood-scratch/` (don't pollute MCP source).
- Tool-by-tool, grouped by category. Each tool gets at least one
  representative invocation; obvious tools get more.
- Findings indexed `F-##` with severity emoji per the established
  legend (`🐛 bug`, `🔳 missing`, `🟡 unclear`, `✅ works as designed`).
- Issues opened on GitHub via `gh issue create` as findings accumulate
  — not batched at the end. Each issue is self-contained and
  reproducible without this notebook.

## Friction log — legend

| Code | Meaning                                                        |
|------|----------------------------------------------------------------|
| 🐛   | Real bug — tool malfunctions or gives wrong output             |
| 🔳   | Missing feature — tool is absent when the flow needs one       |
| 🟡   | Unclear — tool works but output is confusing / not actionable  |
| ✅   | Works as designed (preserved as anti-regression marker)        |

---

## Findings

### Project lifecycle — `ghc_project`, `ghc_modules`, `ghc_deps`

#### F-05 🐛🚨 — `ghc_project(create)` silently ignores `path`, writes to active `projectDir`. Data loss with `overwrite=true`.

**The single most serious finding of this session.** Reproducer:

```jsonc
// MCP currently anchored at:
//   projectDir = ".../mcp-server-haskell"
ghc_project(
  action  = "create",
  path    = "/tmp/dogfood-scratch",   // <- silently ignored
  name    = "dogfood-scratch",
  write   = true,
  overwrite = true)
// returns success: true
//   files_written: ["dogfood-scratch.cabal", "cabal.project",
//                   "src/DogfoodScratch.hs", "test/Spec.hs"]
```

The files were written **inside `mcp-server-haskell/`**, NOT inside
`/tmp/dogfood-scratch/`. With `overwrite=true`, the scaffold's
`test/Spec.hs` (13 lines, hello-world) **replaced the real
`test/Spec.hs` (10967 lines, the project's entire test suite)**.
`cabal.project` was also overwritten, losing the `split-sections:
True` config from #101.

`git diff` headers: `@@ -1,10967 +1,13 @@` for `test/Spec.hs`.
Restoration possible only because of `git`. A Haskell developer using
this MCP outside a `git`-tracked dir would have permanently lost work.

**Why it's so bad:** `path` is documented as
*"Absolute path to the target cabal project directory"* — there is no
hint that `projectDir` is the actual target. Combined with
`overwrite=true`, the failure mode is silent and destructive.

**Adjacent bug (also F-05):** even with `write=false`, the
collision-detection error reports
`"Target files already exist: cabal.project, test/Spec.hs"` —
those files exist in `projectDir`, not in the requested `path`. The
error message neither names the directory it is checking nor
distinguishes "exists at requested path" from "exists at projectDir".

**Fix proposal.**
1. Make `path` actually route the write. If `path` is absent, default
   to `projectDir` (today's behavior) but say so in the response.
2. Reject `overwrite=true` unless the target dir matches an explicit
   `confirm=` token, or unless the dir is empty / not under `git`
   tracking with uncommitted changes. This is the data-loss
   prevention pattern HLS uses for refactors.
3. Error messages must report the directory inspected.

**Subjective remark.** Bug severity aside — this is a *trust*
event. An MCP that silently overwrites a 10k-line test suite while
returning `success: true` is one a Haskell user will not give a
second chance. Of the 35 tools, this is the one they would meet
*first* on a new project. **The first impression is broken.**

#### F-06 🟡 — `ghc_project(bootstrap)` targets `<projectDir>/.claude/rules/`, but most repos keep `.claude/` at the git root

Preview run echoed:

```
target: "/Users/dlattenero/.../mcp-server-haskell/.claude/rules/haskell-flows-mcp.md"
```

But the existing rules in this very repo live one level up:
`/Users/dlattenero/.../haskell-rules-and-mcp/.claude/rules/use-haskell-flows-mcp.md`.
A user who follows the bootstrap output's `target` would create a
file Claude Code does not pick up (rule discovery walks up from the
git root, not the cabal-project root). **Fix.** Walk up from
`projectDir` until hitting a `.git/` ancestor, then drop
`.claude/rules/...` there. Alternatively, accept a
`scope = "git-root" | "cabal-root"` parameter.

#### F-07 🟡 — Scaffolded stubs reference a retired tool name

`src/DogfoodScratch.hs` opens with
`-- | Stub module scaffolded by ghc_create_project.`, but
`ghc_create_project` was retired in #94 Phase C in favor of
`ghc_project(action=create)`. New users grepping the codebase for
the tool that produced their stub will find nothing. **Fix.**
Update the scaffold template string; trivial, < 5 LOC.

#### F-08 🟡 — `ghc_deps(action="list")` only returns first stanza by default

Without `stanza`, `list` returns 1 entry (`["base"]`) on a project
that has 3 deps total (library: base; test-suite: base, self,
QuickCheck). The default is "first stanza", which is documented but
counter-intuitive — when an agent asks "what does this project depend
on", it usually means the whole picture. **Fix.** Either default to
`stanza="all"` and return
`{ stanzas: { library: [...], "test-suite": [...] } }`, or surface a
`hint` line indicating the additional stanzas have unlisted deps.

#### ✅ W-02 — `ghc_project(validate)` is on the gold standard

Calls `cabal check`, returns structured `issues` with severity, and
sets `status: "partial"` (not `failed`) because warnings shouldn't
block a `validate` call. Caught the real `[no-category]` warning.
The structure (`issues[]` with `kind/severity/message` + a top-level
`summary/errors/warnings` count) is exactly what an agent needs to
decide whether to act.

#### ✅ W-03 — `ghc_project(switch)` is well-designed

Returns `{ current, previous, scaffolded, message }` — clean
before/after state plus a one-line UX nudge ("Next tool call boots
a fresh GhcSession"). The reboot semantics are explicit. Compare
favorably to most language-server "switch project" UIs which leave
the session state ambiguous.

#### ✅ W-04 — Scaffold quality is *very high* (subjectively)

The generated `.cabal` uses `GHC2024`, a `common shared` stanza for
DRY, and a sensible `-W` set (`-Wall -Wcompat -Widentities
-Wincomplete-record-updates -Wincomplete-uni-patterns -Wpartial-fields
-Wredundant-constraints -Wunused-packages`). `OverloadedStrings`,
`DerivingStrategies`, `LambdaCase` are pre-enabled. **Subjective
remark.** This is *better than the cookiecutter most experienced
Haskell developers produce by hand.* It pre-imposes good habits
(strict warnings, modern language edition, DRY common stanza) on a
new project. ✅ W-04 is the brightest spot in this entire dogfood.

#### ✅ W-05/W-06/W-07 — `ghc_modules(add/remove)` is symmetric and honest

`add` registers in `exposed-modules` AND scaffolds an empty `.hs`
stub with a `-- | TODO:` comment in the right path. `remove` with
`delete_files=true` is the inverse. Both echo back what they did
(`cabal_added`/`cabal_removed`/`created_files`/`deleted_files`),
making the diff obvious. The cabal file's multi-line indentation is
preserved correctly across both ops.

#### ✅ W-08 — `ghc_deps(add)` writes correctly into cabal

`, QuickCheck >= 2.14` lands with the right comma indentation under
the right stanza. `hint` field correctly explains
"the next ghc_load reloads GHCi with the new package graph — no
explicit session restart tool is needed". Excellent UX.

---

#### F-09 🐛 — `ghc_deps(explain)` parser glues comma-separated rejections

Synthetic input had three rejections:
`rejecting: QuickCheck-2.14.3 (...)` and
`rejecting: QuickCheck-2.14.2, QuickCheck-2.14.1 (...)`.

Tool returned `involved_packages: ["QuickCheck", "QuickCheck-2.14.2, QuickCheck"]`
and `rejection_count: 2`. The second entry has a literal comma plus
the trailing word — the parser kept the multi-version `rejecting`
line as one entry rather than splitting on commas. **Fix.** Split
on `,` after stripping the leading version prefix; bump rejection
count accordingly. The structured `root_cause` extraction (`package`
+ `reason`) was correct.

### Read/inspect tools — `ghc_load`, `ghc_type`, `ghc_info`, `ghc_eval`, `ghc_complete`, `ghc_browse`, `ghc_imports`, `ghc_doc`, `ghc_goto`

**Headline.** 8 of 10 happy-path calls clean on first try. The
class as a whole is the **most reliable group** I've seen in this
session — these tools feel finished.

#### ✅ W-09 — `ghc_load` summary line is perfect

`{ "summary": "Compiled OK. No issues.", "errors": [], "warnings": [] }`
plus a `nextStep` pointing at `ghc_suggest`. One JSON object, four
keys, and an agent knows everything it needs.

#### ✅ W-10 — `ghc_info Functor` is a *better* `:info` than GHCi's

Returns `{class_methods, definition, instances, kind, name}` —
splits the wall-of-text GHCi prints into structured fields. Agents
can act on `instances[]` directly without parsing.

#### ✅ W-11 — `ghc_type` handles real expressions, not just names

`map (+1) . filter even` typechecks to
`forall {b}. Integral b => [b] -> [b]`. Section/composition/eta-reduced
forms all work. Some MCPs only accept bare identifiers; this one
accepts the full GHCi `:t` surface.

#### ✅ W-12 — `ghc_eval` works for pure + plain show paths

`greet "world"` → `"Hello, world!"` and
`map (+1) [1..5]` → `[2,3,4,5,6]`. Output capped at 64 KiB (per the
description). Status: ok plus `truncated: false` flag — clean.

#### F-12 🐛 — `ghc_eval` returns empty string for `IO ()` actions

`ghc_eval("putStrLn \"hello\"")` →
`{ output: "", truncated: false, status: ok }`. The expression
typechecks and runs, but `putStrLn`'s `stdout` is never captured.
Per the tool description: *"Tries `show`-wrapped compileExpr first
(for pure expressions), falls back to an IO String interpretation
(for actions that already return a string, or expressions in the IO
monad)"* — but `IO ()` falls through both. The user gets `ok` with
no output, which falsely suggests "the action ran and printed
nothing". **Fix.** Either (a) capture stdout via `hCapture` for the
`IO ()` path and return it as `output`, or (b) explicitly report
`{kind: "io_unit_no_output", message: "...action returned (); to
see stdout, redirect via Capture or print the result"}`.

#### ✅ W-13 — `ghc_complete` is concise and correct

`prefix: "fold"` returned 9 candidates (foldM, foldM_, foldMap,
foldl, foldl', foldl1, foldl1', foldr, foldr1) — every base
foldlike. Default limit 25, hard-capped 200. Single
round-trip, no fluff.

#### ✅ W-14 — `ghc_browse` returns annotated entries

`["greet :: String -> String"]` rather than just `["greet"]`. Type
information arrives without a follow-up call. ✅

#### F-10 🟡 — `ghc_imports` reveals unexpected MCP-injected imports

Fresh session. Source files import zero modules. `ghc_imports`
reports 7 imports active: `Prelude, DogfoodScratch,
DogfoodScratch.Util, System.IO, Data.List, Control.Monad,
Control.Concurrent`. The latter four are **not in any source file**
— they appear to be MCP convenience preloads of the GHCi session.

This is undocumented, and the consequence is that `ghc_eval` and
`ghc_type` see a *different* identifier namespace than `cabal build`
will. An agent that tests with `ghc_eval`, sees green, then runs
`ghc_check_project` could discover phantom failures. **Fix.** Either
document the preload set in the tool description (and in
`ghc_imports`'s response, e.g.
`{ source_imports: [...], session_preloads: [...] }`), or remove the
preloads — the explicit-source set is more honest.

#### F-13 🐛 — `ghc_doc` misses local Haddock annotations

Source file has:
```haskell
-- | Example function — replace with your own.
greet :: String -> String
greet who = "Hello, " <> who <> "!"
```

`ghc_doc("greet")` returns `{ hasDoc: false, reason: "No Haddock
available (package may have been built without -haddock)" }`. But the
Haddock is *right there* in the loaded source. GHCi's `:doc` only
surfaces docs from `.hi`-with-haddock files, but a Haskell IDE-class
tool should fall back to scanning the source AST when GHCi has
nothing. **Fix.** When GHCi returns no doc, locate the binding via
`ghc_goto`, then read the preceding `-- |` block from disk. ~30 LOC.

#### ✅ W-15 — `ghc_doc` for documented external names is clean

`ghc_doc("map")` returned the full Haddock string with examples
(`map (+1) [1, 2, 3]` → `[2,3,4]`) and complexity annotation. One
small wart: the LaTeX `\\(\\mathcal{O}(n)\\)` is returned raw rather
than rendered — minor, agents can strip it.

#### ✅ W-16 — `ghc_goto` falls back gracefully for external names

`greet` → `{file, line, column, kind: "file"}`. `fmap` →
`{module: "GHC.Internal.Base", kind: "module"}`. The `kind` field
makes the fallback explicit; the description already flags HLS as
the future cross-module precision tool. Honest about its limits.

### Write / refactor — `ghc_arbitrary`, `ghc_format`, `ghc_apply_exports`, `ghc_fix_warning`, `ghc_add_import`, `ghc_refactor`, `ghc_hole`

#### ⭐ W-19 — `ghc_hole` is the single best tool in this MCP

Source under test:

```haskell
mystery :: Int -> String
mystery n = _think
```

Tool returned:

```jsonc
{ hole_count: 1,
  holes: [{
    hole: "_think",
    expectedType: "String",
    location: { line: 28, column: 13, file: "..." },
    relevantBindings: [{ name: "n", type: "Int" }],
    validFits: [
      { name: "[]",     type: "forall a. [a]",
        source: "with [] @Char (bound at <wired into compiler>)" },
      { name: "mempty", type: "forall a. Monoid a => a",
        source: "with mempty @String (imported from 'Prelude' ...)" }
    ]
  }]}
```

This is exactly what an LLM agent can act on. Expected type, the
valid fits with their type-class instantiation, the relevant
in-scope bindings — every piece needed to fill the hole rationally
is right there. No language-agnostic tool replicates this; it
requires the GHC API. This single tool is worth the install.

#### F-15 🟡 — `ghc_suggest` is asymmetric on `[a] -> [a]`

`myReverse :: forall a. [a] -> [a]` → 0 suggestions.
`sortDedup  :: forall a. Ord a => [a] -> [a]` → 4 suggestions.

The Ord constraint is the gating signal. But `myReverse` is *the*
canonical involutive function and a textbook QuickCheck target. The
"length-preserving / non-extending" law (`length (f xs) <= length
xs`) is independent of `Ord` — it should fire for both. **Fix.**
Decouple the list laws from the `Ord` constraint; gate Idempotent
and Self-inverse on a name heuristic (`reverse` / `sort` /
`canonicalize`) instead of typeclass.

#### F-16 🐛 — `ghc_suggest` emits Involutive + Self-inverse as duplicate laws

For `sortDedup :: Ord a => [a] -> [a]`:
- Suggestion 2: `Involutive` → `\\x -> sortDedup (sortDedup x) == x`
- Suggestion 4: `Self-inverse on lists` → `\\(xs :: [Int]) -> sortDedup (sortDedup xs) == xs`

These are the *same property* (modulo type annotation). Both
medium-confidence. Both **false** for `sortDedup` (which idempotents
to the sorted-deduped list, not the original). An agent that
`ghc_quickcheck`s all 4 would learn (a) `sortDedup` is buggy
(it's not), (b) the suggest engine is. **Fix.** Dedupe at the rule
engine: never emit two laws whose property AST is α-equivalent.

#### F-17 🟡 — `ghc_fix_warning` patch is `null` when `dropLine=true`

For GHC-66111 (redundant import): `fixable: true, dropLine: true,
hint: "Drop the unused import line.", patch: null`. The mix of
`dropLine: true` (action signal) and `patch: null` is ambiguous —
is the agent supposed to just delete the line? Empty string instead
of null? **Fix.** Either always populate `patch` (use `""` for
deletes, the rewritten line for substitutions, the inserted block
for additions), or remove the `patch` field on delete responses.
Today's "null vs empty vs string" trichotomy is fragile.

#### ✅ W-20 — `ghc_arbitrary` template is paste-ready

`ghc_arbitrary("Status")` returned a clean instance with
`oneof [pure Active, pure Idle, pure Stopped]`. Constructors
echoed back as structured `[{name, args, arity}]`. Hint says where
to paste it. ✅

#### ✅ W-21 — `ghc_format` is conservative + correct

Preview-only by default (`write=false`). Reorders imports
alphabetically, adds parens to `Ord a =>` for `(Ord a) =>`,
4-space indent for let-blocks. fourmolu defaults — most teams will
accept these.

#### ✅ W-22 — `ghc_fix_warning` for unused-binding (GHC-40910) is precise

`patch: "  let _dead = 42 :: Int"` plus `apply: false` to preview.
Underscore-prefix is the canonical fix for `defined-but-not-used`.

#### ✅ W-23 — `ghc_apply_exports` is idempotent + clean

`exports = ["myReverse", "sortDedup", "Status (..)"]` rewrote the
module header in place. Preserves the data-constructor `(..)`
syntax. ✅

#### F-18 🐛🚨 — `ghc_add_import` returns 0 for names `hoogle_search` returns 10 for

See [#105](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/105). Priority:medium.

#### ✅ W-24 — `ghc_refactor(rename_local)` is *atomic*

Renamed `dead` → `_dead` at line 23. Response includes `compile:
"ok"`, `new_errors: []`, `pre_existing_errors: []`,
`occurrences: 1`, `touched_lines: [23]`. The snapshot-and-compile-
verify is real — confirmed independently with `git diff`.

#### F-21 🐛 — `ghc_refactor(extract_binding)` accepts non-expression ranges

Passed `scope_line_start=24, scope_line_end=24` where line 24 was
`   in x + 1` (the `in`-clause of a let). Tool generated:

```haskell
helper x =
  let _dead = 42 :: Int
   computeOne
computeOne =
  in x + 1
```

`in x + 1` is not a valid expression on its own; `in` is the let-
clause keyword. With `dry_run=true` the tool returned `status: ok`
(no compile-verify on dry runs) — so the broken plan is presented
as ready-to-apply. **Fix.** AST-validate the selected range *is*
a complete expression before generating the rewrite. Or: always
compile-verify the rewrite, even on dry-run, and refuse to return
`ok` without it.

#### ✅ W-25 — `ghc_refactor(extract_binding)` error message on top-level scope is gold-standard

When I selected `scope_line_start=18, scope_line_end=18` (top-level
declaration `sortDedup = nub . sort`), the tool returned:

> *"extract_binding requires an expression range, not a top-level
> declaration. The selected lines 18-18 start at column 0 (e.g.
> 'sortDedup = nub . sort'), which is a whole equation, type
> signature, import, or other top-level form — lifting it would
> produce invalid Haskell. Narrow scope to the indented body
> expression you actually want to lift..."*

Explains *why* it's invalid AND *what to try next*. Best error
message I've seen in the entire MCP. Preserve as anti-regression
marker.

#### F-23 🟡 — `ghc_load(diagnostics=true)` reports holes as compile errors

Description says it "runs a deferred pass (-fdefer-type-errors
-fdefer-typed-holes) and surfaces typed holes...". A typed hole
under deferred pass is a *warning*, not an error — the binary
compiles, you can iterate. But the response had
`status: "failed"`, `errors: [...]` with `severity: "error"`.

This puts the agent in a state where `ghc_load`'s `status` says
the module is broken even though it conceptually compiled. Pair
with `ghc_hole` (which works correctly) to see the asymmetry: same
module, same hole, hole-tool says "here's the type", load-tool
says "compile error". **Fix.** Translate deferred-error categories
to `severity: "warning"` in the JSON response. Reserve `error`
for non-deferred failures.

#### F-34 🐛 — `ghc_refactor(move_symbol)` schema/code mismatch on `from`/`to`

Schema description: *"Source module path (relative)."* with an
example reading like a path. Real behavior: only accepts
**module names** (dot-separated). Reproducer:

| `from`/`to`                              | Result                                                                          |
|------------------------------------------|---------------------------------------------------------------------------------|
| `"src/DogfoodScratch/Util.hs"`           | `module_path_does_not_exist: /tmp/.../src/src/DogfoodScratch/Util/hs.hs` (mangled) |
| `"DogfoodScratch/Util.hs"`               | `module_path_does_not_exist: /tmp/.../src/DogfoodScratch/Util/hs.hs` (extension mangled to `/hs.hs`) |
| `"DogfoodScratch.Util"`                  | ✅ success, returns plan                                                        |

Two bugs in the path mangling:
1. Doubled `src/` prefix when path already starts with `src/`.
2. The string-replace logic treats `.` as a path-separator; `Util.hs` becomes `Util/hs.hs`.

Plus the schema says "path" but the tool only accepts module-name
form. **Fix.** Update the schema description, OR (better) accept
both forms and normalize. The path-mangling shouldn't happen even
with bad input.

#### ✅ W-17 / W-18 — Both `ghc_info` and `ghc_browse` return *gold-standard* `remediation` fields on no_match

`ghc_info("NonExistentClass")` →
*"Name not currently in scope. If it's defined in a loaded module,
run ghc_load on that module first. For external/base names,
hoogle_search may surface candidates ghc_info cannot reach."*

`ghc_browse("Data.Nonexistent")` →
*"Browse only enumerates modules compiled by this project. For
modules in interactive scope (Prelude, base, external deps), look up
individual names with ghc_info or query with hoogle_search."*

Both tell the agent **exactly which other tool to try next**. This is
the difference between an LLM-friendly API and a human-friendly one.
Preserve as anti-regression markers — these are the gold standard
for failure modes in this MCP.

### Quality gates — `ghc_check_module`, `ghc_check_project`, `ghc_lint`, `ghc_lab`, `ghc_perf`, `ghc_witness`, `ghc_explain_error`, `ghc_gate`, `ghc_coverage`

#### ✅ W-26 — `ghc_check_module`'s `gates` object is best-of-breed

```jsonc
{ overall: false,
  gates: {
    compile:    { ok: true,  reason: "module compiles strictly" },
    holes:      { ok: true,  reason: "no deferred typed holes" },
    properties: { ok: true,  passed: 1, total: 1, reason: "1 stored properties pass", regressed: 0, ... },
    warnings:   { ok: false, reason: "3 warning(s) (blocking — pass warnings_block=false to keep iterating)" }
  }}
```

Four orthogonal gates, each with `ok` + `reason`. The `warnings.reason`
even tells the agent how to flip the blocking-vs-informational
behavior. `warnings_block=false` reuses the same tool with
inverted semantics — clean. ✅

#### ✅ W-27 — `ghc_lint` returns `from` / `to` patches

`{from: "{-# LANGUAGE OverloadedStrings #-}", to: "", hint: "Unused
LANGUAGE pragma"}`. Apply by find-and-replace; agent doesn't need
to parse the suggestion. ✅

#### F-26 + F-29 + F-30 🐛🚨 — `ghc_lab` lies; `property_store(export)` ships ambiguous types

See [#104](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/104). Priority:high.

Concretely: `ghc_lab` returned `properties_passed: 0` with all 9
candidate properties at `status: "unknown"`. But the property store
*grew by 4 entries* with `passed: 1`. Agents reading the response
would conclude "lab found nothing"; the next `property_store(list)`
reveals that the lab silently persisted unverified candidates.

Then `ghc_property_store(export)` writes those literal
expressions — including the un-annotated forms `\\x -> ...` that
the lab/suggest engines emit — into `test/Spec.hs`. `cabal test`
chokes with `Ambiguous type variable` errors.

Combined effect: the property-first workflow is broken end-to-end
through `cabal test` regression replay.

#### F-27 🟡 — `ghc_perf` returns full `samples` array

For 50 runs, the response contains `samples: [15038000, 2638000,
...]` — 50 measurements inline. For 1000 runs this would be 10× as
much. **Fix.** Default to a histogram or summary stats; gate the
raw `samples` behind `verbose=true`.

#### F-31 + F-32 + F-33 🐛 — `ghc_perf(compare_baseline)` measures error-handling overhead after session loss

After `ghc_modules(add)` the GHCi session lost the loaded module.
`ghc_perf(compare_baseline=true)` ran the expression 20 times
against an empty session — every run failed with GHC-58427
("attempting to use module which is not loaded") — but the
*timings of the failure path* were still measured and compared to
the live baseline. Result: `regression_pct: 23586%`, status
`refused`.

Three sub-findings:
- F-31 🐛 The MCP should detect and abort the perf run when every
  measurement is an error, not silently treat error overhead as
  a perf datum.
- F-32 🟡 The error's `cause` field is a *stringified JSON blob*
  (literal escape-sequence-laden string), not a structured object.
- F-33 🟡 The status `refused` is unique to perf — `failed`,
  `partial`, `no_match`, `refused`, `ok` are now five different
  outcomes across the surface, with no consistent semantic axis.
  An agent reading `refused` has to know it means "valid call,
  bad measurement, won't honor".

#### F-24 + F-25 🟡 — `ghc_explain_error` returns whole-file context

Two redundant fields:
- `enclosing_range: { start: 1, end: 28 }` — the *whole* 28-line
  file, not the function enclosing the error (lines 27-28).
- `enclosing_slice` and `module_source` are byte-identical strings.

**Fix.** Compute the actual enclosing function via SrcSpan, return
just those lines. Drop one of the duplicate source fields.

#### ✅ W-28 — `ghc_witness` is genuinely useful

Distribution by size (10.6% / 14.1% / 75.3%) and by constructor
(73.8% Just / 26.2% Nothing for `Maybe Int`). Plus `passed: 1000`
to confirm the underlying property held. This is the kind of
property-test instrumentation Haskell devs usually build by hand.

#### ✅ W-29 — `ghc_batch` is honest

Ran 3 actions: `ghc_type, ghc_type, ghc_complete`. Returned
`{ ok: 3, failed: 0, total: 3, results: [...]}` with each result
preserving the original tool's response shape. `fail_fast: true`
default. ✅

#### ✅ W-30 — `ghc_gate` skip flags work as documented

Ran with `skip_cabal_build=true, skip_cabal_test=true` (because
the test/Spec.hs was broken from the export bug). Result:
regression-only gate, 5/5 properties pass, 16s wall time, "Safe to
push" summary. ✅

### Inventory tools — `ghc_workflow`, `ghc_toolchain`

#### F-01 🐛 — `ghc_workflow` reports two different `phase` values seconds apart

Within the same restarted MCP, no intervening state change:

- `ghc_workflow(action="status")` → `phase: "PhasePreScaffold"`
- `ghc_workflow(action="help")`   → `phase: "PhaseDeveloping"`,
  `phaseHint: "Phase: developing. Modules load clean. ghc_suggest…"`

The help variant goes further: its `phaseHint` text confidently
narrates a state ("Modules load clean") that does not exist (no
modules loaded, GHCi not alive). This is the same class of bug as
F-03 from the 2026-04-19 session — a view derives state from the
wrong source. **Remark.** This is exactly the kind of inconsistency
that erodes agent trust: if `status` and `help` disagree on the
current phase, an agent has to pick one to trust, and there is no
principled reason to pick one over the other.

#### F-02 🟡 — `help` advice is too generic to act on without prior knowledge

`help.steps[0]` says *"Call ghc_load with your entry module"* but
gives no hint of which module that is. For a brand-new user landing
on a 40-module repo, "your entry module" is the question, not the
answer. **Fix.** Either parse `cabal-version: ...` for `main-is:` or
exposed-modules, or fall back to `Main`/`Lib` heuristics, then say
*"Try ghc_load(module_path=\"src/Lib.hs\")"*.

#### F-03 🟡 — `nextStep` is static, not session-aware

`ghc_toolchain(action="warmup")` returns
`nextStep: { tool: "ghc_workflow", action: "help" }` — but I just
called `ghc_workflow(action="help")`. The pointer doesn't track
recent history; greedy/static. For a single agent making the
"obvious" sequence of calls, this manifests as nextStep loops.
**Remark.** Subjectively the nextStep idea is a *huge* UX win
(replaces the explicit `ghc_workflow(action="next")` round-trip).
But "what's likely useful next" must condition on what the agent
just did. A 100-LOC fix: keep the last N tool calls in the session
and prune nextStep candidates that match.

#### ✅ W-01 — `ghc_workflow(action="next")` returns clean, single-shot guidance

`{tool: "ghc_load", view: "next", why: "..."}`. No fluff, no
phaseHint, no contradictions with `status`. Preserve as
anti-regression marker — this is the gold standard for what tool
hints should look like.

#### F-04 🟡 — `ghc_toolchain(warmup)` omits gates from response

`warmup` returns only the 5 optional binaries; gates (cabal, ghc,
hlint) are absent from the JSON. Probable reason: gates were already
warm. But the user has no way to confirm that without an extra
`status` call. **Fix.** Either always include the full toolchain
list (matching `status` shape) or add an explicit
`gatesAlreadyWarm: true` field. Consistency between the two action
variants of the same tool is the bare minimum.

