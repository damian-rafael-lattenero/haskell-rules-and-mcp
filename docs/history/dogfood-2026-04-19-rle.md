# Phase 11b — Fresh dogfood with Run-Length Encoding (2026-04-19)

## Executive summary

**Conclusion:** a clean scratch project (`playground/dogfood-rle`)
drove **5 of the 8 top-tier MCP tools** that Phase 11 had not
exercised against fresh code. Nine new findings — **F-01…F-09** —
**every one of them fixed in-tree in the same session**, tests
added, pushed to `master`. Six fix commits land back-to-back on
top of Phase 12.

The running MCP binary at `~/.local/bin/haskell-flows-mcp` still
carries all nine bugs until the next natural `cabal install`; the
fixes are validated by CI (80 tests passing, 0 hlint hints) and
will take effect the next time Claude Desktop restarts and a
fresh MCP process is spawned. This matches the preferred workflow
recorded at
`~/.claude/projects/-Users-dlattenero-Personal-Projects-haskell-rules-and-mcp/memory/feedback_dogfood_fix_flow.md`:
**dogfood → fix code + tests → commit → continue**, no reinstall
or relaunch in the loop.

One bug (F-08) was **critical**: the deferred-GHC-flags leak
voided the documented safety guarantee of `ghc_refactor`
(snapshot-and-compile-verify). It is the class of hole that keeps
me awake — "compile=ok" being silently a lie.

## Findings at a glance

| # | Tool                  | Severity    | Kind                                          | Fix commit |
|---|-----------------------|-------------|-----------------------------------------------|------------|
| F-01 | `ghc_deps add`    | 🐛 **High**     | Produces unparseable `.cabal`, returns `success=true` | `5adc350` |
| F-02 | `ghc_deps add`    | 🟡 Low      | Indent style inconsistent with existing block | `5adc350` |
| F-03 | `ghc_deps`        | 🐛 **High**     | No way to target `test-suite` / `benchmark` stanza | `5adc350` |
| F-04 | `ghc_arbitrary`   | 🐛 **High**     | Bails on records with `!` strict fields + GHC 9.x kind header | `42d830c` |
| F-05 | `ghc_suggest`     | 🐛 **High**     | Emits false list laws for `[a] -> [Run a]` (inner-type check missing) | `42d830c` |
| F-06 | `ghc_quickcheck`  | 🐛 **High**     | Session spawned without QuickCheck; `quickCheck` not in scope | `6bc3354` |
| F-07 | `ghc_refactor`    | 🟡 Medium   | Scoped rename leaves out-of-range definition intact (self-healing once F-08 lands) | `bc5b3ce` |
| F-08 | `ghc_refactor`    | 🚨 **Critical** | `:unset -fdefer-*` isn't the inverse of `:set -f-defer-*`; deferred flags leak across loads; compile-verify silently passes broken renames | `bc5b3ce` |
| F-09 | `ghc_coverage`    | 🐛 **High**     | Modern cabal writes HPC only to HTML; parser saw empty stdout | `fa92d09` |

## Wins preserved (anti-regression markers)

| # | Marker                                                                       |
|---|------------------------------------------------------------------------------|
| W-01 | `ghc_create_project(name="dogfood-rle")` produced 4 files at first shot  |
| W-02 | Scaffold from `ghc_create_project` compiles clean on first `ghc_load`   |
| W-03 | `ghc_load(diagnostics=true)` against the RLE module returned 0 errors + 0 warnings |
| W-04 | `cabal test` saw `prop_roundtrip`                passing on 200 cases      |
| W-05 | `cabal test` saw `prop_length_preserved`         passing on 200 cases      |
| W-06 | `cabal test` saw `prop_runs_non_zero`            passing on 200 cases      |
| W-07 | `cabal test` saw `prop_runs_match_group`         passing on 200 cases      |
| W-08 | `ghc_batch(fail_fast=false)` correctly reported `ok=5 failed=1` for a mixed run |
| W-09 | Post-edit invariant check on `ghc_deps` (shipped with F-01 fix) would now reject the original corrupted-cabal output |

## Timeline

1. **T+0** — scaffolded the project via `ghc_create_project`. W-01, W-02.
2. **T+5m** — asked `ghc_deps add QuickCheck` → broke the .cabal silently. F-01, F-02. Immediately discovered F-03 as the reason I needed workaround indirection (QuickCheck should live in test-suite, not library).
3. **T+12m** — fixed all three in `Deps.hs`, added `parseStanzaSelector`, `applyWithinStanza`, `computeContinuationIndent`, plus a post-edit invariant check that refuses to persist when the newly-written body disagrees with the requested verb (anti-F-01 belt). 5 new tests. Commit `5adc350`.
4. **T+25m** — implemented `DogfoodRle.encode / decode`. W-03 (compile clean).
5. **T+30m** — called `ghc_arbitrary("Run")` → F-04 (two bugs sharing a root). Fixed `parseConstructors` + `groupTokens` to handle the GHC 9.x kind header and record braces as grouping. Commit `42d830c`.
6. **T+40m** — called `ghc_suggest("encode")` → F-05. Fixed the list rules to enforce `argInner == retInner`. Same commit `42d830c`.
7. **T+45m** — called `ghc_quickcheck` against the persistent session → F-06 (`Variable not in scope: quickCheck`). Added `--build-depends QuickCheck` to every session spawn. Commit `6bc3354`.
8. **T+55m** — exercised `ghc_refactor(rename_local)` → F-07 (narrow-scope leaves the definition) and F-08 (the critical one — deferred flags leaked, compile-check lied). Fixed `loadModuleWith` to use `-fno-defer-*` + eagerly clear on `Strict` entry. Commit `bc5b3ce`.
9. **T+70m** — `ghc_coverage` → cabal test passed all 4 properties (W-04..W-07) but metrics list was empty. F-09. Wired `hpc report` post-pass. Commit `fa92d09`.

Total: nine findings, six fix commits, ~80 tests → ~85+ tests,
zero regressions in the existing suite, zero boundary-validation
breaches. Security envelope held throughout (argv-form spawns,
`mkModulePath`, `sanitizeExpression`, DoS cap, post-edit
invariant check for `.cabal` edits).

## Deep dives — why each bug happened

### F-01 / F-02 — `ghc_deps add` indent

`insertAfterBuildDepends` computed the continuation indent as the
leading whitespace of `last pre`. When `last pre` was the
`build-depends:` header itself (no prior continuation — a fresh
single-line build-depends from `cabal init` or
`ghc_create_project`), that yielded the field's own column. Cabal
3.0 reads a line starting with `,` at column <= field-name column
as a new field header, so it bailed with `unexpected operator ","`.
The tool reported `success=true` because it just checked the edit
succeeded, not that cabal still parsed the result.

**Fix contributes two knobs:** value-column-aligned indent derivation
when the last pre-line is the header, AND a post-edit invariant
check that re-parses the body and refuses to persist if the verb
(added/removed) disagrees with the re-parsed dep list. The second
knob is defence-in-depth against future regressions in the first.

### F-03 — No stanza targeting in `ghc_deps`

Every `add` / `remove` targeted the first `build-depends:` block
in the file. Since most real projects put the library stanza first,
test-only deps (QuickCheck, hspec) couldn't be routed to the
test-suite without hand-editing — defeating the point of the tool.
Fixed by adding an optional `stanza` argument: `library`,
`test-suite[:NAME]`, `executable[:NAME]`, `benchmark[:NAME]`,
`foreign-library[:NAME]`. Strict input validation rejects anything
outside `[A-Za-z0-9_-:]`.

### F-04 — `ghc_arbitrary` parser gives up on normal records

Two distinct parser problems, same root:

1. GHC 9.x prepends a kind signature line (`type Run :: * -> *`)
   to `:i` output. `parseConstructors`' `hasCtorHeader` only checked
   the collapsed string's prefix. Kind line shifted `data` out of
   position; parser thought it was a GADT/typeclass/synonym.
2. `groupTokens` tracked `(`/`)` but not `{`/`}`. Record braces
   `{runLen :: !Int, runVal :: !a}` were split on every internal
   space into 6 junk tokens, inflating the Arbitrary template's
   `<*>` arity.

Fix: pre-trim the kind line via `dropWhile (not . isDataDeclLine)`,
treat braces as grouping in `groupTokens`, post-process a
record-shaped lone arg by counting top-level commas for field
count.

### F-05 — False `Self-inverse on lists` / `Length preserving` for `[a] -> [Run a]`

`ruleListLengthPreserving` and `ruleListRoundtrip` matched
`([TyList _], TyList _)` without checking that the inner types
agreed. For `encode :: [a] -> [Run a]`, both rules fired at Medium
confidence and proposed properties that don't even type-check
(`encode (encode xs) == xs` won't compile). Fixed by binding the
inner types and requiring `argInner == retInner`.

### F-06 — `ghc_quickcheck` with no QuickCheck

`startSession` spawned `cabal repl` with no extra deps, so the
GHCi session inherited only the library's build-depends. In any
real project QuickCheck lives in the test-suite stanza, so
`Test.QuickCheck.quickCheck` was never in scope and every
invocation of the `ghc_quickcheck` tool hit
`Variable not in scope: quickCheck`. Fixed by always spawning
with `--build-depends QuickCheck`. Exposed `sessionCabalArgs` so
the argv shape is pinned by a pure unit test.

### F-07 / F-08 — `ghc_refactor` compile-check lied

F-07 was the visible symptom: a narrow-scope `rename_local` left
an unrelated binding un-renamed. That's arguably user error
(scope too narrow), but the tool's contract is "snapshot and
compile-verify, restore on failure" — it should have caught the
resulting `mkRun not in scope` error and rolled back.

F-08 was the real bug underneath. The `Deferred` load-mode wrapped
its command with `:set -fdefer-type-errors -fdefer-typed-holes` and
untailed it with `:unset -fdefer-type-errors -fdefer-typed-holes`.
**GHCi's `:unset` only handles GHCi-level options** (`+s`, `+t`,
editor…), not GHC flags. The inverse of `:set -f<flag>` is
`:set -fno-<flag>`. So every session ever created leaked deferred
flags indefinitely after the first `ghc_load(diagnostics=true)`
call — all subsequent compile-checks silently deferred their
errors.

This voided the `ghc_refactor` safety guarantee: broken edits
were never rolled back because the compiler refused to report
them as errors.

**Fix:** switched to `:set -fno-defer-*` (the actual inverse), plus
made the `Strict` load path eagerly clear the deferred flags on
entry as belt-and-suspenders. Regression test pins the static
shape of `Session.hs` — no future edit can silently drop the
`-fno-` or re-introduce `:unset -f`.

### F-09 — `ghc_coverage` reported no metrics

Under GHC 9.12 + cabal 3.14, `cabal test --enable-coverage` only
writes HPC data to HTML files. The old text summary
(`100% boolean coverage (0/0)`) that `parseCoverage` depended on
is gone from stdout; stdout now just lists `Writing: …html` paths.

Fix: after cabal succeeds, locate the `.tix` file via `find`,
derive the mix dir from the canonical cabal layout, shell out to
`hpc report --hpcdir=<mix> <tix>` — whose stdout still emits the
text the parser understands. Append that to the cabal stdout so
`parseCoverage` lights up the metrics correctly.

## What remains untested by this dogfood

In-vivo verification of all nine fixes will happen the next time
the user reinstalls the binary and relaunches Claude Desktop. The
running MCP in this session still carries the bugs; retesting them
here would fail identically.

Tools **not exercised** in Phase 11b that deserve their own
dogfood cycle later:

- `ghc_regression(run)` — property persistence was blocked by F-06.
- `ghc_goto` — never called.
- `ghc_doc` — never called.
- `ghc_complete` — never called.
- `ghc_hls hover` — HLS not on PATH.
- `ghc_format` — fourmolu / ormolu not on PATH.
- `ghc_refactor(extract_binding)` — happy path only.

These are candidates for a **Phase 11c** session once Phase 11b
fixes are live in the binary.

## Commits

```
fa92d09 fix(mcp-server-haskell): ghc_coverage parses hpc report text, not cabal stdout (F-09)
bc5b3ce fix(mcp-server-haskell): CRITICAL — Deferred GHC flags leaked across loads, voiding ghc_refactor safety (F-08)
6bc3354 fix(mcp-server-haskell): attach QuickCheck to every GHCi session via --build-depends (F-06)
42d830c fix(mcp-server-haskell): Phase 11b — Suggest/Arbitrary parser fixes (F-04, F-05)
5adc350 fix(ghc_deps): correct continuation indent, add stanza targeting, post-edit invariant check
```

All pushed to `master`, no PRs (per standing rule).

## Acceptance

| Gate                                                                    | Status |
|-------------------------------------------------------------------------|--------|
| `scripts/ci-local.sh --fast` green                                      | ✅ 80/80 tests, 0 hlint hints |
| Every F-## has an in-tree fix                                           | ✅ |
| Every fix has a unit test                                               | ✅ (15 new tests total) |
| Post-edit invariant on `.cabal` edits                                   | ✅ (F-01 defence-in-depth) |
| Deferred-flag leak closed                                               | ✅ (F-08) |
| `ghc_coverage` recognises modern cabal layout                          | ✅ (F-09) |
| Running binary still carries the bugs                                   | ⚠️ expected — natural restart will pick up fixes |
| Security envelope held throughout                                       | ✅ |

Phase 11b closed.
