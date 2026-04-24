# Testing architecture

Guide for anyone extending or debugging the haskell-flows MCP test
suite. The suite has two layers; each has a distinct job and a
distinct debugging playbook.

## Layers

### 1. Unit tests — `mcp-server-haskell/test/Spec.hs`

Plain hspec + QuickCheck suite. ~3 kLOC. Covers the library code
directly: parsers, the property store, session framing helpers,
workflow-state tracking, each tool's pure bits.

Run it with:

```bash
cabal test haskell-flows-mcp-test
```

Typical cycle time: 5–10 s.

### 2. E2E scenarios — `mcp-server-haskell/test-e2e/`

In-process black-box tests that spin up a real `Server` against a
fresh tempdir per scenario. Each scenario drives the MCP via
`handleRequest` (the same entrypoint the stdio transport uses) and
asserts on the JSON responses.

Run it with:

```bash
cabal test haskell-flows-mcp-e2e
```

Typical cycle time: 200–220 s with all scenarios, ~130 s with the
slow-tagged ones skipped (see below).

## On parallel execution (deferred)

There's no `HASKELL_FLOWS_E2E_PARALLEL` knob today — an earlier
experiment tried one, but N≥2 is fundamentally flaky against the
current architecture (each scenario spawns `cabal repl`, and
cabal-install upstream doesn't serialise the cross-process state
it depends on). Proof by reference: no major Haskell project runs
concurrent `cabal repl` per-test (HLS uses the GHC API as a
library, ghcid uses hie-bios, etc.).

The honest fix is a `startSession` refactor to bypass `cabal repl`
and use the GHC API via hie-bios — same pattern as HLS. See
[`docs/TODO-parallel-e2e.md`](TODO-parallel-e2e.md) for the full
design doc, references, implementation plan, and acceptance
criteria. ~1–2 days of focused work; deferred until someone takes
the slot.

## Skipping slow scenarios in the dev inner loop

Some scenarios dominate wall-time:
- `FlowCoverage` (~25 s for a real `cabal test --enable-coverage`)
- `FlowCrossValidation` (~30 s across 3 mini projects)
- `FlowTimeoutEnforcement` (~35 s, the test itself is the 30 s
  inner-budget assertion)
- `FlowConcurrentClients`, `FlowDiskFull`,
  `FlowExprEvaluator`, `FlowExprEvaluatorDogfood`,
  `FlowPropertyStoreRace` — each spins up multiple GHCi sessions

All are tagged `isSlow = True` in `test-e2e/Main.hs`. To skip them
during development:

```bash
HASKELL_FLOWS_E2E_SKIP_SLOW=1 cabal test haskell-flows-mcp-e2e
```

**CI runs everything by default.** The env var is opt-in, never on
in the shipped workflow. Use it for the inner loop where you want
fast feedback on a local change; run the full suite before pushing.

## Discipline: one test-run per change, not three

When iterating, run **either**:
- `cabal test haskell-flows-mcp-e2e` (inner loop, fast feedback on
  the e2e), or
- `scripts/ci-local.sh --fast` (pre-push gate: build + library tests
  + e2e + hlint, all in one go)

Do **not** chain both — `ci-local.sh` already invokes `cabal test`.
Running them back-to-back burns ~4 min per iteration for no gain.

## Anatomy of a scenario

Every scenario exports `runFlow`:

```haskell
runFlow :: Client.McpClient -> FilePath -> IO [Check]
```

The runtime gives you a pre-built client and a fresh tempdir. You
fire tool calls via `Client.callTool`, accumulate `Check`s from
`E2E.Assert`, and return the list.

A typical shape:

```haskell
runFlow c projectDir = do
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("demo" :: Text) ])

  t0 <- stepHeader 1 "short description of the step"
  result <- Client.callTool c "ghci_some_tool" (object [...])
  c1 <- liveCheck $ checkPure
          "human-readable check name"
          (<predicate on result>)
          "<debugging context if it fails>"
  stepFooter 1 t0

  pure [c1]
```

## Writing honest oracles

This is the hardest part and where the suite has paid the most
dividends. Four failure modes to watch for:

### 1. Structural assertions that pass on garbage

```haskell
-- BAD: tool can return anything for 'type' and this passes
checkJsonFieldMatches "has type field" r "type" (\_ -> True)

-- GOOD: known expected value
checkJsonFieldMatches "type is Int -> Int" r "type"
  (containsText "Int -> Int")
```

### 2. Tautological oracles that match the code under test

```haskell
-- BAD: we called ghci_eval, we know it returned X, we check X
let expected = extractField "output" r
in check expected (== "some value we just extracted")

-- GOOD: independent oracle — mathematics, documented contract,
-- cross-validated with cabal/ghc
check (fieldText "output" r) (== Just "3")  -- 1 + 2 must be 3
```

### 3. Happy-path-only scenarios

If your scenario only sets up a clean project and asserts every
tool returns success, you've built a smoke test — useful but not
a regression oracle. Plant a deliberate defect (type error, hint,
concurrent write, bad dep) and assert the tool SURFACES it.

`FlowQualityGates` demonstrates this: a clean module (anchor) +
a module with `reverse . reverse` (must trigger HLint) + a module
with a type error (must be counted as `failed` by `check_project`).

### 4. Vacuous passes from mis-matched fields

When you write an oracle based on the tool's output field, always
verify against the raw response first. The cost of `ghci_deps list`
returning `build_depends` not `packages` was two test-level bugs
during development — the oracles always saw `[]` and reported false
positives.

## Concurrency and the architectural limits

The MCP is a shared-nothing server for everything EXCEPT two places
where filesystem state is shared:

- `.cabal` file edits via `ghci_deps(add/remove)`
- `.haskell-flows/properties.json` via `ghci_quickcheck`
  persistence

Both now use `withCabalLock` / `withGlobalStoreLock` patterns
(sidecar `.lock` file + in-process MVar) to serialise writers.
`FlowConcurrentClients` exercises the `.cabal` path.

One thing that **cannot** be tested with two concurrent clients on
one project dir: simultaneous GHCi sessions. `cabal repl` takes an
exclusive lock on `dist-newstyle/`; whichever client booted second
gets `SessionExhausted`. This is a cabal invariant, not an MCP bug.
`FlowPropertyStoreRace` documents this in its assertions: one client
wins, the loser returns `error_kind=session_exhausted`, the
winner's property is correctly persisted.

## Adding a scenario

1. Create `test-e2e/Scenarios/FlowX.hs` exporting `runFlow`.
2. Register it in `test-e2e/Main.hs` in two places:
   - `import qualified Scenarios.FlowX as FlowX`
   - append `( "Flow: X (one-line pitch)", <isSlow?>, FlowX.runFlow )`
     to `scenarios`.
3. Add the module name to the `other-modules:` list in
   `haskell-flows-mcp.cabal` (under the `haskell-flows-mcp-e2e`
   test-suite stanza).
4. Run `cabal build haskell-flows-mcp-e2e` once to catch compile
   errors cheaply.
5. Run `cabal test haskell-flows-mcp-e2e` to exercise.
6. When the scenario starts green on the full suite, run
   `scripts/ci-local.sh --fast` as the pre-push gate.

## Debugging a failing scenario

Each `liveCheck` prints a `FAIL` line with a detail string as soon
as the check runs. The detail string is your diagnostic — include
the relevant field values and the raw response prefix. Example:

```haskell
cBoth <- liveCheck $ checkPure
  "both concurrent calls returned success=true"
  bothSucceeded
  ("A=" <> T.pack (show aOk) <> ", B=" <> T.pack (show bOk)
   <> ". Raw A: " <> truncRender rA)
```

That output pattern (`A=Just True, B=Just False. Raw A: …`) is
enough to tell you which client won without replaying the scenario.

For the full stream, the test framework writes every scenario's
log to:

```
mcp-server-haskell/dist-newstyle/build/.../t/haskell-flows-mcp-e2e/test/haskell-flows-mcp-0.1.0.0-haskell-flows-mcp-e2e.log
```

Grep for the `═══ Flow: X` section header to isolate one scenario.

## Bugs this suite has caught (as of this writing)

- **BUG-A** — `runTool` emitted raw plain-text on exceptions
  instead of the standard `{success:false, error, error_kind}`
  JSON envelope. Caught by `FlowTimeoutEnforcement` asserting on
  `error_kind`.
- **BUG-D** — `sanitizeExpression` had no size cap; a 1 MB
  expression flew straight through to the child GHCi. CWE-400 DoS
  across 9 tools. Caught by `FlowOversizedInput`.
- **BUG-G** — `ghci_deps(add/remove)` had no locking; two clients
  editing concurrently dropped a write. Caught by
  `FlowConcurrentClients`.
- Test-level bugs (false positives) caught by the scenarios'
  post-fix oracles: wrong field name `packages` vs `build_depends`
  (v1 of `FlowDependencyConflict` and `FlowConcurrentClients`), 0xFF
  planted inside a comment (v1 of `FlowNonUTF8`). Each was
  surfaced by an independent oracle step in the same scenario.
  ('FlowGhciSigkill' was retired when the Wave-5 in-process GHCi
  migration removed the subprocess-respawn code path it exercised —
  its v1 / v2 exitWith + exitImmediately bugs are historical only.
  In-process equivalents (user-space exceptions, uncaught 'error'
  calls) remain covered by 'FlowSessionRobustness'.)

## References

- `test-e2e/E2E/Assert.hs` — the 65-line assertion + report layer.
  No hspec, no tasty; deliberately minimal.
- `test-e2e/E2E/Client.hs` — in-process client; explains why we're
  not using a subprocess on macOS-arm64.
- `test-e2e/Main.hs` — orchestrator; scenario list lives here.
