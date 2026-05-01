# haskell-flows-mcp — Concurrency Contract

> Issue [#97](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/97) Phase A.  Audience: integrators (host wrappers,
> parallel-client setups), agents writing batching logic, and anyone
> reading directly from `.haskell-flows/`.  Each contract below is
> tagged **ENFORCED** (an MVar / lock makes it impossible to violate)
> or **DOCUMENTED INTENT** (the source assumes it but does not yet
> mechanically prevent the failure mode).  Phases B–E of [#97](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/97) close
> the gap between those two states; this doc tracks the snapshot.

---

## Overview

The MCP has three concurrency surfaces:

| Surface | Scope | Phase A status |
|---|---|---|
| Single MCP, multiple in-flight tool calls | Per `GhcSession` | **ENFORCED** (see Contract 1) |
| Multiple MCPs against the same project | Cross-process, same filesystem | **DOCUMENTED INTENT** (see Contracts 2–4) |
| Mid-call client disconnect / reconnect | JSON-RPC transport | **DOCUMENTED INTENT** (see Contract 5) |

Each subsequent contract states what the consumer can safely rely on
*today* and what they must defensively avoid.

---

## Contract 1 — Single-MCP per-call serialisation  **(ENFORCED)**

Every `tools/call` request handled by one MCP server is serialised
against every other tool call **on the same `GhcSession`**.  The
`gsLock :: MVar ()` in `HaskellFlows.Ghc.ApiSession` is taken via
`withMVar` for every `withGhcSession` invocation.

**What this means for the consumer:**

* Sending two `ghc_eval` requests in parallel does **not** speed them
  up — they execute sequentially.
* `tools/call` request ordering on the wire = effective execution
  ordering.
* Throughput is bounded by the slowest tool in the active queue.
* The lock is **per-session, not global**.  Two MCP servers on
  different project roots do not block each other on this lock; they
  do compete for filesystem state — see Contracts 2–5.

**Enforcing site:**
[`src/HaskellFlows/Ghc/ApiSession.hs`](../mcp-server-haskell/src/HaskellFlows/Ghc/ApiSession.hs) — `gsLock` field on `GhcSession`,
acquired in `withGhcSession`.

**Why it exists:**  GHC's `HscEnv` is not thread-safe within one
process.  Two threads invoking `setSessionDynFlags` or `load` against
the same `HscEnv` simultaneously corrupt the module graph.

**Test coverage:**  e2e scenario `FlowConcurrentClients` exercises
two clients hitting the same MCP simultaneously.

---

## Contract 2 — Property store consistency  **(IN-PROCESS ENFORCED, CROSS-PROCESS DOCUMENTED INTENT)**

`.haskell-flows/properties.json` is the persisted set of QuickCheck
laws that have passed at least once.  It is read on every
`ghc_regression run` and written on every `ghc_quickcheck` pass.

**What is enforced today:**

1. **Per-`Store` MVar (`sLock`).**  Within one MCP process, every
   `loadAll` / `save` / `remove` is serialised.  No torn writes from
   the same process.
2. **In-process global MVar (`inProcessStoreLock`).**  Multiple
   `Store` values opened within one MCP — e.g. after a project switch
   — also serialise on the same path.

**What is *not* enforced today (DOCUMENTED INTENT):**

3. **Cross-process file lock.**  Two MCP servers running against the
   same project root *can* race on the read-modify-write cycle.
   Last-writer-wins, silently.  Phase B of [#97](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/97) adds an `flock`
   /`fcntl`-based file lock to close this hole.

**What this means for the consumer:**

* Within one wrapper that owns a single MCP child: safe; no special
  precautions.
* Across two wrappers (e.g. two terminals open against the same
  project): assume an `flock` will arrive; until it does, **avoid
  parallel `ghc_quickcheck` from independent MCPs against the same
  project**.  Sequencing them via your wrapper is the cheapest mitigation.
* Direct file reads of `properties.json` by external tooling: the
  file format is a single JSON array; it is rewritten atomically only
  *within* one MCP — external readers may catch a partial state if
  they read while a cross-process race is in flight.

**Enforcing site:**
[`src/HaskellFlows/Data/PropertyStore.hs`](../mcp-server-haskell/src/HaskellFlows/Data/PropertyStore.hs) — `inProcessStoreLock`
(top-level) + `sLock` (per-`Store`).

**Test coverage:**  unit tests `propstore: save auto-creates dir`,
`propstore: save after rm -rf dir`, `propstore: concurrent saves no
loss` cover the in-process surface.

---

## Contract 3 — Cabal file consistency  **(DOCUMENTED INTENT)**

`ghc_deps`, `ghc_add_modules`, and `ghc_remove_modules` are the only
tools that rewrite the project's `.cabal` file.  The current
implementation is a non-atomic read-modify-write
(`TIO.writeFile file newBody`).

**What is *not* enforced today:**

* **Atomic write.**  A crash mid-write or two parallel writers
  produces a torn cabal file.
* **Cross-process serialisation.**  Two MCP servers both calling
  `ghc_deps add` against the same `.cabal` simultaneously can
  interleave their writes.

**What is enforced today:**

* **Post-write verification.**  Every `ghc_deps` write re-parses the
  cabal file after rewriting and refuses the operation if the verb
  (added / removed) does not appear in the parsed dep list.  This
  surfaces a torn write as a `success: false` rather than a silent
  corruption — but the file is *still* torn.

**What this means for the consumer:**

* Single MCP, sequential calls: safe; the existing per-session lock
  serialises rewrites within the process.
* Multiple MCPs against the same project: **avoid concurrent cabal
  edits**.  Sequence them via your wrapper.
* External tools that watch `.cabal` (e.g. HLS, `cabal repl`):
  expect transient torn states until Phase C of [#97](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/issues/97) lands.

**Phase C will add:** atomic-rename writes (`<file>.tmp` →
`rename`) plus an `flock` around the read-modify-write cycle.

**Site:**
[`src/HaskellFlows/Tool/Deps.hs`](../mcp-server-haskell/src/HaskellFlows/Tool/Deps.hs),
[`src/HaskellFlows/Tool/AddModules.hs`](../mcp-server-haskell/src/HaskellFlows/Tool/AddModules.hs),
[`src/HaskellFlows/Tool/RemoveModules.hs`](../mcp-server-haskell/src/HaskellFlows/Tool/RemoveModules.hs).

---

## Contract 4 — Stanza-flag captures  **(DOCUMENTED INTENT)**

The MCP captures the per-stanza `ghc-options` produced by
`cabal repl --with-compiler=…` and stores them under
`.haskell-flows/flags/<stanza>.args`.  The captures are auto-invalidated
on `.cabal` mtime change (see BUG-PLUS-03 fix in `ApiSession.hs`).

**What is *not* enforced today:**

* **Atomic write of the capture.**  The current path is a direct
  `writeFile`.  Two MCPs bootstrapping the same stanza simultaneously
  can interleave.

**What this means for the consumer:**

* Auto-invalidation on cabal mtime means that a stale capture is
  self-healing within one tool call.
* Concurrent bootstrap of the *same* stanza: the worst case is a
  re-bootstrap on the next call.  No silent data loss, but a brief
  performance hit.

**Phase E will add:** atomic-rename writes (`<file>.tmp` → `rename`).

**Site:**  `ApiSession.hs` (`ensureStanzaFlags` + `captureStanzaFlags`).

---

## Contract 5 — Client disconnect mid-call  **(DOCUMENTED INTENT)**

If the JSON-RPC client closes its end of stdio while a tool call is
in flight:

**What is enforced today:**

* `Server.runTool` wraps every handler in a 10-minute outer
  `System.Timeout.timeout`.  On expiry, the in-flight action is
  killed via async exception, the MVar releases, and the binary
  remains alive for subsequent connections.
* GHC `SessionStatus` machinery (`Alive | Overflowed | Dead`)
  guarantees that a wedged GHCi flips to `Dead` and the next caller
  sees `SessionExhausted` rather than hanging.

**What is *not* enforced today:**

* **EOF detection during a call.**  The MCP does not actively poll
  stdin while it is computing; an early-disconnect client waits for
  the 10-minute timeout to fire even though the actual work
  completes faster.

**What this means for the consumer:**

* Client may safely abandon a request: the server will not leak.
* Client should expect that the server may continue running the
  abandoned tool call to completion (especially for fast tools like
  `ghc_type` / `ghc_eval`); resources are not actively reclaimed.
* Long calls (e.g. `ghc_check_project` on a 100-module project) sit
  in the kernel pipe until the timeout if the client disconnects
  mid-call.

**Phase D will add:** active EOF detection on stdin → cancellation
of the in-flight Ghc action.

**Site:**  [`src/HaskellFlows/Mcp/Server.hs`](../mcp-server-haskell/src/HaskellFlows/Mcp/Server.hs) — `runTool` outer
timeout; `ApiSession.hs` — `SessionStatus` STM TVar.

---

## Contract 6 — CWD discipline  **(ENFORCED — by absence)**

The MCP **never mutates the process-level current working directory**.
Every tool that needs to invoke `cabal` or `ghc-pkg` against a specific
project passes the path explicitly via flags (`--project-dir`,
`--working-directory`) rather than `setCurrentDirectory`.

**What this means for the consumer:**

* Spawning the MCP from any working directory is safe.
* Two MCPs against different project roots in the same shell never
  clobber each other's CWD.
* Wrappers that themselves manage CWD (e.g. embedding the MCP inside
  a larger build tool) need not unwind anything after a tool call.

**Why this rule exists:**  BUG-PLUS-XX (see `ApiSession.hs` line
~267) fixed a race where the original code used `withCurrentDirectory`
to rebase relative cabal paths.  Two parallel e2e scenarios serialised
by the MVar but raced on the *process-global* CWD outside the lock,
causing one scenario to compile against the wrong project.  The fix
was `absolutizeStanzaFlags`: rewrite argv paths to absolute, never
touch the CWD.

**Site:**  `ApiSession.hs` — `absolutizePathArg`,
`absolutizeStanzaFlags`.

**Test coverage:**  `ghc-api: absolutizePathArg single-token shapes
(#43)`, `ghc-api: absolutizeStanzaFlags two-token pairs (#43)`,
`ghc-api: absolutizeStanzaFlags idempotent (#43)`,
`ghc-api: absolutizeStanzaFlags preserves order (#43)`.

---

## Quick reference — what is safe today

| Scenario | Safe? | Why |
|---|---|---|
| One MCP, sequential `tools/call` requests | ✅ | Default, fully enforced. |
| One MCP, parallel `tools/call` requests | ✅ | MVar serialises (Contract 1). |
| Two MCPs, different projects, in parallel | ✅ | No shared filesystem state. |
| Two MCPs, same project, **read-only** tools (`ghc_type`, `ghc_browse`, etc.) | ✅ | No writes, no contention. |
| Two MCPs, same project, parallel `ghc_quickcheck` | ⚠️ | Cross-process property-store race; sequence externally until Phase B. |
| Two MCPs, same project, parallel `ghc_deps add` | ⚠️ | Cross-process cabal-file race; sequence externally until Phase C. |
| Client disconnects mid-call | ✅ for resource leak; ⚠️ for early reclaim | 10-minute outer timeout fires; Phase D will cut to ~1 s. |
| `HASKELL_FLOWS_E2E_PARALLEL=N` against the project | ✅ | E2E test harness uses one MCP per scenario worker; no cross-process file state contention beyond the existing per-MCP MVars. |

---

## Roadmap

| Phase | Scope | Status |
|---|---|---|
| **A** | This document — contracts + status snapshot | ✅ landed |
| B | `withFileLock` wrapper + property-store cross-process lock | open |
| C | `withFileLock` + atomic-rename for `.cabal` writes | open |
| D | EOF-on-stdin → cancellation of in-flight tool call | open |
| E | Atomic-rename for stanza-flag captures | open |
| F | (optional) RWLock instead of MVar for read-only tools | deferred |

Each phase replaces a "DOCUMENTED INTENT" tag in the contract list
above with "ENFORCED".  This doc is the single source of truth — when
a phase lands, the contract's status changes here in the same PR.

---

## BUG-PLUS audit (for the curious)

The source carries `BUG-PLUS-XX` comments documenting concurrency
bugs that have already been patched.  Each represents a concrete
race-condition discovery + fix; the next race is the one not yet
discovered.  Phase A's measurement-driven addition: every BUG-PLUS in
the source has a regression test cited next to it.

To audit: `grep -rn 'BUG-PLUS' mcp-server-haskell/src` — the
comments live where the fix landed; the test names cite the same
identifier.

The patterns the audit confirms:

* **CWD races** → solved by absolutize-argv-don't-mutate-CWD.
* **Stanza-flag staleness** → solved by mtime-based invalidation.
* **Deferred-tool wrapper writes** → solved by a write-then-flush
  protocol (BUG-PLUS-08 patch in `AddModules.hs`).

---

*Last updated: Phase A landing.  Update this doc in the same PR that
flips a contract from "DOCUMENTED INTENT" to "ENFORCED".*
