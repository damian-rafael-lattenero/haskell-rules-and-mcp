# MCP Flow Diagrams

Visual map of the four most important flows across the 20 tools shipped
in Phases 1-8. GitHub renders the Mermaid blocks inline — no tooling
needed to read this file.

Conventions used across every diagram:

| Shape       | Meaning                                  |
|-------------|------------------------------------------|
| `([text])`  | Terminal state (start / end)             |
| `[text]`    | MCP tool invocation                      |
| `((text))`  | Sub-loop referenced by name              |
| `[/text/]`  | Filesystem side effect                   |
| `[(text)]`  | Persistent store (JSON on disk)          |

---

## 1. Property-first dev loop

The central workflow. Every tool in the dev triangle (load / hole /
quickcheck) converges here; the terminal state is a module whose gates
are all green.

```mermaid
flowchart TD
    start([Agent starts]) --> load[ghc_load<br/>diagnostics=true]
    load -->|errors| fix[Agent edits code]
    load -->|holes detected| hole[ghc_hole]
    load -->|clean| qc

    hole --> explore[ghc_type<br/>ghc_info<br/>hoogle_search<br/>ghc_doc]
    explore --> impl[Agent implements hole]
    impl --> load

    qc[ghc_quickcheck<br/>property=...] -->|QcPassed| persist[(PropertyStore<br/>auto-persist<br/>.haskell-flows/<br/>properties.json)]
    qc -->|QcFailed / Exception| fix
    qc -->|QcGaveUp| relax[Agent relaxes<br/>precondition]
    relax --> qc

    persist --> check[ghc_check_module]
    check -->|compile OK<br/>no warnings<br/>no holes<br/>props pass| done([Module complete])
    check -->|any gate red| fix

    fix --> load

    style done fill:#9f9,stroke:#383,stroke-width:2px
    style persist fill:#fd9,stroke:#a73
    style qc fill:#9cf,stroke:#369
```

---

## 2. Bootstrap a new project end-to-end

Zero-to-shippable with an empty directory as the input. No hand-edits
to `.cabal`, no manual scaffolding.

```mermaid
flowchart LR
    empty([Empty directory]) --> create[ghc_create_project<br/>name=mypkg]
    create --> files[/.cabal<br/>cabal.project<br/>src/Mypkg.hs<br/>test/Spec.hs/]

    files --> deps[ghc_deps<br/>action=add<br/>package=text]
    deps --> restart[ghc_session<br/>action=restart]
    restart --> first[ghc_load<br/>src/Mypkg.hs]

    first --> types[Agent defines<br/>data types]
    types --> arb[ghc_arbitrary<br/>type_name=...]
    arb --> paste[Agent pastes<br/>Arbitrary instance]
    paste --> devloop((property-first<br/>dev loop))

    devloop --> allgreen[ghc_regression<br/>action=run]
    allgreen --> coverage[ghc_coverage]
    coverage --> ship([Shippable])

    style create fill:#fcf,stroke:#939
    style devloop fill:#9cf,stroke:#369
    style ship fill:#9f9,stroke:#383,stroke-width:2px
```

---

## 3. Refactor with snapshot-and-compile (the safety model)

This is the Phase 8 invariant: the compiler is the correctness oracle.
Textual rewrites are legal only if GHCi still type-checks the result;
otherwise the snapshot is restored verbatim.

```mermaid
sequenceDiagram
    autonumber
    actor A as Agent
    participant T as Tool.Refactor
    participant V as validateIdentifier<br/>+ mkModulePath
    participant FS as File System
    participant G as GHCi Session

    A->>T: rename_local(old, new, scope, dry_run=true)
    T->>V: validate old + new + path
    V-->>T: ok (or reject → error to agent)
    T->>FS: read original
    T->>T: word-boundary rewrite<br/>(skips comments + strings)
    T-->>A: preview JSON (disk untouched)

    Note over A,T: agent decides to apply

    A->>T: rename_local(..., dry_run=false)
    T->>FS: read original (snapshot)
    T->>T: recompute rewrite
    T->>FS: write rewrite
    T->>G: loadModuleWith Strict
    G-->>T: diagnostics

    alt Compile green
        T-->>A: success, committed
    else Compile red
        T->>FS: restore snapshot verbatim
        alt restore OK
            T-->>A: errors + "snapshot restored"
        else restore failed
            T-->>A: errors + ⚠️ "file is dirty"
        end
    end
```

---

## 4. GHCi session lifecycle (DoS + recovery model)

The Phase-5 security invariant. An agent asking for
`print [1..]` via `ghc_eval` will cause the child process to pipe
unbounded output — the buffer cap + overflow state + MVar eviction
turn that from a memory exhaustion vector into a self-healing recovery.

```mermaid
stateDiagram-v2
    [*] --> Spawning : startSession<br/>(cabal repl)

    Spawning --> Alive : init sentinel<br/>received

    Alive --> Alive : execute<br/>buffer < 16 MiB
    Alive --> Overflowed : drainHandle cap<br/>16 MiB exceeded

    Overflowed --> Evicted : next tool call<br/>throws<br/>SessionExhausted

    Evicted --> Spawning : getOrStartSession<br/>rebuilds on demand

    Alive --> [*] : killSession<br/>(explicit)
    Evicted --> [*] : Server shutdown

    note right of Alive
        STM lock serialises
        concurrent execute calls
    end note

    note right of Overflowed
        drainHandle keeps draining
        so the GHCi child never
        deadlocks on a pipe write
    end note

    note right of Evicted
        The Server MVar is set
        to Nothing — next tool
        call transparently boots
        a fresh child process
    end note
```

---

## Tool coverage

| Tool                   | Appears in |
|------------------------|------------|
| `ghc_load`            | 1, 2       |
| `ghc_hole`            | 1          |
| `ghc_type`            | 1          |
| `ghc_info`            | 1          |
| `hoogle_search`        | 1          |
| `ghc_doc`             | 1          |
| `ghc_quickcheck`      | 1          |
| `ghc_check_module`    | 1          |
| `ghc_create_project`  | 2          |
| `ghc_deps`            | 2          |
| `ghc_session`         | 2, 4       |
| `ghc_arbitrary`       | 2          |
| `ghc_regression`      | 2          |
| `ghc_coverage`        | 2          |
| `ghc_refactor`        | 3          |

Not shown because they're auxiliary / meta tools that don't anchor a
distinct flow: `ghc_eval`, `ghc_complete`, `ghc_goto`, `ghc_format`,
`ghc_workflow`.
