# Haskell MCP Workflow

## PRIME DIRECTIVE

The goal is MCP-driven development. Every decision goes through an MCP tool.
Even if you already know the implementation — USE THE TOOLS FIRST.
The compiler's structured output drives development, not pre-existing knowledge.

---

## MODE

### guided (default)
Full ceremony — every MANDATORY step in every flow.
Best for: new Haskell developers, unfamiliar codebases, complex type-level code.

### expert
Relaxed inner loop. The compiler still drives, but skip ceremony when confident:
- FLOW 4 Steps 1-2 (hole phase): RECOMMENDED, not mandatory
- FLOW 4 Step 7 (post-impl type verify): RECOMMENDED, not mandatory
- FLOW 4 Step 3 (explore): skip freely when types are familiar

### ALWAYS mandatory (both modes):
- `ghci_load` after every `.hs` edit — **no exceptions**
- `ghci_quickcheck` incremental (FLOW 4.5) AND at module-complete (FLOW 6)
- Zero tolerance for warnings
- Arbitrary instances for data types in stub phase

---

## TOOL TIERS

### Tier 1 — Every function (the inner loop)
| Tool | When |
|------|------|
| `ghci_load` | After every `.hs` edit — no exceptions |
| `ghci_type` | Verify types before AND after implementation |
| `ghci_hole_fits` | Read typed hole analysis before implementing |
| `ghci_suggest` | **At start of FLOW 4** — auto-discover hole fits for all undefined functions |

### Tier 2 — Frequently during development
| Tool | When |
|------|------|
| `ghci_info` | Understand a type, class, or function before using it |
| `hoogle_search` | Find a function by type signature |
| `ghci_eval` | Test a function with sample input |
| `ghci_add_import` | Resolve "Not in scope" errors |
| `ghci_complete` | Discover available names by prefix |
| `ghci_arbitrary` | **FLOW 3** — generate Arbitrary instances for new data types |
| `ghci_workflow` | Check progress, get next step, view checklist |
| `ghci_trace` | Debug logic errors — wrap expressions with Debug.Trace |

### Tier 3 — Module complete gate (MANDATORY before next module)
| Tool | When |
|------|------|
| `ghci_quickcheck` | **MANDATORY** — test the module's COMPLETE algebraic contract |
| `ghci_check_module` | Review the module's API summary |
| `ghci_lint` | Code quality pass |
| `ghci_format` | Formatting pass |

### Tier 4 — As needed
`ghci_batch` · `ghci_kind` · `ghci_doc` · `ghci_goto` · `ghci_references` · `ghci_rename` · `ghci_imports`

---

## FLOW 1: Pre-Flight (once per session)

```
ghci_session(status) → alive?
  → ghci_switch_project() → see available projects
  → ghci_load(load_all=true) → verify compiles
```

If dead: `ghci_session(restart)`. Still dead: `mcp_restart`. Still dead: STOP, ask the user.

## FLOW 2: New Project

```
Write .cabal
  → ghci_scaffold
  → ghci_session(restart)
  → ghci_load(load_all=true) → verify 0 errors
  → start FLOW 3 for each module
```

## FLOW 3: New Module (stub phase)

```
Write: module header + exports + imports + data types + type sigs with = undefined
  → ghci_load(diagnostics=true) → fix any type design errors
  → If data types defined: ghci_arbitrary(type_name="MyType") for EACH data type
    → Copy generated instance into source file (import Test.QuickCheck)
    → ghci_load → verify Arbitrary compiles
  → 0 errors? → ghci_suggest(module_path="...") → review suggestions overview
  → start FLOW 4 for each function
```

**Stubs** (OK in one Write): module declaration, exports, imports, data types, type sigs with `= undefined`.
**Implementation** (ONE at a time): function bodies replacing `= undefined`.

**`ghci_arbitrary` is MANDATORY when the module defines data types.** It generates correct
Arbitrary instances including `sized`/`resize` for recursive types. Don't write them by hand.

**`ghci_suggest` is MANDATORY before starting FLOW 4.** It shows you what GHC expects for
each undefined function — expected types, valid hole fits, relevant bindings. This gives
you a roadmap for implementation before you write a single line.

**IMPORTANT: Add Arbitrary instances IN THE MODULE, not inline in GHCi.**
GHCi multiline definitions are fragile and time out. Define them in the source file
during the stub phase so QuickCheck is available from the start. Add `QuickCheck` to
the `.cabal` build-depends if not already there (FLOW 7).

## FLOW 4: Implement One Function ← THE CORE LOOP

Before starting the first function, run `ghci_suggest(module_path="...")` to see
the full picture: expected types and hole fits for ALL undefined functions at once.
Use `ghci_workflow(action="checklist")` to track progress as you go.

```
1. HOLE       Replace = undefined with = _                           [guided]
2. COMPILE    ghci_load(diagnostics=true) → read hole analysis       [guided]
3. EXPLORE    ghci_type / ghci_info / hoogle_search                  [guided]
4. IMPLEMENT  Write the function body (max ~20 lines)                [all modes]
5. COMPILE    ghci_load(diagnostics=true)                            [all modes]
6. FIX        errors → fix → recompile | warnings → fix ALL         [all modes]
7. VERIFY     ghci_type("functionName") → confirm type               [guided]
8. TEST       ghci_eval("functionName sampleArg") → test behavior    [all modes]
9. PROPERTY   **Incremental QuickCheck — FLOW 4.5**                  [all modes]
```

**Steps 1-2 are MANDATORY in guided mode.** In expert mode, skip when the implementation is obvious.
**Step 7 is MANDATORY in guided mode.** In expert mode, skip for simple functions.
**Step 9 is MANDATORY in ALL modes.** Test laws as soon as they become testable.

Use `ghci_quickcheck(property="suggest", function_name="funcName")` to discover testable laws.

**Debugging logic errors (correct types but wrong behavior):**
Use `ghci_trace(expression, trace_points=["subexpr1", "subexpr2"])` to see intermediate
values during evaluation. This is for step 8 when `ghci_eval` shows unexpected results.

## FLOW 4.5: Incremental Property Check (triggered from FLOW 4 step 9)

When a function completes an algebraic law:

```
1. IDENTIFY    Which law(s) are now testable?
               → Use ghci_quickcheck(property="suggest", function_name="...") to discover
2. FORMULATE   Write the property as a lambda expression
3. TEST        ghci_quickcheck(property, incremental=true)
4. FIX         fail? → read counterexample → fix → ghci_load → re-test
5. CONTINUE    Return to FLOW 4 for the next function
```

Common trigger points:
| Just implemented | Law now testable | Example |
|-----------------|------------------|---------|
| `apply` + `emptySubst` | Substitution identity | `\t -> apply emptySubst t == t` |
| `composeSubst` | Composition | `\s1 s2 t -> apply (compose s1 s2) t == apply s1 (apply s2 t)` |
| `unify` | Correctness | `\t1 t2 -> case unify t1 t2 of Right s -> apply s t1 == apply s t2; _ -> True` |
| Monoid instance | Monoid laws | `ghci_quickcheck(property="suggest", function_name="MyType")` |

**Principle**: A property is testable the moment ALL functions it references are implemented.
Run it IMMEDIATELY — do not accumulate untested properties.

## FLOW 5: Explore & Discover (use DURING Flow 4, step 3)

```
Need a function for a type?     → hoogle_search("a -> b -> c")
What does this name do?         → ghci_info("name")
What's in scope?                → ghci_imports / ghci_complete("prefix")
Need documentation?             → ghci_doc("name")
Jump to definition?             → ghci_goto("name")
```

Don't guess — ask the compiler.

## FLOW 6: Module Complete ← CONFIRMATION GATE (do NOT skip to next module)

If you followed FLOW 4.5 throughout implementation, this gate CONFIRMS that all properties
still hold after every function is implemented. It should be a quick re-run, not the first test.

Before starting the next module, you MUST complete ALL of these steps:

```
1. QUICKCHECK  ghci_quickcheck — test the module's COMPLETE algebraic contract
               One property per law. The module dictates how many, not an arbitrary cap.
               → fail? read counterexample → fix → ghci_load → retry
2. REVIEW      ghci_check_module(module_path="...") → review API summary
3. LINT        ghci_lint(module_path="...") → apply good suggestions
4. FORMAT      ghci_format(module_path="...", write=true) → format code
```

**Step 1 is MANDATORY.** You cannot move to the next module without running QuickCheck.

### What to test: the COMPLETE algebraic contract

Identify ALL the laws that the module's exports should satisfy, then test each one.
Properties test RELATIONSHIPS BETWEEN functions, not individual functions in isolation.

```
Substitution module — 4 laws:
  "apply nullSubst t == t"                              -- identity
  "apply (compose s1 s2) t == apply s1 (apply s2 t)"   -- composition
  "compose nullSubst s == s"                            -- left unit
  "compose s nullSubst == s"                            -- right unit

Unification module — 3 laws:
  "unify t t == Right nullSubst"                        -- reflexivity
  "case unify t1 t2 of Right s -> apply s t1 == apply s t2; _ -> True"  -- correctness
  "case unify t1 t2 of Right _ -> unify t2 t1 is also Right; _ -> True" -- symmetry

Pretty printer — 1 law:
  "parse (pretty x) == x"                               -- roundtrip

Type inference — 2 laws:
  "inferExpr env (ELit (LInt n)) == Right (Forall [] typeInt)"  -- literal typing
  "inferExpr env (ELam x body) returns a function type"         -- lambda typing
```

Testing a partial contract is like not testing — a bug in an untested law passes silently.

If custom types lack Arbitrary instances, use `ghci_eval` with concrete cases
as a minimum, but prefer QuickCheck whenever possible.

## FLOW 7: Add Dependency or Module

```
Edit .cabal
  → ghci_scaffold (if new module)
  → ghci_session(restart)
  → ghci_load(load_all=true) → verify clean
```

---

## ERROR RESOLUTION (use the right tool, don't guess)

| Situation | Tool |
|-----------|------|
| "Not in scope" | `ghci_add_import("name")` → `ghci_info` to verify |
| Type mismatch | `ghci_type` on subexpressions to find the divergence |
| "No instance" | `ghci_info("Type")` to see available instances |
| "No Arbitrary" | `ghci_arbitrary(type_name="Type")` → generate and add to source |
| Incomplete patterns | `ghci_info("Type")` to see all constructors |
| Logic error (types OK, wrong result) | `ghci_trace(expression, trace_points=["x","y"])` → inspect values |
| Don't know where to start | `ghci_suggest(module_path="...")` → see hole fits for all undefined |
| Lost track of progress | `ghci_workflow(action="next")` → see what step to do next |
| 2 failed attempts | `undefined` → `ghci_type` on context → build bottom-up |

## WARNING AUTO-FIX

Fix EVERY `warningAction` immediately — never "deal with it later":

| Category | Action |
|----------|--------|
| `unused-import` | Remove or narrow the import |
| `missing-signature` | Add the type from `suggestedAction` |
| `incomplete-patterns` | `ghci_info` for constructors, add missing cases |
| `unused-binding` | Prefix with `_` or remove |
| `name-shadowing` | Rename the inner binding |
| `typed-hole` | Read the fits, pick the best one or implement |

---

## FORBIDDEN

- Implementation without hole phase (Flow 4 steps 1-2) **[guided mode]**
- Skipping `ghci_type` after implementation (Flow 4 step 7) **[guided mode]**
- **Moving to next module without running `ghci_quickcheck`** (Flow 6 step 1) **[all modes]**
- **Skipping incremental QuickCheck when a law becomes testable** (Flow 4.5) **[all modes]**
- **Writing Arbitrary instances by hand** when `ghci_arbitrary` can generate them (FLOW 3) **[all modes]**
- **Starting FLOW 4 without running `ghci_suggest`** to preview hole fits first **[all modes]**
- Multiple `.hs` edits between `ghci_load` calls **[all modes]**
- Using Bash for ANY Haskell toolchain operation **[all modes]**
- "I'll fix warnings later" — fix them NOW **[all modes]**
- MCP tool fails → falling back to Bash (diagnose → retry → `mcp_restart` → ask user) **[all modes]**
- Writing a full module in one Write call (use FLOW 3 → FLOW 4) **[all modes]**
