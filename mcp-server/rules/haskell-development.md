# Haskell Development Workflow

## STANDARD FLOWS

These are the main development flows. Each one is a complete path — follow the steps in order.

### Flow 1: New Project
```
Write .cabal
  → ghci_scaffold
  → ghci_session(restart)
  → ghci_load(load_all=true)
  → verify 0 errors
  → start Flow 2 for each module
```

### Flow 2: New Module (stub phase)
```
Write module header: module declaration + exports + imports + type sigs with = undefined
  → ghci_load(diagnostics=true)
  → fix errors (missing imports, typos in types, etc.)
  → 0 errors? → start Flow 3 for each function
```

**What counts as a stub (OK to write in one go):**
- `module` declaration + export list
- `import` lines
- `data` / `newtype` / `type` declarations (these are API design, not implementation)
- `class` declarations with method signatures
- Function type signatures with `= undefined`

**What counts as implementation (ONE at a time, each followed by ghci_load):**
- Function bodies (replacing `= undefined` with real code)
- Typeclass instance method bodies

### Flow 3: Implement One Function
```
Replace = undefined with = _ (typed hole)
  → ghci_load(diagnostics=true)
  → read hole: expected type, bindings in scope, valid fits
  → write the implementation (guided by hole info)
  → ghci_load(diagnostics=true)
  → errors? → fix, recompile
  → warnings? → fix ALL, recompile
  → clean? → ghci_type on key subexpressions to verify
  → move to next function
```

**Why `_` before implementing?** GHC tells you the expected type, what's in scope, and even suggests fits. This turns GHC into your pair programmer. Use `undefined` only for stubs you won't implement yet (it compiles silently). Use `_` for the function you're implementing NOW (it gives you feedback).

### Flow 4: Verify with QuickCheck
```
After implementing a group of related functions:
  → ghci_quickcheck(property="...")
  → pass? → done, move on
  → fail? → read counterexample → fix → ghci_load → ghci_quickcheck again
```

### Flow 5: Explore & Discover
```
Don't know what function exists for a type? → hoogle_search("a -> b -> c")
Don't know what a name does?               → ghci_info("name")
Don't know what's in scope?                → ghci_imports / ghci_complete("prefix")
Need documentation?                        → ghci_doc("name")
Need to find a definition?                 → ghci_goto("name")
```

Use Flow 5 **during** Flow 3 — don't guess, ask the compiler.

### Flow 6: Add a Dependency or Module
```
Edit .cabal (add dependency or module to exposed-modules)
  → ghci_scaffold (if new module added — creates stub file)
  → ghci_session(restart) (picks up .cabal changes)
  → ghci_load(load_all=true)
  → verify clean
```

---

## MANDATORY STUB PHASE

For **EVERY** new module, follow Flow 2 above. The full sequence:

1. **Write ONLY**: module declaration + export list + imports + type signatures with `= undefined`
2. **`ghci_load`** — verify the stubs compile (types are consistent across modules)
3. **THEN** implement functions **ONE AT A TIME** using Flow 3:
   a. Replace ONE `= undefined` with `= _` (typed hole)
   b. `ghci_load(diagnostics=true)` — read the hole's expected type and fits
   c. Write the implementation based on hole info
   d. `ghci_load(diagnostics=true)` — fix any errors, fix ALL warnings
   e. `ghci_type` on key subexpressions to verify
   f. Repeat for the next function

**NEVER** write a full module implementation without going through the stub phase first.

### Why Stubs First?
- Catches type design errors BEFORE you invest time implementing
- Forces you to think about the API before the implementation
- Makes compilation errors small and localized (1 function at a time)
- The MCP gives you structured error output — use it incrementally, not in bulk

---

## Implementation Cadence

- **Maximum 1 function body** per edit-compile cycle
- **Maximum ~20 lines** of new Haskell between `ghci_load` calls
- After implementing each function: **MANDATORY** `ghci_type` on key subexpressions to verify
- After implementing a group of related functions: `ghci_quickcheck` to verify properties

---

## Type-First Development (MANDATORY)

These are not suggestions. Follow them always.

- Write type signatures **BEFORE** implementations — for ALL functions, not just complex ones
- Use `ghci_type` to verify subexpression types before composing them
- Use `ghci_info` to understand typeclass hierarchies and available instances
- Use `hoogle_search` to find functions by type signature when unsure what exists
- Use typed holes (`_`) when starting to implement a function — let GHC guide you

---

## Typed Holes

When implementing a function, **start with `= _`** (typed hole), not a guess:
- `ghci_load` with diagnostics will show: expected type, relevant bindings, valid hole fits
- `ghci_hole_fits` gives more detailed structured fits
- Pick the most appropriate fit or use the type information to guide implementation

**`_` vs `undefined`:**
- `= _` → for the function you are implementing NOW (gives compiler feedback)
- `= undefined` → for stubs you will implement LATER (compiles silently)

---

## Error Recovery

When a compilation error persists after 2 fix attempts:
1. Replace the expression with `undefined`
2. Use `ghci_type` on the context to see expected type
3. Build the expression bottom-up from verified sub-expressions
4. **NEVER** rewrite large code sections speculatively — isolate the specific failing subexpression

---

## Navigation & Discovery (Flow 5)

Use these tools **during implementation**, not just for exploration:

- Use `ghci_goto` to jump to the definition of any name (returns file:line for local, module for library)
- Use `ghci_complete` to discover available functions matching a prefix
- Use `ghci_doc` to read Haddock documentation for library functions
- Use `ghci_imports` to see what's currently in scope
- Use `ghci_add_import` when a name is "Not in scope" — it finds the right module via Hoogle
- Use `hoogle_search` to find functions by type signature — "I need `Map k v -> [(k,v)]`"

---

## Code Quality

- Use `ghci_format` to format code with ormolu/fourmolu (if installed)
- Use `ghci_lint` to get hlint suggestions (if installed)
- Apply lint suggestions that improve clarity; skip those that reduce readability

---

## Module Hygiene

- New modules **MUST** be added to `exposed-modules` in `.cabal` before compiling
- Use explicit export lists in every module
- Prefer qualified imports for library modules (e.g., `import Data.Map.Strict qualified as Map`)
- After adding a module to `.cabal`: follow Flow 6 (scaffold → restart → load)
