# Haskell MCP Workflow

## PRIME DIRECTIVE

The goal is MCP-driven development. Every decision goes through an MCP tool.
Even if you already know the implementation — USE THE TOOLS FIRST.
The compiler's structured output drives development, not pre-existing knowledge.

---

## TOOL TIERS

### Tier 1 — Every function (the inner loop)
| Tool | When |
|------|------|
| `ghci_load` | After every `.hs` edit — no exceptions |
| `ghci_type` | Verify types before AND after implementation |
| `ghci_hole_fits` | Read typed hole analysis before implementing |

### Tier 2 — Frequently during development
| Tool | When |
|------|------|
| `ghci_info` | Understand a type, class, or function before using it |
| `hoogle_search` | Find a function by type signature |
| `ghci_eval` | Test a function with sample input |
| `ghci_add_import` | Resolve "Not in scope" errors |
| `ghci_complete` | Discover available names by prefix |

### Tier 3 — Module complete gate (MANDATORY before next module)
| Tool | When |
|------|------|
| `ghci_quickcheck` | **MANDATORY** — 1-3 properties testing the module's algebraic contract |
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
Write: module header + exports + imports + type sigs with = undefined
  → ghci_load(diagnostics=true) → fix any type design errors
  → 0 errors? → start FLOW 4 for each function
```

**Stubs** (OK in one Write): module declaration, exports, imports, data types, type sigs with `= undefined`.
**Implementation** (ONE at a time): function bodies replacing `= undefined`.

## FLOW 4: Implement One Function ← THE CORE LOOP

```
1. HOLE       Replace = undefined with = _
2. COMPILE    ghci_load(diagnostics=true) → read hole: expected type, fits, bindings
3. EXPLORE    ghci_type / ghci_info / hoogle_search on anything you need to understand
4. IMPLEMENT  Write the function body (max ~20 lines)
5. COMPILE    ghci_load(diagnostics=true)
6. FIX        errors → fix → recompile | warnings → fix ALL → recompile (zero tolerance)
7. VERIFY     ghci_type("functionName") → confirm the type is what you expect
8. TEST       ghci_eval("functionName sampleArg") → verify runtime behavior
```

**Steps 1-2 are MANDATORY.** Never skip the hole phase — even if you know the answer.
**Step 7 is MANDATORY.** Never skip type verification after implementation.

## FLOW 5: Explore & Discover (use DURING Flow 4, step 3)

```
Need a function for a type?     → hoogle_search("a -> b -> c")
What does this name do?         → ghci_info("name")
What's in scope?                → ghci_imports / ghci_complete("prefix")
Need documentation?             → ghci_doc("name")
Jump to definition?             → ghci_goto("name")
```

Don't guess — ask the compiler.

## FLOW 6: Module Complete ← MANDATORY GATE (do NOT skip to next module)

Before starting the next module, you MUST complete ALL of these steps:

```
1. QUICKCHECK  ghci_quickcheck — test the MODULE'S CONTRACT (1-3 properties)
               Properties test RELATIONSHIPS BETWEEN functions, not individual functions.
               → fail? read counterexample → fix → ghci_load → retry
2. REVIEW      ghci_check_module(module_path="...") → review API summary
3. LINT        ghci_lint(module_path="...") → apply good suggestions
4. FORMAT      ghci_format(module_path="...", write=true) → format code
```

**Step 1 is MANDATORY.** You cannot move to the next module without running QuickCheck.

### What to test: the module's algebraic contract

Properties should group multiple functions together and test their relationships:

```
Substitution module:
  "apply nullSubst t == t"                              -- identity
  "apply (compose s1 s2) t == apply s1 (apply s2 t)"   -- composition law

Unification module:
  "unify t t == Right nullSubst"                        -- reflexivity
  "case unify t1 t2 of Right s -> apply s t1 == apply s t2; _ -> True"

Pretty printer:
  "parse (pretty x) == x"                               -- roundtrip
```

**Don't test individual functions in isolation** — test how the module's exports
work together. 1-3 properties per module is enough if they cover the key laws.

If custom types lack Arbitrary instances, use `ghci_eval` with concrete examples
as a minimum, but prefer QuickCheck when possible.

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
| Incomplete patterns | `ghci_info("Type")` to see all constructors |
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

- Implementation without hole phase (Flow 4 steps 1-2)
- Skipping `ghci_type` after implementation (Flow 4 step 7)
- **Moving to next module without running `ghci_quickcheck`** (Flow 6 step 1)
- Multiple `.hs` edits between `ghci_load` calls
- Using Bash for ANY Haskell toolchain operation
- "I'll fix warnings later" — fix them NOW
- MCP tool fails → falling back to Bash (diagnose → retry → `mcp_restart` → ask user)
- Writing a full module in one Write call (use FLOW 3 → FLOW 4)
