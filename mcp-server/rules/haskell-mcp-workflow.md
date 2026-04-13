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

### Tier 3 — At milestones
| Tool | When |
|------|------|
| `ghci_quickcheck` | After implementing 3-5 related functions |
| `ghci_check_module` | After finishing a module — review the API |
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

## FLOW 5: Verify with QuickCheck

```
After 3-5 related functions:
  → ghci_quickcheck(property="\\x -> prop x") → pass? done
  → fail? read counterexample → fix → ghci_load → ghci_quickcheck again
```

Properties to test: roundtrips, algebraic laws, identity, associativity, inverses.

## FLOW 6: Explore & Discover (use DURING Flow 4, step 3)

```
Need a function for a type?     → hoogle_search("a -> b -> c")
What does this name do?         → ghci_info("name")
What's in scope?                → ghci_imports / ghci_complete("prefix")
Need documentation?             → ghci_doc("name")
Jump to definition?             → ghci_goto("name")
```

Don't guess — ask the compiler.

## FLOW 7: Module Complete

```
ghci_check_module(module_path="...") → review API summary
  → ghci_lint(module_path="...") → apply good suggestions
  → ghci_format(module_path="...", write=true) → format code
```

## FLOW 8: Add Dependency or Module

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
- Multiple `.hs` edits between `ghci_load` calls
- Using Bash for ANY Haskell toolchain operation
- "I'll fix warnings later" — fix them NOW
- MCP tool fails → falling back to Bash (diagnose → retry → `mcp_restart` → ask user)
- Writing a full module in one Write call (use FLOW 3 → FLOW 4)
