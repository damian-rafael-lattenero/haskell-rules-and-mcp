# Automated Development Loop

## The Development State Machine

You are ALWAYS in one of these states. There is NO shortcut from EDIT to DONE.

```
┌─────────┐
│  EDIT   │──── Write/Edit ONE function body (max ~20 lines)
└────┬────┘
     │
┌────▼────┐
│ COMPILE │──── ghci_load(diagnostics=true)
└────┬────┘
     │
┌────▼────┐     ┌───────────┐
│ CHECK   │────►│ FIX ERROR │──► back to COMPILE
│ RESULT  │     └───────────┘
└────┬────┘
     │ (errors == 0)
┌────▼─────────┐  ┌──────────────┐
│ CHECK WARNS  │─►│ FIX WARNING  │──► back to COMPILE
└────┬─────────┘  └──────────────┘
     │ (warningActions == 0)
┌────▼────┐
│  DONE   │──── Move to next function / ghci_quickcheck
└─────────┘
```

---

## Primary Protocol

After every edit, run `ghci_load`. Read the structured output. Take the FIRST applicable action:

1. If `errors` > 0 → apply Error Resolution (below)
2. If `warningActions` > 0 → fix each one automatically (below)
3. If success with 0 issues → move to next task

**NEVER ask the developer "should I fix this warning?" — just fix it.**
**NEVER leave -Wall warnings unfixed unless explicitly told to ignore them.**

---

## Warning Action Table

When `ghci_load` returns `warningActions[]`, act on each one by matching `warningActions[].category`:

| category | warningFlag | Action |
|---|---|---|
| unused-import | -Wunused-imports | Remove the import line. If partially used, narrow to only the used names. |
| unused-binding | -Wunused-matches | Replace binding with `_` wildcard. |
| unused-binding | -Wunused-local-binds | If genuinely unused: prefix with `_`. If it should be used: it's a bug, investigate. |
| incomplete-patterns | -Wincomplete-patterns | Add the missing pattern cases. Use `ghci_info` on the type to see all constructors. |
| missing-signature | -Wmissing-signatures | The `suggestedAction` field contains the inferred type. Add it as a signature. |
| name-shadowing | -Wname-shadowing | Rename the inner binding to avoid shadowing. |
| redundant-constraint | -Wredundant-constraints | Remove the unused constraint from the type signature. |
| unused-do-bind | -Wunused-do-bind | Add `void $` or `_ <-` before the expression. |
| type-defaults | -Wtype-defaults | Add explicit type annotation to remove defaulting. |
| typed-hole | -Wtyped-holes | Read the hole fits. Pick the best one or implement. NOT auto-fixable. |

After fixing all warnings, compile again to verify 0 warnings remain.

---

## Error Resolution Table

When `ghci_load` returns `errors[]`, match on `errors[].code` and apply:

| Code | Name | Action | Verify with |
|---|---|---|---|
| GHC-83865 | Type mismatch | Read `expected`/`actual` fields. expected=`X->Y`, actual=`X` → missing arg. expected=`IO X`, actual=`X` → wrap in `pure`. expected=`X`, actual=`IO X` → use `<-` not `let`. | `ghci_type` on `context` subexpr |
| GHC-39999 | Not in scope | (1) Module in .cabal exposed-modules? (2) Missing import? → `ghci_add_import` to find the right module. (3) Typo? → `ghci_complete` to find similar names. | `ghci_info` after adding import |
| GHC-39660 | No instance | Own type → add `deriving`. Constraint missing → add to sig. Orphan → add import. | `ghci_info` on the type |
| GHC-46956 | Ambiguous type | Add explicit type annotation to the ambiguous expression. | `ghci_type` on subexpressions |

### After 2 failed attempts on same error
1. Replace expression with `undefined`
2. `ghci_type` on context → see expected type
3. Build bottom-up from verified sub-expressions
4. **NEVER** rewrite large code sections speculatively

---

## ANTI-PATTERNS

These are real mistakes. Each one shows the wrong way and the right way.

### Bulk-Write Module
```
BAD:  Write entire HM/Infer.hs (200 lines) in one Write call
GOOD: Write stubs → compile → implement infer() → compile → implement generalize() → compile
```

### Multiple Files Without Compiling
```
BAD:  Write Syntax.hs, then Subst.hs, then Unify.hs, then compile
GOOD: Write Syntax.hs → ghci_load → Write Subst.hs → ghci_load → Write Unify.hs → ghci_load
```

### Skip Bootstrap
```
BAD:  Write .cabal + all source files + ghci_load at the very end
GOOD: Write .cabal → ghci_scaffold → ghci_session(restart) → ghci_load → then start coding
```

### Ignore Warnings
```
BAD:  "It compiles, there are some warnings but I'll deal with them later"
GOOD: Fix EVERY warningAction before moving to the next function
```

### Guess Instead of Ask the Compiler
```
BAD:  Write a complex expression and hope the types work out
GOOD: Use ghci_type on subexpressions, use typed holes (_), let GHC guide you
```

### MCP Bypass (the most dangerous anti-pattern)
```
BAD:  MCP tool fails → fall back to manual Write/Bash → code without compilation gate
GOOD: MCP tool fails → read error → fix root cause → retry MCP tool → mcp_restart if needed
```
This is the #1 way the development loop breaks. When a tool fails, the temptation is to "just write the file manually." This silently disables the entire MCP-driven workflow — you lose structured errors, typed holes, warning actions, and the compilation gate. **The tools failing is a bug to fix, not a reason to abandon them.**

### Skip Pre-Flight
```
BAD:  Start writing .hs files immediately without checking MCP health
GOOD: ghci_session(status) → ghci_switch_project() → verify alive → then start
```

---

## QuickCheck Integration

Use `ghci_quickcheck` to verify properties during development:
- After implementing a new function: write a property and test it
- For parsers: `parse (ppExpr e)` should roundtrip
- For algebraic operations: test associativity, identity, inverses
- If a property fails: the counterexample tells you exactly what input breaks

---

## The Loop (Summary)

```
edit ONE function
  → ghci_load (compile)
    → errors? fix them, recompile
    → warnings? fix them ALL, recompile
    → clean? move on
      → ghci_quickcheck (verify properties)
        → pass? done
        → fail? fix with counterexample, recompile
```

This loop runs WITHOUT human intervention until the task is complete.
The developer only intervenes for design decisions.
