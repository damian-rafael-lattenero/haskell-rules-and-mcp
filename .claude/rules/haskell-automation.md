# Automated Development Loop

## Primary Protocol
After every edit, run `ghci_load`. Read the structured output. Take the FIRST applicable action:

1. If `errors` > 0 → apply Error Resolution (below)
2. If `warningActions` > 0 → fix each one automatically (below)
3. If success with 0 issues → move to next task

**NEVER ask the developer "should I fix this warning?" — just fix it.**
**NEVER leave -Wall warnings unfixed unless explicitly told to ignore them.**

---

## Warning Action Table

When `ghci_load` returns `warningActions`, act on each one:

| warningFlag | category | Action |
|---|---|---|
| -Wunused-imports | unused-import | Remove the import line. If partially used, narrow to only the used names. |
| -Wunused-matches | unused-binding | Replace binding with `_` wildcard. |
| -Wunused-local-binds | unused-binding | If genuinely unused: prefix with `_`. If it should be used: it's a bug, investigate. |
| -Wincomplete-patterns | incomplete-patterns | Add the missing pattern cases. Use `ghci_info` on the type to see all constructors. |
| -Wmissing-signatures | missing-signature | The `suggestedAction` contains the inferred type. Add it as a signature. |
| -Wname-shadowing | name-shadowing | Rename the inner binding to avoid shadowing. |
| -Wredundant-constraints | redundant-constraint | Remove the unused constraint from the type signature. |
| -Wunused-do-bind | unused-do-bind | Add `void $` or `_ <-` before the expression. |
| -Wtype-defaults | type-defaults | Add explicit type annotation to remove defaulting. |
| -Wtyped-holes | typed-hole | Read the hole fits. Pick the best one or implement. NOT auto-fixable. |

After fixing all warnings, compile again to verify 0 warnings remain.

---

## Error Resolution Protocol

When `ghci_load` returns errors, use the structured fields:

### Type mismatch (GHC-83865)
- Read `expected` and `actual` from the error JSON
- If expected=`X -> Y`, actual=`X` → missing function argument
- If expected=`X`, actual=`X -> Y` → extra argument or missing parens
- If expected=`IO X`, actual=`X` → wrap in `pure`
- If expected=`X`, actual=`IO X` → change `let` to `<-` in do-block
- Use `ghci_type` on the subexpression from `context` to verify before fixing

### Not in scope (GHC-39999)
- Is the module in exposed-modules in .cabal?
- Is there a missing import? Use `hoogle_search` to find the right module
- Is it a typo? Look for similar names in the module

### No instance (GHC-39660)
- If we own the type: add `deriving` clause
- If the constraint is missing from signature: add it
- If it's an orphan instance: add the right import

### Ambiguous type (GHC-46956)
- Add explicit type annotation to the ambiguous expression

### After 2 failed fix attempts on the same error:
1. Replace the expression with `undefined`
2. Run `ghci_type` on the context to see expected type
3. Build the expression bottom-up from verified sub-expressions
4. NEVER rewrite large code sections speculatively

---

## QuickCheck Integration

Use `ghci_quickcheck` to verify properties during development:
- After implementing a new function: write a property and test it
- For parsers: `parse (ppExpr e)` should roundtrip
- For algebraic operations: test associativity, identity, inverses
- If a property fails: the counterexample tells you exactly what input breaks

---

## The Loop

```
edit code
  → ghci_load (compile)
    → errors? fix them, recompile
    → warnings? fix them all, recompile
    → clean? move on
      → ghci_quickcheck (verify properties)
        → pass? done
        → fail? fix with counterexample, recompile
```

This loop runs WITHOUT human intervention until the task is complete.
The developer only intervenes for design decisions.
