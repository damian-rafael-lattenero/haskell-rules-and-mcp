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

## Error Resolution Table

When `ghci_load` returns errors, match on `code` and apply:

| Code | Name | Action | Verify with |
|---|---|---|---|
| GHC-83865 | Type mismatch | Read `expected`/`actual`. expected=`X->Y`, actual=`X` → missing arg. expected=`IO X`, actual=`X` → wrap in `pure`. expected=`X`, actual=`IO X` → use `<-` not `let`. | `ghci_type` on `context` subexpr |
| GHC-39999 | Not in scope | (1) Module in .cabal exposed-modules? (2) Missing import? → `hoogle_search`. (3) Typo? → similar names. | `ghci_info` after adding import |
| GHC-39660 | No instance | Own type → add `deriving`. Constraint missing → add to sig. Orphan → add import. | `ghci_info` on the type |
| GHC-46956 | Ambiguous type | Add explicit type annotation to the ambiguous expression. | `ghci_type` on subexpressions |

### After 2 failed attempts on same error
1. Replace expression with `undefined`
2. `ghci_type` on context → see expected type
3. Build bottom-up from verified sub-expressions
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
