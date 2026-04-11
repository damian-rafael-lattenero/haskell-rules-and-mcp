# GHC Error Pattern Knowledge Base

When encountering a GHC error, ALWAYS read the Expected vs Actual types first. Do NOT guess fixes — use `:t` to verify your understanding before changing code.

If an error persists after 2 attempts: break the expression into smaller let-bindings with explicit type annotations and compile each one separately.

---

## Type Mismatch Errors

### [GHC-83865] Couldn't match type
**What**: The expression has a different type than what the context expects.
**Common causes** (check in this order):
1. Function arguments in wrong order
2. Missing or extra function application (partially applied vs fully applied)
3. Numeric type mismatch (Int vs Integer, Int vs Double) — use `fromIntegral` or `fromRational`
4. String vs Text vs ByteString confusion
5. Monadic value where pure value expected (or vice versa) — missing `return` or extra `<-`

**Fix strategy**: Check Expected vs Actual in the error. Use `:t subexpression` to find exactly where the mismatch originates.
```haskell
-- WRONG: length returns Int, can't add to Double
bad = length [1,2,3] + 1.5
-- RIGHT:
good = fromIntegral (length [1,2,3]) + 1.5
```

### [GHC-18872] Couldn't match expected type with actual type (rigid type variable)
**What**: A polymorphic type variable can't be unified with a concrete type.
**Common causes**:
1. Trying to return different concrete types from branches of a case/if
2. Missing `ScopedTypeVariables` when referencing a type variable from an outer scope
3. GADT pattern match losing type information

**Fix strategy**: Add explicit `forall` with `ScopedTypeVariables`, or add type annotations to case branches.

### [GHC-27346] Couldn't match expected type: wrong number of arguments
**What**: A function is applied to too many or too few arguments.
**Common causes**:
1. Parentheses missing: `f a b` vs `f (a b)`
2. Operator precedence: `f . g x` means `f . (g x)`, not `(f . g) x`
3. Accidentally treating a constructor as a function with extra args

**Fix strategy**: Count arguments. Use `:t functionName` to see exactly how many arguments it expects.

---

## Scope and Name Errors

### [GHC-88464] Variable not in scope (typed hole)
**What**: This is NOT an error — it's useful information. GHC found a typed hole `_` and is telling you what type it needs.
**Action**: Read the "Relevant bindings" and "Valid hole fits" sections. These tell you what expressions would work.

### [GHC-39999] Not in scope
**What**: A name is used that GHC can't find.
**Common causes**:
1. Missing import — use `:i name` in GHCi or search Hoogle
2. Typo in function/variable name
3. Qualified name needed: `Map.lookup` instead of `lookup` (if Data.Map is imported qualified)
4. Function defined below its first use (only in GHCi, not in modules)
5. Missing module in `.cabal` file `exposed-modules`

**Fix strategy**: Check imports. Use Hoogle to find which module exports the name.

### [GHC-76037] Ambiguous occurrence
**What**: A name is imported from multiple modules.
**Fix**: Use qualified imports or hiding clauses.
```haskell
-- WRONG: both Prelude and Data.Map export 'lookup'
import Data.Map
-- RIGHT:
import qualified Data.Map as Map
-- then use: Map.lookup
```

---

## Typeclass Errors

### [GHC-39660] No instance for (SomeClass SomeType)
**What**: A typeclass instance is needed but not available.
**Common causes**:
1. Missing `deriving` clause on a data type
2. Missing import of an orphan instance (common with `aeson`, `binary`)
3. Need to add a constraint to the function's type signature
4. The instance genuinely doesn't exist and you need to write one

**Fix strategy**: Check if the type derives the class. Check if there's an orphan instance in another module. If you own the type, add `deriving (Show, Eq, Ord)` etc.
```haskell
-- WRONG: no Show instance
data Foo = Foo Int
-- RIGHT:
data Foo = Foo Int deriving (Show, Eq, Ord)
```

### [GHC-46956] Ambiguous type variable
**What**: GHC can't determine which concrete type to use for a polymorphic value.
**Common causes**:
1. `show (read x)` — GHC doesn't know the intermediate type
2. Numeric literals without context: `fromIntegral 5` — what's the target type?
3. Overloaded strings/lists without type context

**Fix strategy**: Add an explicit type annotation to the ambiguous expression.
```haskell
-- WRONG: ambiguous intermediate type
bad = show (read "42")
-- RIGHT:
good = show (read "42" :: Int)
```

### [GHC-56834] Could not deduce constraint
**What**: A function body uses an operation that requires a constraint not in the signature.
**Fix**: Add the missing constraint to the type signature.
```haskell
-- WRONG: uses == but no Eq constraint
myElem :: a -> [a] -> Bool
-- RIGHT:
myElem :: Eq a => a -> [a] -> Bool
```

---

## Syntax and Parse Errors

### Parse error on input '='
**Common causes**:
1. Missing `let` in a `do` block: `x = expr` should be `let x = expr`
2. Indentation error — the `=` is not aligned correctly
3. Missing `where` or `let...in`

### Parse error (possibly incorrect indentation)
**What**: Haskell's layout rule is not satisfied.
**Fix strategy**:
1. Ensure all definitions in a `where` block are aligned to the same column
2. In `do` blocks, all statements must be aligned to the same column
3. Never mix tabs and spaces — use spaces only
4. The body after `where`, `let`, `do`, `of` must be indented further than the keyword

### Unexpected 'do' / Parse error in pattern
**Common causes**:
1. Missing `$` or parentheses: `putStrLn show x` should be `putStrLn $ show x` or `putStrLn (show x)`
2. Missing `do` keyword in a monadic sequence
3. Using `<-` outside of a `do` block or list comprehension

---

## Kind Errors

### [GHC-83865] Expected kind * but got kind * -> *
**What**: A type constructor is used without enough type arguments.
**Common causes**:
1. `Maybe` where `Maybe Int` is needed (in instance declarations, type annotations)
2. Wrong kind in typeclass instances: `instance Functor Int` (Int has kind *, Functor needs * -> *)

**Fix strategy**: Use `:k TypeName` in GHCi to check the kind. Add the required type arguments.

---

## Infinite Type Errors

### [GHC-83865] Occurs check: cannot construct infinite type
**What**: A type would need to contain itself, creating an infinite type.
**Common causes**:
1. Accidentally applying a function to itself: `f f`
2. List of mixed types: `[1, [2]]` (list element and nested list)
3. Missing `newtype` wrapper for recursive data structures

**Fix strategy**: Introduce a `newtype` wrapper for recursion, or fix the logic error.
```haskell
-- WRONG: infinite type
bad xs = xs : xs
-- RIGHT: probably meant
good xs = [xs]
```

---

## Monadic / Do-Notation Errors

### Couldn't match type 'IO ()' with '()'
**What**: Mixing monadic and pure code.
**Common causes**:
1. Using a pure expression as a statement in a `do` block (add `return` or `pure`)
2. Using `let x = action` instead of `x <- action` in a `do` block
3. Returning `IO a` from a function annotated as `a`

**Fix strategy**: Remember: in `do` blocks, `<-` unwraps monadic values, `let` binds pure values.
```haskell
-- WRONG:
main = do
  let line = getLine  -- getLine is IO String, not String
-- RIGHT:
main = do
  line <- getLine     -- unwrap the IO
```

### Couldn't match type 'IO' with 'Maybe' (or any two different monads)
**What**: Trying to use two different monads in the same `do` block.
**Fix**: Use monad transformers (e.g., `MaybeT IO`) or explicit `case` to handle the inner monad.

---

## Record Errors

### [GHC-45392] Fields of record do not match
**What**: Pattern match or construction with wrong field names.
**Fix**: Check the data type definition for exact field names.

### Duplicate field name
**What**: GHC2024 does NOT enable `DuplicateRecordFields` by default.
**Fix**: Use different field name prefixes, or enable `DuplicateRecordFields` + `OverloadedRecordDot`.

---

## Quick Debugging Checklist

When stuck on ANY type error:
1. [ ] Read Expected vs Actual types in the error
2. [ ] Use `:t` on the specific subexpression mentioned in "In the expression: ..."
3. [ ] Check if function is partially applied (missing argument)
4. [ ] Check if you're in a monadic context (need `<-` vs `let`, `return` vs bare value)
5. [ ] Check operator precedence with `:i operator` (shows fixity)
6. [ ] If error chains through multiple functions, start from the innermost expression
7. [ ] When all else fails: add explicit type annotations to EVERY subexpression and compile — the first one that fails is your bug
