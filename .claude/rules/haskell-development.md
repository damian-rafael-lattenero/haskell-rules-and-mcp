# Haskell Development Workflow

## PATH Configuration
All Haskell tool invocations (ghc, cabal, ghci, hoogle) MUST be prefixed with:
```bash
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH" &&
```

## Type-Driven Development Protocol

1. **ALWAYS write the type signature BEFORE the implementation**
2. For functions with complex types (3+ type variables, constraints, higher-kinded types): write the signature, compile with `= undefined` as body, verify it type-checks, THEN implement
3. Use explicit `:: Type` annotations on let-bindings for any non-trivial subexpression
4. When unsure about a type, query GHCi BEFORE writing the code:
   ```bash
   export PATH="$HOME/.ghcup/bin:$PATH" && echo ':t EXPRESSION' | cabal repl lib:haskell-rules-and-mcp 2>&1
   ```

## Incremental Compilation Protocol

**CRITICAL: NEVER write more than ONE top-level function without compiling.**

The workflow for every function must be:
1. Write the type signature
2. Write `= undefined` as the body
3. Compile: `export PATH="$HOME/.ghcup/bin:$PATH" && cabal build 2>&1`
4. If the signature compiles, implement the body
5. Compile again
6. Fix any errors BEFORE moving to the next function
7. Repeat for the next function

For complex modules with many interdependent functions:
1. Write ALL type signatures first, all with `= undefined` bodies
2. Compile to verify all signatures are consistent
3. Implement bodies ONE AT A TIME, compiling after each one

## Typed Holes for Discovery

When you don't know what expression to write, use a typed hole `_`:
```haskell
myFunc :: [a] -> Int
myFunc xs = _ -- GHC will tell you it needs :: Int, with relevant bindings
```

- Named holes `_nameHere` are better for readability in complex expressions
- GHC shows: expected type, relevant bindings in scope, and valid hole fits
- The `.ghci` config has `-fdefer-type-errors` enabled, so holes become warnings not hard errors

## Using :t for Subexpression Verification

Before composing complex expressions, verify the types of subparts:
```bash
# Check what type an expression has
export PATH="$HOME/.ghcup/bin:$PATH" && echo ':t map (+1)' | cabal repl lib:haskell-rules-and-mcp 2>&1

# Check type info for a type or typeclass
export PATH="$HOME/.ghcup/bin:$PATH" && echo ':i Functor' | cabal repl lib:haskell-rules-and-mcp 2>&1

# Check the kind of a type constructor
export PATH="$HOME/.ghcup/bin:$PATH" && echo ':k Maybe' | cabal repl lib:haskell-rules-and-mcp 2>&1
```

Use `:t` proactively when:
- Composing functions with `.` or `>>=` to verify the types align
- Using polymorphic functions where the concrete type matters
- Working with monad transformers or complex typeclass hierarchies
- Debugging a type error by checking subexpressions individually

## Subagent Strategy for Parallel Type-Checking

When writing multiple functions or a large module:
1. Write a batch of 3-5 functions following the incremental protocol
2. Launch a background Agent (`run_in_background: true`) to verify the entire module compiles:
   ```
   Agent(run_in_background=true): "Run cabal build for the project and report any type errors found"
   ```
3. Continue writing the next section while the background agent verifies
4. If the background agent reports errors, STOP and fix them before continuing
5. Never accumulate more than 5 unverified functions

## Error Recovery Protocol

When a compilation error occurs:
1. **READ the error message carefully** — check Expected vs Actual types
2. **Do NOT guess** — use `:t` on subexpressions to understand the mismatch
3. If the error is unclear, simplify: break the expression into let-bindings with explicit type annotations
4. If the same error persists after 2 fix attempts:
   - Comment out the problematic expression
   - Replace with `undefined` (to unblock the rest of the module)
   - Use `:t` to verify what type the context expects
   - Rebuild the expression from smaller, verified pieces
5. Never rewrite large sections of code to "fix" a type error — isolate the specific subexpression that's wrong

## Module Hygiene

- Every new module must be added to `exposed-modules` or `other-modules` in the `.cabal` file BEFORE compiling
- Use explicit export lists: `module Foo (func1, func2, Type(..)) where`
- Prefer qualified imports for library modules to avoid name clashes
- After adding a new dependency to the `.cabal` file, run `cabal build` to ensure it resolves
