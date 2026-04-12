# Haskell Project Conventions

## Toolchain
- Use GHC2024 as default language
- Enable `-Wall` for both library and executable
- Use `.ghci` with `-fdefer-type-errors -ferror-spans -fprint-explicit-foralls`
- Dependencies: keep base, containers, mtl as core; add QuickCheck for property testing

## Import Style
- All Map/Set imports should be `qualified` (e.g., `import Data.Map.Strict qualified as Map`)
- Prefer explicit import lists for application modules
- Use unqualified imports only for the project's own modules

## Module Structure
- New modules must be added to `exposed-modules` in `.cabal` before compiling
- Use explicit export lists in every module
- Keep modules focused: one type or one concern per module
- Separate pure logic from IO

## Naming
- Types: PascalCase (`TypeEnv`, `ParseError`)
- Functions: camelCase (`inferExpr`, `parseProgram`)
- Type variables: single lowercase letters (`a`, `b`, `t`)
- Modules: dotted hierarchy matching directory structure (`HM.Infer`, `Parser.Core`)

## Testing
- Use QuickCheck for property-based testing
- Write properties alongside implementations
- Test algebraic laws: associativity, identity, roundtrip
- Use `Arbitrary` instances with size-controlled generation for AST types
