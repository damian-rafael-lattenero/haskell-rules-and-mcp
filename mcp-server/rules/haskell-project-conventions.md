# Haskell Project Conventions

## Toolchain
- Use `Haskell2010` or `GHC2024` as default-language (project's choice)
- Enable `-Wall` for both library and executable
- Dependencies: keep base, containers, mtl as core; add QuickCheck for property testing

## Import Style
- Qualified imports for Map/Set (e.g., `import qualified Data.Map.Strict as Map`)
- Prefer explicit import lists for application modules
- Use unqualified imports only for the project's own modules
- Cross-module imports are generated automatically by `ghci_scaffold`

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
- Pass `module="src/X.hs"` to `ghci_quickcheck` for accurate property tracking
- Use `ghci_regression` to re-run all saved properties after changes
