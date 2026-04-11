---
paths:
  - "src/**/*.hs"
  - "app/**/*.hs"
  - "*.cabal"
  - "cabal.project"
---

# Project: haskell-rules-and-mcp

## Toolchain
- **GHC**: 9.12 (pinned in `cabal.project` via `with-compiler: ghc-9.12`)
- **Language**: GHC2024
- **Build system**: Cabal 3.12
- **Platform**: macOS aarch64

## GHC2024 Extensions Enabled by Default
These are ON without needing explicit pragmas:
- `DataKinds`, `DerivingStrategies`, `DisambiguateRecordFields`
- `ExplicitNamespaces`, `GADTs`, `MonoLocalBinds`
- `LambdaCase`, `RoleAnnotations`
- `TypeData`, `TypeFamilies`

NOT enabled by default (must add pragma if needed):
- `OverloadedStrings`, `OverloadedRecordDot`
- `DuplicateRecordFields`
- `TemplateHaskell`
- `ScopedTypeVariables` (use explicit `forall` instead in GHC2024)

## Project Layout
```
src/          -- Library modules (hs-source-dirs for library)
app/          -- Executable modules (hs-source-dirs for executable)
```

- Library modules go in `src/` and must be listed in `exposed-modules` in the `.cabal` file
- Executable source goes in `app/`
- The executable depends on the library (`haskell-rules-and-mcp` in build-depends)

## Dependencies
Current: `base >= 4.21`, `containers >= 0.7`, `array`
To add a new dependency: edit `haskell-rules-and-mcp.cabal`, add to BOTH library and executable `build-depends` if needed, then `cabal build` to resolve.

## Build Commands
```bash
# Build everything
export PATH="$HOME/.ghcup/bin:$PATH" && cabal build 2>&1

# Start REPL for the library
export PATH="$HOME/.ghcup/bin:$PATH" && cabal repl lib:haskell-rules-and-mcp 2>&1

# Run the executable
export PATH="$HOME/.ghcup/bin:$PATH" && cabal run haskell-rules-and-mcp 2>&1

# Clean build artifacts
export PATH="$HOME/.ghcup/bin:$PATH" && cabal clean

# Quick type-check an expression
export PATH="$HOME/.ghcup/bin:$PATH" && echo ':t EXPRESSION' | cabal repl lib:haskell-rules-and-mcp 2>&1
```

## Compiler Flags
- `-Wall` is enabled for both library and executable
- The `.ghci` file enables: `-fdefer-type-errors`, `-ferror-spans`, `-fprint-explicit-foralls`

## Adding a New Module
1. Create the file in `src/NewModule.hs`
2. Add `NewModule` to `exposed-modules` in the `.cabal` file
3. Run `cabal build` to verify it compiles
4. Import it where needed
