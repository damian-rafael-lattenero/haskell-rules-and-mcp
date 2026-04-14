# Haskell Project Conventions

## Toolchain
- Use `Haskell2010` or `GHC2024` as default-language (project's choice)
- Enable `-Wall` for both library and executable
- Dependencies: keep base, containers, mtl as core; add QuickCheck for property testing
- Build tool: Cabal by default; pass `build_tool="stack"` to `ghci_init` for Stack projects
- MCP toolchain resolution for `hlint`, `fourmolu`/`ormolu`, and `hls` is:
  **host PATH first, then bundled binary**.
- Always check `source` and `binaryPath` fields in tool responses when diagnosing
  lint/format/HLS behavior.

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

## Dependency Management
- **Never** manually edit `.cabal` `build-depends` for adding or removing packages
- Use `ghci_deps(action="add", package="name")` to add
- Use `ghci_deps(action="remove", package="name")` to remove
- Use `ghci_deps(action="list")` to inspect current dependencies
- After any dependency change, run `ghci_session(restart)` to reload GHCi
- `base` is protected — it cannot be removed via `ghci_deps`

## Testing
- Use QuickCheck for property-based testing
- Write properties alongside implementations
- Test algebraic laws: associativity, identity, roundtrip
- Use `Arbitrary` instances with size-controlled generation for AST types
- Pass `module_path="src/X.hs"` to `ghci_quickcheck` for accurate property tracking
  (`module="src/X.hs"` is also accepted for backward compatibility)
- Use `ghci_regression` to re-run all saved properties after changes
- Use `ghci_hole(module_path="...")` to explore typed holes before implementing
- **Do NOT use trivially-true properties** (`\x -> True`, `const True`) — they are
  automatically dropped by `ghci_quickcheck_export` and provide no test coverage
- `ghci_quickcheck_export` auto-adds qualified imports (`Data.Map.Strict`, `Data.Set`, etc.)
  based on what the properties actually use — no manual import editing needed in `Spec.hs`

## Refactoring
- Use `ghci_refactor(action="rename_local")` to rename a binding across a module
  — never use manual find/replace or `sed` for this
- Use `ghci_refactor(action="extract_binding")` to lift code to a new top-level function
- Always run `ghci_load(diagnostics=true)` after any refactor to verify compilation

## Performance
- Before optimizing, run `ghci_profile(action="suggest", module_path="...")` for static hints
- Common issues detected: `String` concatenation in loops, naive recursion without accumulator,
  `head`/`fromJust` partial functions, unhandled `Map.lookup` results
- For benchmarking: `ghci_profile(action="time")` runs GHC time profiling

## Language Extensions
- Use `ghci_flags(action="set", flags="-XSomeExtension")` for session-only exploration
- To persist an extension: add to `default-extensions` in the `.cabal` file,
  then `ghci_session(restart)` to apply
- Use `ghci_flags(action="list")` to see currently active language settings

## HLS Integration
- Run `ghci_hls(action="available")` to check if HLS is installed
- Use `ghci_hls(action="hover", module_path="...", line=N, character=M)` for type info at position
- HLS resolution is host-first, then bundled. MCP does not auto-install HLS.
- If unavailable, provide the binary in host PATH or bundled toolchain and retry.
- For all compilation diagnostics: prefer `ghci_load(diagnostics=true)` — it doesn't require HLS

## Bundled Toolchain Maintenance
- Bundled binaries are tracked in `vendor-tools/bundled-tools-manifest.json`.
- Update checksums/version metadata with:
  `npm run tools:update-manifest -- --tool <name> --platform <platform> --arch <arch> --version <version> --provenance <url>`
- Run unit/integration/e2e test suites after updating bundled binaries.
