# Haskell Project Conventions

## Toolchain
- Use `Haskell2010` or `GHC2024` as default-language (project's choice)
- Enable `-Wall` for both library and executable
- Dependencies: keep base, containers, mtl as core; add QuickCheck for property testing
- `ghci_init` now seeds `containers` and QuickCheck by default so Map-heavy projects don't start in a broken state
- Build tool: Cabal by default; pass `build_tool="stack"` to `ghci_init` for Stack projects
- MCP toolchain resolution for `hlint`, `fourmolu`/`ormolu`, and `hls` is:
  **host PATH first, then bundled binary, then auto-download**.
- Always check `source` and `binaryPath` fields in tool responses when diagnosing
  lint/format/HLS behavior.
- Use `ghci_toolchain_status` to capture runtime + cross-platform matrix diagnostics before changing release metadata.

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
- If `ghci_arbitrary` warns about `listOf`/recursive growth, prefer `resize` or other bounded generation helpers
- Pass `module_path="src/X.hs"` to `ghci_quickcheck` for accurate property tracking
  (`module="src/X.hs"` is also accepted for backward compatibility)
- Use `ghci_regression` to re-run all saved properties after changes
- Use `ghci_hole(module_path="...")` to explore typed holes before implementing
- If QuickCheck returns a counterexample, use `ghci_trace` before guessing at a fix
- **Do NOT use trivially-true properties** (`\x -> True`, `const True`) — they are
  automatically dropped by `ghci_quickcheck_export` and provide no test coverage
- `ghci_quickcheck_export` auto-adds qualified imports (`Data.Map.Strict`, `Data.Set`, etc.)
  based on what the properties actually use — no manual import editing needed in `Spec.hs`
- `ghci_quickcheck_export` validates the exported suite with `cabal_test` by default
- `ghci_quickcheck` rejects unsafe properties (e.g., unused binders) before persistence
- Run `ghci_property_lifecycle(action="audit")` to find invalid persisted properties before export
- Use `ghci_fuzz_parser(parser="...")` when parser robustness or malformed-input handling matters

## Refactoring
- Use `ghci_refactor(action="rename_local")` to rename a binding across a module
  — never use manual find/replace or `sed` for this
- Use `ghci_refactor(action="extract_binding")` to lift code to a new top-level function
- Use `ghci_apply_exports(module_path="...")` to materialize the explicit export list suggested by `ghci_check_module`
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

## Session Management Best Practices

- Never use `:set +m` in .ghci files (breaks sentinel protocol)
- Never use `:set prompt` or `:set prompt-cont` (overridden by MCP)
- If session feels stuck, check health status (session auto-reports health)
- Timeouts auto-trigger session restart on next tool call
- Session health states: `healthy` (normal), `degraded` (slow), `corrupted` (needs restart)
- After timeout error, next MCP tool call will auto-recover the session

## Warning Management

- Fix warnings immediately using `ghci_fix_warning` when available
- Check `suggestedFixes` in `ghci_load` responses for auto-fixable warnings
- Supported auto-fixes: unused-matches (GHC-40910), unused-imports (GHC-38417)
- Preview fixes with `apply=false`, apply with `apply=true`

## HLS Integration
- Run `ghci_hls(action="available")` to check if HLS is installed
- Use `ghci_hls(action="hover", module_path="...", line=N, character=M)` for type info at position
- HLS resolution is host-first, then bundled, then auto-download. MCP handles installation automatically.
- If unavailable, provide the binary in host PATH or bundled toolchain and retry.
- For all compilation diagnostics: prefer `ghci_load(diagnostics=true)` — it doesn't require HLS
- Strict mode (`ghci_workflow(..., strict=true)`) keeps unavailable lint/format as blocking gates.

## Bundled Toolchain Maintenance
- Bundled binaries are tracked in `vendor-tools/bundled-tools-manifest.json`.
- Update checksums/version metadata with:
  `npm run tools:update-manifest -- --tool <name> --platform <platform> --arch <arch> --version <version> --provenance <url>`
- Maintain production-ready entries with real `sha256`, `version`, and `provenance`.
- When a bundled tool is unavailable, `ghci_lint` / `ghci_format` become recommended but not blocking in `_guidance`.
- `ghci_lint_basic` is a degraded fallback for hints only and does not satisfy lint gate completion.
- Run unit/integration/e2e test suites after updating bundled binaries or workflow policy.

## Coverage
- Use `cabal_coverage` near session close to collect structured HPC percentages.
- Treat coverage threshold checks as CI policy, not ad-hoc manual judgment.

## MCP Maintenance Policy
- Every MCP code change must carry unit, integration, and e2e coverage unless a stronger justification is documented.
- Keep `rules/`, embedded fallbacks in `src/resources/rules.ts`, tool descriptions, and workflow behavior aligned in the same change.
