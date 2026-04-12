# Haskell Development Workflow

## Compilation Discipline
- Compile after every non-trivial edit. Use `ghci_load(load_all=true)` for multi-module checks, `ghci_load(module_path=...)` for single module.
- Never accumulate more than 5 unverified top-level definitions.
- For complex types (3+ type vars, constraints, higher-kinded): write signature with `= undefined`, compile, then implement.
- For simple functions: write and compile in batches.

## Type-First Development
- Write type signatures BEFORE implementations.
- Use `ghci_type` to verify subexpression types before composing them.
- Use `ghci_info` to understand typeclass hierarchies and available instances.
- Use `hoogle_search` to find functions by type signature.

## Typed Holes
When unsure what expression to write, use `_` (typed hole):
- `ghci_load` with diagnostics will show: expected type, relevant bindings, valid hole fits.
- `ghci_hole_fits` gives more detailed structured fits.
- Pick the most appropriate fit or use the type information to guide implementation.

## Error Recovery
When a compilation error persists after 2 fix attempts:
1. Replace the expression with `undefined`
2. Use `ghci_type` on the context to see expected type
3. Build the expression bottom-up from verified sub-expressions
4. Never rewrite large code sections speculatively — isolate the specific failing subexpression

## Module Hygiene
- New modules must be added to `exposed-modules` in `.cabal` before compiling
- Use explicit export lists
- Prefer qualified imports for library modules
