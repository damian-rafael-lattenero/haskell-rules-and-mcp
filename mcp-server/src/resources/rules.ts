import { readFile } from "node:fs/promises";
import path from "node:path";

export interface RuleDefinition {
  name: string;
  uri: string;
  title: string;
  description: string;
  fileName: string;
  embeddedContent: string;
}

const RULES_DIR = path.resolve(import.meta.dirname, "../../rules");

// Embedded fallback content — used when rules files are not found on disk.
// These are minimal versions; the full versions live in mcp-server/rules/*.md.

const WORKFLOW_FALLBACK = `# Haskell MCP Workflow

## PRIME DIRECTIVE
MCP-driven development. Every decision goes through an MCP tool.
The compiler's structured output drives development, not pre-existing knowledge.

## CONTEXTUAL GUIDANCE
The MCP provides automatic \`_guidance\` in tool responses based on module state.
Lost? Not sure what to do next? → ghci_workflow(action="help") for contextual guidance.

## ALWAYS MANDATORY
- ghci_load after every .hs edit — no exceptions
- ghci_quickcheck incrementally when laws become testable AND at module-complete
- Zero tolerance for warnings — fix every warningAction immediately
- ghci_arbitrary for new data types — don't write Arbitrary instances by hand
- ghci_regression(action="run") at session start on existing projects
- Follow _guidance in tool responses

## TOOLCHAIN
- Resolution order: host PATH -> bundled binary
- ghci_lint / ghci_format / ghci_hls report source, binaryPath, and version when available
- If ghci_lint / ghci_format are unavailable, guidance downgrades them to recommended but not blocking

## WHEN → TOOL → WHY

### Session startup
| When | Tool | Why |
|------|------|-----|
| Start of session | ghci_session(status) | Verify MCP is alive |
| Lost / unsure what to do | ghci_workflow(action="help") | Context-aware next steps |
| Clean obsolete properties | ghci_property_lifecycle(action="list") | See all saved properties |
| Remove old property | ghci_property_lifecycle(action="remove", property="...") | Delete from store |
| Deprecate property | ghci_property_lifecycle(action="deprecate", property="...", reason="...") | Mark as deprecated (filters from exports) |
| Replace property | ghci_property_lifecycle(action="replace", property="old", replaced_by="new") | Link old to new version |

### Project / dependency management
| When | Tool | Why |
|------|------|-----|
| Start a new project | ghci_init(name, modules, deps) | Creates a project with containers + QuickCheck defaults |
| Need to add a dependency | ghci_deps(action="add", package="name") | Edits .cabal — never edit by hand |
| Remove a dependency | ghci_deps(action="remove", package="name") | Safe removal (protects base) |
| List current deps | ghci_deps(action="list") | See all packages with versions |
| See module import graph | ghci_deps(action="graph") | Detects cycles and orphan modules |
| After add/remove dep | ghci_session(restart) | Pick up the change in GHCi |
| New project with Stack | ghci_init(name, modules, deps, build_tool="stack") | Also generates stack.yaml |

### Implementing functions
| When | Tool | Why |
|------|------|-----|
| Wrote/edited a function | ghci_load(diagnostics=true) | Compile, see errors + importSuggestions |
| Module has typed holes | ghci_hole(module_path="src/X.hs") | Expected type + valid fits |
| After compilation | ghci_eval("funcName arg") | Test behavior |
| A law becomes testable | ghci_quickcheck(property, module_path="src/X.hs") | Test immediately |
| Testing roundtrip property | ghci_quickcheck(roundtrip="pretty,parse,normalize") | Auto-generate roundtrip test |
| QuickCheck failed with counterexample | ghci_trace(...) | Follow trace-first debugging guidance |
| Debugging parser failures | ghci_trace(expression="parse input", parser_mode=true) | Get call tree + backtracking analysis |
| All functions done | ghci_quickcheck_batch | Complete contract |
| Apply suggested export list | ghci_apply_exports(module_path="src/X.hs") | Materialize ghci_check_module suggestions |
| Smoke-test parser robustness | ghci_fuzz_parser(parser="...") | Detect malformed-input crashes |
| Need to rename a binding | ghci_refactor(action="rename_local", module_path="...", old_name="foo", new_name="bar") | Word-boundary safe rename |
| Extract code to function | ghci_refactor(action="extract_binding", module_path="...", new_name="helper", lines=[5,8]) | Lift lines to top-level |
| Enable GHC extension | ghci_flags(action="set", flags="-XOverloadedStrings") | Session-only flag |
| See active flags | ghci_flags(action="list") | Language settings |

### Module complete gate
| When | Tool | Why |
|------|------|-----|
| Session on existing project | ghci_regression(action="run") | Re-run saved properties |
| Before next module | ghci_check_module, ghci_lint, ghci_format(write=true) | Quality gate; lint/format become recommended if unavailable |

### Session close
| When | Tool | Why |
|------|------|-----|
| After all modules pass | ghci_quickcheck_export(output_path="test/Spec.hs") | Generate persistent test suite (auto-filters deprecated properties) |
| After export | cabal_test | Validate exported tests actually run |
| After tests | cabal_build | Verify full package compilation |

### Performance analysis
| When | Tool | Why |
|------|------|-----|
| Code seems slow, quick hints | ghci_profile(action="suggest", module_path="src/X.hs") | Static heuristic analysis |
| GHC time profiling | ghci_profile(action="time") | Top cost centres |

### HLS integration
| When | Tool | Why |
|------|------|-----|
| Check if HLS installed | ghci_hls(action="available") | Returns { available: bool } from host/bundled resolution |
| Type info at position | ghci_hls(action="hover", module_path="...", line=5, character=3) | LSP hover (requires HLS) |

## PARAMETER NOTES
- ghci_quickcheck: use module_path="src/X.hs" (preferred) or module="src/X.hs" (also works)
- ghci_quickcheck_export validates with cabal_test by default
- ghci_format write=true: requires fourmolu/ormolu from host or bundled toolchain
- ghci_flags: session-only; persist with default-extensions in .cabal
- ghci_refactor / ghci_apply_exports: run ghci_load after to verify compilation

## FORBIDDEN
- Multiple .hs edits between ghci_load calls
- Using Bash for Haskell toolchain operations
- Moving to next module without ghci_quickcheck
- Writing Arbitrary instances by hand
- Manually editing .cabal for dependencies — use ghci_deps instead
`;

const CONVENTIONS_FALLBACK = `# Haskell Project Conventions

## Toolchain
- host PATH -> bundled binary for hlint / fourmolu / ormolu / hls
- ghci_init includes containers + QuickCheck defaults
- update rules/docs/tests together when workflow behavior changes

## Import Style
- Qualified imports for Map/Set: \`import qualified Data.Map.Strict as Map\`
- Explicit import lists for application modules
- Cross-module imports auto-generated by ghci_scaffold

## Module Structure
- One type or concern per module
- Explicit export lists
- Modules match directory structure

## Dependencies
- NEVER edit .cabal build-depends manually for adding/removing packages
- Use ghci_deps(action="add"/"remove"/"list") instead
- After any dep change: ghci_session(restart) to reload

## Testing
- QuickCheck for property-based testing
- Test algebraic laws: associativity, identity, roundtrip
- Pass module_path="src/X.hs" to ghci_quickcheck for accurate tracking (module= also accepted)
- Use ghci_regression to re-run saved properties
- Use ghci_hole(module_path="...") to explore typed holes before implementing
- Use ghci_trace when QuickCheck returns a counterexample
- Use ghci_fuzz_parser(parser="...") for malformed-input parser checks
- ghci_quickcheck_export validates with cabal_test by default and auto-filters deprecated properties
- Use ghci_property_lifecycle to manage property lifecycle (list/remove/deprecate/replace)
- Deprecate old properties instead of deleting to maintain audit trail

## MCP Maintenance
- Every MCP code change should include unit, integration, and e2e coverage
- Keep rules/, embedded fallbacks, tool descriptions, and workflow behavior aligned

## Refactoring
- Use ghci_refactor(action="rename_local") to rename bindings — never manual find/replace
- Always run ghci_load after refactoring to verify compilation

## Performance
- Use ghci_profile(action="suggest") for quick static analysis before optimizing
- Check for: String (++) in loops, naive recursion without accumulator, partial functions
`;

export const RULES_REGISTRY: RuleDefinition[] = [
  {
    name: "haskell-mcp-workflow",
    uri: "rules://haskell/mcp-workflow",
    title: "Haskell MCP Workflow — Flows, Tool Tiers, and Development Protocol",
    description:
      "MCP-driven Haskell development workflow: contextual guidance, mandatory tools, " +
      "when/tool/why tables, error resolution, warning auto-fix, and forbidden patterns.",
    fileName: "haskell-mcp-workflow.md",
    embeddedContent: WORKFLOW_FALLBACK,
  },
  {
    name: "haskell-project-conventions",
    uri: "rules://haskell/project-conventions",
    title: "Haskell Project Conventions",
    description:
      "Common conventions for Haskell projects: import style, module structure, naming, testing.",
    fileName: "haskell-project-conventions.md",
    embeddedContent: CONVENTIONS_FALLBACK,
  },
  {
    name: "session-management",
    uri: "rules://haskell/session-management",
    title: "GHCi Session Management",
    description:
      "GHCi session lifecycle, health monitoring, timeout behavior, and recovery strategies.",
    fileName: "session-management.md",
    embeddedContent: "# GHCi Session Management\n\nSee rules/session-management.md for full documentation.",
  },
];

/**
 * Load a rule from disk, falling back to embedded content.
 */
export async function loadRule(rule: RuleDefinition): Promise<string> {
  try {
    return await readFile(path.join(RULES_DIR, rule.fileName), "utf-8");
  } catch {
    return rule.embeddedContent;
  }
}
