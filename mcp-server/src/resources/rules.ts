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

const AUTOMATION_FALLBACK = `# Automated Development Loop

## Primary Protocol
After every edit, run \`ghci_load\`. Read the structured output. Take the FIRST applicable action:
1. If \`errors\` > 0 → apply Error Resolution
2. If \`warningActions\` > 0 → fix each one automatically
3. If success with 0 issues → move to next task

**NEVER ask the developer "should I fix this warning?" — just fix it.**

## Warning Action Table
| warningFlag | Action |
|---|---|
| -Wunused-imports | Remove the import line. If partially used, narrow to used names. |
| -Wunused-matches | Replace binding with \`_\` wildcard. |
| -Wincomplete-patterns | Add missing pattern cases. Use \`ghci_info\` to see all constructors. |
| -Wmissing-signatures | Add the inferred type from \`suggestedAction\` as a signature. |
| -Wtyped-holes | Read hole fits. Pick the best one or implement. |

## Error Resolution
| Code | Action |
|---|---|
| GHC-83865 | Type mismatch: read expected/actual. |
| GHC-39999 | Not in scope: use \`ghci_add_import\` to find the module, or \`ghci_complete\` for typos. |
| GHC-39660 | No instance: add deriving, constraint, or import. |

## The Loop
\`\`\`
edit → ghci_load → errors? fix → warnings? fix → clean? → ghci_quickcheck → done
\`\`\`
`;

const DEVELOPMENT_FALLBACK = `# Haskell Development Workflow

## Compilation Discipline
- Compile after every non-trivial edit
- For complex types: write signature with \`= undefined\`, compile, then implement

## Type-First Development
- Write type signatures BEFORE implementations
- Use \`ghci_type\` to verify subexpression types
- Use \`hoogle_search\` to find functions by type signature

## Typed Holes
Use \`_\` (typed hole) when unsure what expression to write.
\`ghci_hole_fits\` gives structured fits with types.

## Navigation & Discovery
- \`ghci_goto\`: jump to definition (file:line for local, module for library)
- \`ghci_complete\`: find functions matching a prefix
- \`ghci_doc\`: read Haddock documentation
- \`ghci_imports\`: see what's in scope
- \`ghci_add_import\`: find the right module for an out-of-scope name

## Code Quality
- \`ghci_format\`: format with ormolu/fourmolu (if installed)
- \`ghci_lint\`: hlint suggestions (if installed)

## Module Hygiene
- Add modules to \`exposed-modules\` in \`.cabal\` before compiling
- Use explicit export lists
- Prefer qualified imports for library modules
`;

const MCP_FIRST_FALLBACK = `# MCP-First: All Haskell Operations Go Through the MCP Server

## Mandatory Rule
For **ALL** Haskell operations in this project, use the \`haskell-ghci\` MCP tools.
**NEVER** run \`cabal\`, \`ghc\`, \`ghci\`, \`stack\`, or any Haskell toolchain command directly via Bash.

## Tool Mapping
| Operation | MCP Tool | NOT this |
|---|---|---|
| Create project scaffolding | \`ghci_scaffold\` | ~~manual file creation + Bash cabal~~ |
| Switch between playground projects | \`ghci_switch_project\` | ~~cd + Bash cabal~~ |
| Build the project | \`cabal_build\` | ~~Bash: cabal build~~ |
| Load/reload modules | \`ghci_load\` | ~~Bash: ghci, cabal repl~~ |
| Evaluate expressions | \`ghci_eval\` | ~~Bash: ghci -e~~ |
| Type-check expressions | \`ghci_type\` | ~~Bash: ghci :t~~ |
| Get info on names | \`ghci_info\` | ~~Bash: ghci :i~~ |
| Find definitions | \`ghci_goto\` | ~~Bash: ghci :i~~ |
| Search by type signature | \`hoogle_search\` | ~~Bash: hoogle~~ |
| Find references | \`ghci_references\` | ~~grep~~ |
| Rename across project | \`ghci_rename\` | ~~sed/find-replace~~ |
| Format code | \`ghci_format\` | ~~Bash: ormolu/fourmolu~~ |
| Lint code | \`ghci_lint\` | ~~Bash: hlint~~ |
| Run QuickCheck properties | \`ghci_quickcheck\` | ~~Bash: cabal test~~ |
| Restart GHCi session | \`ghci_session(action="restart")\` | ~~kill process + Bash ghci~~ |
| Restart MCP server | \`mcp_restart\` | ~~manual restart~~ |

## Project Bootstrap Sequence
1. **New project**: Edit the .cabal file, then \`ghci_scaffold\`, then \`ghci_session(action="restart")\`, then \`ghci_load(load_all=true)\`.
2. **Existing project**: \`ghci_switch_project(project="name")\`, then \`ghci_load(load_all=true)\`.
3. **List available projects**: \`ghci_switch_project()\` with no arguments.
`;

const CONVENTIONS_FALLBACK = `# Haskell Project Conventions

## Import Style
- Qualified imports for Map/Set: \`import Data.Map.Strict qualified as Map\`
- Explicit import lists for application modules

## Module Structure
- One type or concern per module
- Explicit export lists
- Modules match directory structure

## Testing
- QuickCheck for property-based testing
- Test algebraic laws: associativity, identity, roundtrip
`;

export const RULES_REGISTRY: RuleDefinition[] = [
  {
    name: "haskell-mcp-first",
    uri: "rules://haskell/mcp-first",
    title: "MCP-First: All Haskell Operations Go Through the MCP Server",
    description:
      "MANDATORY: Use haskell-ghci MCP tools for ALL Haskell operations. Never use Bash for cabal/ghc/ghci. " +
      "Includes tool mapping table and project bootstrap sequence.",
    fileName: "haskell-mcp-first.md",
    embeddedContent: MCP_FIRST_FALLBACK,
  },
  {
    name: "haskell-automation",
    uri: "rules://haskell/automation",
    title: "Haskell Automated Development Loop",
    description:
      "Rules for the edit-compile-fix loop: warning action table, error resolution table, QuickCheck integration.",
    fileName: "haskell-automation.md",
    embeddedContent: AUTOMATION_FALLBACK,
  },
  {
    name: "haskell-development",
    uri: "rules://haskell/development",
    title: "Haskell Development Workflow",
    description:
      "Rules for type-first development, compilation discipline, typed holes, and error recovery.",
    fileName: "haskell-development.md",
    embeddedContent: DEVELOPMENT_FALLBACK,
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
