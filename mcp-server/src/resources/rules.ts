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

## ALWAYS MANDATORY (all modes)
- ghci_load after every .hs edit — no exceptions
- ghci_quickcheck incrementally when laws become testable AND at module-complete
- Zero tolerance for warnings
- ghci_arbitrary for new data types

## WHEN → TOOL → WHY
| When | Tool | Why |
|------|------|-----|
| Wrote/edited a function | ghci_load(diagnostics=true) | Compile, see errors |
| After compilation | ghci_eval("funcName arg") | Test behavior |
| A law becomes testable | ghci_quickcheck(incremental=true) | Test immediately |
| All functions done | ghci_quickcheck / ghci_quickcheck_batch | Complete contract |
| Before next module | ghci_check_module, ghci_lint, ghci_format | Quality gate |

## FORBIDDEN
- Multiple .hs edits between ghci_load calls
- Using Bash for Haskell toolchain operations
- Moving to next module without ghci_quickcheck
- Writing Arbitrary instances by hand
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
    name: "haskell-mcp-workflow",
    uri: "rules://haskell/mcp-workflow",
    title: "Haskell MCP Workflow — Flows, Tool Tiers, and Development Protocol",
    description:
      "MCP-driven Haskell development workflow: mode selection, mandatory tools, " +
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
