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
The goal is MCP-driven development. Every decision goes through an MCP tool.
Even if you already know the implementation — USE THE TOOLS FIRST.

## TOOL TIERS

### Tier 1 — Every function
\`ghci_load\` (after every edit) · \`ghci_type\` (verify types) · \`ghci_hole_fits\` (read holes)

### Tier 2 — Frequently
\`ghci_info\` · \`hoogle_search\` · \`ghci_eval\` · \`ghci_add_import\` · \`ghci_complete\`

### Tier 3 — Module complete gate (MANDATORY before next module)
\`ghci_quickcheck\` (MANDATORY — complete contract) · \`ghci_check_module\` · \`ghci_lint\` · \`ghci_format\`

## FLOW 4: Implement One Function (THE CORE LOOP)
1. HOLE: Replace = undefined with = _
2. COMPILE: ghci_load(diagnostics=true) → read hole type + fits
3. EXPLORE: ghci_type / ghci_info / hoogle_search
4. IMPLEMENT: Write the body (max ~20 lines)
5. COMPILE: ghci_load(diagnostics=true)
6. FIX: errors → fix → recompile | warnings → fix ALL → recompile
7. VERIFY: ghci_type("functionName")
8. TEST: ghci_eval("functionName sampleArg")

Steps 1-2 and 7 are MANDATORY. Never skip them.

## FLOW 6: Module Complete (MANDATORY before next module)
1. ghci_quickcheck — test the COMPLETE algebraic contract. CANNOT skip this.
2. ghci_check_module → review API
3. ghci_lint / ghci_format

## FORBIDDEN
- Implementation without hole phase (steps 1-2)
- Skipping ghci_type after implementation (step 7)
- Moving to next module without running ghci_quickcheck
- Multiple .hs edits between ghci_load calls
- Using Bash for any Haskell toolchain operation
- MCP tool fails → falling back to Bash
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
      "The complete MCP-driven Haskell development workflow: tool tiers (1-4), " +
      "8 development flows, error resolution, warning auto-fix, and forbidden patterns. " +
      "This is the single source of truth — injected via MCP server instructions.",
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
