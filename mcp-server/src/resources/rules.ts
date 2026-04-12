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

## Module Hygiene
- Add modules to \`exposed-modules\` in \`.cabal\` before compiling
- Use explicit export lists
- Prefer qualified imports for library modules
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
