import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { handleHoogleSearch } from "./hoogle.js";
import { type ToolContext, registerStrictTool, zBool } from "./registry.js";

interface ImportSuggestion {
  module: string;
  importLine: string;
  package: string;
}

/**
 * Suggest an import for a name that is "Not in scope".
 * Uses Hoogle to find which module the name lives in.
 * Does NOT edit files — returns the import line for the LLM agent to apply.
 */
export async function handleAddImport(args: {
  name: string;
  qualified?: boolean;
  projectDir?: string;
}): Promise<string> {
  // Search Hoogle for the name
  const hoogleResult = JSON.parse(await handleHoogleSearch({ query: args.name, count: 10 }));

  if (!hoogleResult.success || hoogleResult.results.length === 0) {
    return JSON.stringify({
      success: false,
      name: args.name,
      error: `No Hoogle results found for '${args.name}'`,
    });
  }

  // Read project dependencies to rank results (prefer project deps over random packages)
  let projectDeps: Set<string> | null = null;
  if (args.projectDir) {
    try {
      const { extractBuildDepends } = await import("../parsers/cabal-parser.js");
      const deps = await extractBuildDepends(args.projectDir);
      projectDeps = new Set(deps);
    } catch {
      // No .cabal or no deps — use generic ranking
    }
  }

  // Deduplicate and rank modules
  const seen = new Set<string>();
  const suggestions: ImportSuggestion[] = [];

  for (const r of hoogleResult.results) {
    const mod = r.module;
    if (!mod || seen.has(mod)) continue;
    seen.add(mod);

    const importLine = args.qualified
      ? `import ${mod} qualified`
      : `import ${mod} (${args.name})`;

    suggestions.push({
      module: mod,
      importLine,
      package: r.package ?? "unknown",
    });
  }

  // Rank by project dependencies first, then by Hoogle order
  suggestions.sort((a, b) => {
    const aPkg = a.package.replace(/-[0-9].*$/, "");
    const bPkg = b.package.replace(/-[0-9].*$/, "");
    const aInProject = projectDeps?.has(aPkg) ? 0 : 1;
    const bInProject = projectDeps?.has(bPkg) ? 0 : 1;
    return aInProject - bInProject;
  });

  const best = suggestions[0];
  if (!best) {
    return JSON.stringify({
      success: false,
      name: args.name,
      error: `Could not determine module for '${args.name}'`,
    });
  }

  return JSON.stringify({
    success: true,
    name: args.name,
    suggestedImport: best.importLine,
    module: best.module,
    package: best.package,
    alternatives: suggestions.slice(1, 5).map((s) => ({
      module: s.module,
      importLine: s.importLine,
      package: s.package,
    })),
  });
}

/**
 * Look up the best import for a single name. Returns null if Hoogle is unavailable
 * or no results found. Designed for inline use in ghci_load import suggestions.
 * When projectDir is provided, results are ranked by project dependencies.
 */
export async function lookupImportForName(
  name: string,
  projectDir?: string
): Promise<{ name: string; import: string; module: string } | null> {
  try {
    const result = JSON.parse(await handleAddImport({ name, projectDir }));
    if (!result.success) return null;
    return {
      name,
      import: result.suggestedImport,
      module: result.module,
    };
  } catch {
    return null;
  }
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_add_import",
    "Suggest an import line for a Haskell name that is 'Not in scope'. " +
      "Uses Hoogle to find which module the name lives in, ranked by project dependencies. " +
      "Does NOT edit files — the import line is returned for the agent to apply.",
    {
      name: z.string().describe(
        'The identifier that is not in scope. Examples: "sort", "Map.fromList", "liftIO"'
      ),
      qualified: zBool().optional().describe(
        "If true, suggest a qualified import. Default: false (import with explicit name)."
      ),
    },
    async ({ name, qualified }) => {
      const result = await handleAddImport({ name, qualified, projectDir: ctx.getProjectDir() });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
