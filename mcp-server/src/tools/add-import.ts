import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { handleHoogleSearch } from "./hoogle.js";
import type { ToolContext } from "./registry.js";

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

  // Prefer base/containers/mtl modules over obscure packages
  const preferred = ["base", "containers", "mtl", "text", "bytestring", "transformers"];
  suggestions.sort((a, b) => {
    const aIdx = preferred.indexOf(a.package);
    const bIdx = preferred.indexOf(b.package);
    const aScore = aIdx >= 0 ? aIdx : preferred.length;
    const bScore = bIdx >= 0 ? bIdx : preferred.length;
    return aScore - bScore;
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

export function register(server: McpServer, _ctx: ToolContext): void {
  server.tool(
    "ghci_add_import",
    "Suggest an import line for a Haskell name that is 'Not in scope'. " +
      "Uses Hoogle to find which module the name lives in, then returns the import line to add. " +
      "Does NOT edit files — the import line is returned for the agent to apply.",
    {
      name: z.string().describe(
        'The identifier that is not in scope. Examples: "sort", "Map.fromList", "liftIO"'
      ),
      qualified: z.boolean().optional().describe(
        "If true, suggest a qualified import. Default: false (import with explicit name)."
      ),
    },
    async ({ name, qualified }) => {
      const result = await handleAddImport({ name, qualified });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
