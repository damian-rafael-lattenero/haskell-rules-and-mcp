import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseCompletionOutput } from "../parsers/completion-parser.js";
import { handleHoogleSearch } from "./hoogle.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

export async function handleComplete(
  session: GhciSession,
  args: { prefix: string }
): Promise<string> {
  const result = await session.completionsOf(args.prefix);
  if (!result.success) {
    return JSON.stringify({ success: false, error: result.output });
  }

  const parsed = parseCompletionOutput(result.output);

  // If no completions found and prefix is reasonable, try Hoogle as fallback
  if (parsed.total === 0 && args.prefix.length >= 2) {
    try {
      const hoogleResult = JSON.parse(await handleHoogleSearch({ query: args.prefix, count: 10 }));
      if (hoogleResult.success && hoogleResult.results?.length > 0) {
        const hoogleCompletions = hoogleResult.results.map((r: { name: string; module: string; package: string }) => ({
          name: r.name,
          module: r.module,
          package: r.package,
        }));
        return JSON.stringify({
          success: true,
          ...parsed,
          hoogleFallback: hoogleCompletions,
          hint: `No in-scope completions for '${args.prefix}'. Showing Hoogle results — you may need to add an import.`,
        });
      }
    } catch {
      // Hoogle fallback is best-effort
    }
  }

  return JSON.stringify({
    success: true,
    ...parsed,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_complete",
    "Get completions for a Haskell identifier prefix. Returns all in-scope names matching the prefix. " +
      "Falls back to Hoogle search when no in-scope completions found. " +
      "Useful for discovering available functions, types, and modules.",
    {
      prefix: z.string().describe(
        'The prefix to complete. Examples: "ma" (finds map, mapM, max), "Data.Map." (finds Data.Map.fromList, etc.)'
      ),
    },
    async ({ prefix }) => {
      const session = await ctx.getSession();
      const result = await handleComplete(session, { prefix });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
