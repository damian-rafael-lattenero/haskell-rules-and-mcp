import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseCompletionOutput } from "../parsers/completion-parser.js";
import type { ToolContext } from "./registry.js";

export async function handleComplete(
  session: GhciSession,
  args: { prefix: string }
): Promise<string> {
  const result = await session.completionsOf(args.prefix);
  if (!result.success) {
    return JSON.stringify({ success: false, error: result.output });
  }

  const parsed = parseCompletionOutput(result.output);
  return JSON.stringify({
    success: true,
    ...parsed,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_complete",
    "Get completions for a Haskell identifier prefix. Returns all in-scope names matching the prefix. " +
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
