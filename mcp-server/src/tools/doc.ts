import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import type { ToolContext } from "./registry.js";

export async function handleDoc(
  session: GhciSession,
  args: { name: string }
): Promise<string> {
  const result = await session.docOf(args.name);
  if (!result.success) {
    return JSON.stringify({ success: false, error: result.output });
  }

  const output = result.output.trim();

  // GHCi returns specific messages when no docs are available
  if (
    output === "" ||
    output.includes("No documentation found") ||
    output.includes("has no documentation")
  ) {
    return JSON.stringify({
      success: true,
      name: args.name,
      documentation: null,
      message: `No documentation available for '${args.name}'`,
    });
  }

  return JSON.stringify({
    success: true,
    name: args.name,
    documentation: output,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_doc",
    "Get the Haddock documentation for a Haskell name. Returns the documentation string if available. " +
      "Requires the package to have been built with documentation. Works best for base library functions.",
    {
      name: z.string().describe(
        'The name to get documentation for. Examples: "map", "foldr", "Data.Map.fromList"'
      ),
    },
    async ({ name }) => {
      const session = await ctx.getSession();
      const result = await handleDoc(session, { name });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
