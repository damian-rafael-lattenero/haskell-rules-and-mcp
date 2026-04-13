import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseTypeOutput } from "../parsers/type-parser.js";
import type { ToolContext } from "./registry.js";

export const typeCheckTool = {
  name: "ghci_type",
  description:
    "Get the type of a Haskell expression using GHCi's :t command. " +
    "Use this to verify types of subexpressions before composing them, " +
    "or to understand what type a function expects/returns.",
  inputSchema: {
    type: "object" as const,
    properties: {
      expression: {
        type: "string",
        description:
          'The Haskell expression to type-check. Examples: "map (+1)", "foldr", "Just . show"',
      },
    },
    required: ["expression"],
  },
};

export async function handleTypeCheck(
  session: GhciSession,
  args: { expression: string }
): Promise<string> {
  const result = await session.typeOf(args.expression);
  if (!result.success) {
    return JSON.stringify({
      success: false,
      error: result.output,
    });
  }

  // Detect deferred out-of-scope variables (when -fdefer-type-errors is active,
  // GHCi assigns type "p" instead of reporting an error)
  if (/deferred-out-of-scope-variables|Variable not in scope/.test(result.output)) {
    return JSON.stringify({
      success: false,
      error: result.output,
    });
  }

  const parsed = parseTypeOutput(result.output);
  if (parsed) {
    return JSON.stringify({
      success: true,
      expression: parsed.expression,
      type: parsed.type,
    });
  }

  return JSON.stringify({
    success: true,
    raw: result.output,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_type",
    "Get the type of a Haskell expression using GHCi :t. Use to verify types of subexpressions before composing them.",
    {
      expression: z.string().describe(
        'The Haskell expression to type-check. Examples: "map (+1)", "foldr", "Just . show"'
      ),
    },
    async ({ expression }) => {
      const session = await ctx.getSession();
      const result = await handleTypeCheck(session, { expression });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
