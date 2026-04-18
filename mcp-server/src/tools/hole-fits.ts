import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseTypedHoles } from "../parsers/hole-parser.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

// Re-export types for consumers
export type { HoleFit, RelevantBinding, TypedHole } from "../parsers/hole-parser.js";

/**
 * Load a module and extract structured information about all typed holes.
 */
export async function handleHoleFits(
  session: GhciSession,
  args: { module_path: string; max_fits?: number }
): Promise<string> {
  const maxFits = args.max_fits ?? 10;
  await session.execute(`:set -fmax-valid-hole-fits=${maxFits}`);

  const loadResult = await session.loadModule(args.module_path);

  await session.execute(":set -fmax-valid-hole-fits=6");

  const holes = parseTypedHoles(loadResult.output);

  if (holes.length === 0) {
    return JSON.stringify({
      success: true,
      holes: [],
      summary: "No typed holes found in module",
    });
  }

  return JSON.stringify({
    success: true,
    holes,
    summary: `Found ${holes.length} typed hole(s)`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_hole_fits",
    "Load a module containing typed holes (_) and return structured analysis of each hole: " +
      "expected type, relevant bindings in scope, and valid hole fits that GHC suggests. " +
      "Use when exploring what expressions could fill a gap in your code.",
    {
      module_path: z.string().describe(
        'Path to a module containing typed holes. Examples: "src/HM/Infer.hs"'
      ),
      max_fits: z.number().optional().describe(
        "Maximum number of valid hole fits to show per hole (default 10, GHC default is 6)"
      ),
    },
    async ({ module_path, max_fits }) => {
      const session = await ctx.getSession();
      const result = await handleHoleFits(session, { module_path, max_fits });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
