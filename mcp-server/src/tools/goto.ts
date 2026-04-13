import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseInfoOutput } from "../parsers/type-parser.js";
import type { ToolContext } from "./registry.js";

/**
 * Extract definition location from :i output.
 * Patterns: "Defined at file:line:col" or "Defined in 'Module'"
 */
export function parseDefinitionLocation(output: string): {
  file?: string;
  line?: number;
  column?: number;
  module?: string;
} | null {
  const atMatch = output.match(/Defined at (.+?):(\d+):(\d+)/);
  if (atMatch) {
    return {
      file: atMatch[1]!,
      line: parseInt(atMatch[2]!, 10),
      column: parseInt(atMatch[3]!, 10),
    };
  }
  const inMatch = output.match(/Defined in ['\u2018](.+?)['\u2019]/);
  if (inMatch) {
    return { module: inMatch[1]! };
  }
  return null;
}

export async function handleGoto(
  session: GhciSession,
  args: { name: string }
): Promise<string> {
  const result = await session.infoOf(args.name);
  if (!result.success) {
    return JSON.stringify({ success: false, error: result.output });
  }

  if (/deferred-out-of-scope-variables|Variable not in scope|Not in scope/.test(result.output)) {
    return JSON.stringify({ success: false, error: result.output });
  }

  const info = parseInfoOutput(result.output);
  const location = parseDefinitionLocation(result.output);

  return JSON.stringify({
    success: true,
    name: info.name,
    kind: info.kind,
    location: location ?? undefined,
    definition: info.definition,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_goto",
    "Go to the definition of a Haskell name. Returns the file path, line, and column where the name is defined. " +
      "For local project definitions, returns the source file location. For library functions, returns the module name.",
    {
      name: z.string().describe(
        'The name to find the definition of. Examples: "map", "MyType", "myFunction"'
      ),
    },
    async ({ name }) => {
      const session = await ctx.getSession();
      const result = await handleGoto(session, { name });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
