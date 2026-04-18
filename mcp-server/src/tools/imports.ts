import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { GhciSession } from "../ghci-session.js";
import { parseImportsOutput } from "../parsers/import-parser.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

export async function handleImports(
  session: GhciSession
): Promise<string> {
  const result = await session.showImports();
  if (!result.success) {
    return JSON.stringify({ success: false, error: result.output });
  }

  const imports = parseImportsOutput(result.output);
  return JSON.stringify({
    success: true,
    imports,
    count: imports.length,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_imports",
    "Show all currently loaded imports in the GHCi session. Returns structured information about each import: " +
      "module name, whether it's qualified, alias, specific items imported, and whether it's implicit.",
    {},
    async () => {
      const session = await ctx.getSession();
      const result = await handleImports(session);
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
