import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { handleLoadModule } from "./load-module.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

/**
 * @deprecated Use ghci_load with diagnostics=true instead.
 * This tool now delegates to the enhanced ghci_load.
 */
export async function handleDiagnostics(
  session: GhciSession,
  args: { module_path: string },
  projectDir?: string
): Promise<string> {
  return handleLoadModule(
    session,
    { module_path: args.module_path, diagnostics: true },
    projectDir
  );
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_diagnostics",
    "Full diagnostic check for a Haskell module. Runs a strict compilation pass to find real type errors, " +
      "then a deferred pass to collect typed-hole information. Returns a unified report.",
    {
      module_path: z.string().describe(
        'Path to the module to diagnose. Examples: "src/HM/Infer.hs", "src/Lib.hs"'
      ),
    },
    async ({ module_path }) => {
      const session = await ctx.getSession();
      const result = await handleDiagnostics(session, { module_path }, ctx.getProjectDir());
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
