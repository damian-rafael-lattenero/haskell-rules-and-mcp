import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { type ToolContext, registerStrictTool } from "./registry.js";
import { handleCheckModule } from "./check-module.js";

export function replaceModuleHeaderWithExportList(
  source: string,
  suggestedHeader: string
): { updated: string; replaced: boolean } {
  const lines = source.split("\n");
  const moduleStart = lines.findIndex((line) => line.trimStart().startsWith("module "));
  if (moduleStart === -1) {
    return { updated: source, replaced: false };
  }

  let depth = 0;
  let moduleEnd = moduleStart;
  for (let i = moduleStart; i < lines.length; i++) {
    const line = lines[i]!;
    for (const ch of line) {
      if (ch === "(") depth++;
      if (ch === ")") depth = Math.max(0, depth - 1);
    }
    moduleEnd = i;
    if (line.includes("where") && depth === 0) break;
  }

  const updatedLines = [
    ...lines.slice(0, moduleStart),
    ...suggestedHeader.split("\n"),
    ...lines.slice(moduleEnd + 1),
  ];
  return { updated: updatedLines.join("\n"), replaced: true };
}

export async function handleApplyExports(
  projectDir: string,
  args: { module_path: string; module_name?: string; suggested_export_list?: string }
): Promise<string> {
  const absPath = path.resolve(projectDir, args.module_path);
  const original = await readFile(absPath, "utf8");

  let suggested = args.suggested_export_list;
  if (!suggested) {
    throw new Error("suggested_export_list is required when handleApplyExports is called directly");
  }

  const { updated, replaced } = replaceModuleHeaderWithExportList(original, suggested);
  if (!replaced) {
    return JSON.stringify({
      success: false,
      error: `Could not find a module header in ${args.module_path}`,
    });
  }

  await writeFile(absPath, updated, "utf8");
  return JSON.stringify({
    success: true,
    module_path: args.module_path,
    module_name: args.module_name,
    applied: true,
    exportList: suggested,
    _nextStep: `Export list applied to ${args.module_path}. Run ghci_load(diagnostics=true) to verify the module still compiles.`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_apply_exports",
    "Apply the suggested explicit export list for a module. " +
      "If suggested_export_list is omitted, the tool first runs ghci_check_module to compute it, " +
      "then rewrites the module header in place.",
    {
      module_path: z.string().describe('Path to the module to rewrite. Example: "src/MyModule.hs"'),
      module_name: z.string().optional().describe(
        'Optional Haskell module name. Example: "Expr.Eval". Inferred from ghci_check_module when omitted.'
      ),
      suggested_export_list: z.string().optional().describe(
        "Optional explicit module header to apply. If omitted, ghci_check_module is run first."
      ),
    },
    async ({ module_path, module_name, suggested_export_list }) => {
      let suggested = suggested_export_list;
      let inferredName = module_name;

      if (!suggested) {
        const session = await ctx.getSession();
        const checked = JSON.parse(
          await handleCheckModule(session, { module_path, module_name })
        ) as {
          success?: boolean;
          module?: string;
          suggestedExportList?: string;
          error?: string;
        };
        if (checked.success === false) {
          return {
            content: [{
              type: "text" as const,
              text: JSON.stringify({
                success: false,
                error: checked.error ?? `Could not derive exports for ${module_path}`,
              }),
            }],
          };
        }
        suggested = checked.suggestedExportList;
        inferredName = checked.module ?? module_name;
      }

      if (!suggested) {
        return {
          content: [{
            type: "text" as const,
            text: JSON.stringify({
              success: false,
              error: `ghci_check_module did not return a suggested export list for ${module_path}`,
            }),
          }],
        };
      }

      const result = await handleApplyExports(ctx.getProjectDir(), {
        module_path,
        module_name: inferredName,
        suggested_export_list: suggested,
      });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
