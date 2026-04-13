import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { parseBrowseOutput, inferModuleName } from "../parsers/browse-parser.js";
import type { ToolContext } from "./registry.js";

// Re-export for consumers
export type { ModuleDefinition } from "../parsers/browse-parser.js";

/**
 * Load a module and return a structured summary of its exports:
 * all definitions with their types, plus any compilation errors.
 */
export async function handleCheckModule(
  session: GhciSession,
  args: { module_path: string; module_name?: string }
): Promise<string> {
  // Disable -fdefer-type-errors so real type errors show up
  await session.execute(":set -fno-defer-type-errors");

  const loadResult = await session.loadModule(args.module_path);
  const errors = parseGhcErrors(loadResult.output);
  const compileErrors = errors.filter((e) => e.severity === "error");
  const warnings = errors.filter((e) => e.severity === "warning");

  if (compileErrors.length > 0) {
    await session.execute(":set -fdefer-type-errors");
    return JSON.stringify({
      success: false,
      compiled: false,
      errors: compileErrors,
      warnings,
      definitions: [],
      summary: `Module failed to compile: ${compileErrors.length} error(s)`,
    });
  }

  const moduleName = args.module_name ?? inferModuleName(args.module_path);
  const browseResult = await session.execute(`:browse ${moduleName}`);
  const definitions = parseBrowseOutput(browseResult.output);

  const functions = definitions.filter((d) => d.kind === "function");
  const types = definitions.filter((d) => d.kind === "type" || d.kind === "data");
  const classes = definitions.filter((d) => d.kind === "class");

  await session.execute(":set -fdefer-type-errors");

  return JSON.stringify({
    success: true,
    compiled: true,
    errors: [],
    warnings,
    definitions,
    summary: {
      total: definitions.length,
      functions: functions.length,
      types: types.length,
      classes: classes.length,
      warnings: warnings.length,
    },
    module: moduleName,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_check_module",
    "Load a module and return a structured summary of all its exported definitions with types. " +
      "Shows: total definitions, functions with signatures, type aliases, data types, classes, " +
      "and any compilation errors or warnings. Use for a quick overview of a module's API.",
    {
      module_path: z.string().describe(
        'Path to the module to check. Examples: "src/HM/Infer.hs", "src/Lib.hs"'
      ),
      module_name: z.string().optional().describe(
        'Haskell module name (optional, inferred from path). Examples: "HM.Infer", "Lib"'
      ),
    },
    async ({ module_path, module_name }) => {
      const session = await ctx.getSession();
      const result = await handleCheckModule(session, { module_path, module_name });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
