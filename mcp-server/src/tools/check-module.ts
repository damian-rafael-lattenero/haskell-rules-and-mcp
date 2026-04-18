import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { GhciSession } from "../ghci-session.js";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { parseBrowseOutput, inferModuleName } from "../parsers/browse-parser.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

// Re-export for consumers
export type { ModuleDefinition } from "../parsers/browse-parser.js";

/**
 * Detect whether a Haskell source has an explicit export list.
 * Matches `module Name (exports) where`, allowing multiline lists.
 */
function hasExplicitExportList(source: string): boolean {
  const stripped = source.replace(/--[^\n]*/g, "").replace(/\{-[\s\S]*?-\}/g, "");
  return /^\s*module\s+\S+\s*\([\s\S]*?\)\s+where/m.test(stripped);
}

/**
 * Load a module and return a structured summary of its exports:
 * all definitions with their types, plus any compilation errors.
 */
export async function handleCheckModule(
  session: GhciSession,
  args: { module_path: string; module_name?: string },
  projectDir?: string
): Promise<string> {
  // Disable -fdefer-type-errors so real type errors show up
  await session.execute(":set -fno-defer-type-errors");

  const loadResult = await session.loadModule(args.module_path);
  const errors = parseGhcErrors(loadResult.output);
  const compileErrors = errors.filter((e) => e.severity === "error");
  // GHC-32850 (-Wmissing-home-modules) is a GHCi session artifact, not a real
  // code warning.  It fires when GHCi is started for a single module rather
  // than the full package.  Suppress it so it doesn't confuse the LLM into
  // thinking the module has a real warning to fix.
  const warnings = errors.filter(
    (e) => e.severity === "warning" && e.code !== "GHC-32850"
  );

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

  let hasExports = false;
  if (projectDir) {
    try {
      const source = await readFile(path.resolve(projectDir, args.module_path), "utf-8");
      hasExports = hasExplicitExportList(source);
    } catch {
      // Unable to read source — fall through as if no explicit list detected.
    }
  }

  let suggestedExportList: string | null = null;
  if (!hasExports) {
    const exportNames = [
      ...types.map((d) => (d.kind === "data" ? `${d.name}(..)` : d.name)),
      ...classes.map((d) => `${d.name}(..)`),
      ...functions.map((d) => d.name),
    ];
    suggestedExportList = exportNames.length > 0
      ? `module ${moduleName}\n  ( ${exportNames.join("\n  , ")}\n  ) where`
      : null;
  }

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
    hasExplicitExports: hasExports,
    ...(suggestedExportList ? { suggestedExportList } : {}),
    ...(suggestedExportList
      ? { _nextStep: "Consider adding an explicit export list to control the module's public API." }
      : {}),
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
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
      const result = await handleCheckModule(
        session,
        { module_path, module_name },
        ctx.getProjectDir()
      );
      // Mark completion gate
      try {
        const parsed = JSON.parse(result);
        if (parsed.success !== false) {
          const activeModule = ctx.getWorkflowState().activeModule ?? module_path;
          const mod = ctx.getModuleProgress(activeModule);
          if (mod) {
            ctx.updateModuleProgress(activeModule, {
              completionGates: { ...mod.completionGates, checkModule: true },
            });
          }
        }
      } catch { /* non-fatal */ }
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
