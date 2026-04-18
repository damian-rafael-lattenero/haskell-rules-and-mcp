/**
 * ghci_regression — re-run persisted QuickCheck properties as a regression suite.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { type ToolContext, registerStrictTool } from "./registry.js";
import { getAllProperties, getModuleProperties } from "../property-store.js";
import { handleQuickCheck } from "./quickcheck.js";

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_regression",
    "Re-run persisted QuickCheck properties as a regression suite. " +
      "Properties are saved automatically when they pass during development. " +
      "Use action='list' to see stored properties, action='run' to re-run them all, " +
      "action='save' to learn about auto-save behaviour.",
    {
      action: z.enum(["list", "run", "save"]).optional().describe(
        '"list": show stored properties. ' +
          '"run" (default): re-run all and report regressions. ' +
          '"save": explains that properties are auto-saved (no manual action needed).'
      ),
      module: z.string().optional().describe(
        "Filter by module path (matches tests_module when set, otherwise module). " +
          "If omitted, runs all properties."
      ),
    },
    async ({ action = "run", module: modulePath }) => {
      const projectDir = ctx.getProjectDir();

      // Save alias — explain auto-save behaviour
      if (action === "save") {
        return {
          content: [{
            type: "text" as const,
            text: JSON.stringify({
              saved: false,
              message:
                "Properties are auto-saved when they pass via ghci_quickcheck or " +
                "ghci_quickcheck_batch. No manual save needed. " +
                "Use action='list' to see all saved properties.",
              tip:
                "To tag properties to the module they test (not just the load context), " +
                "pass tests_module='src/YourModule.hs' to ghci_quickcheck or " +
                "ghci_quickcheck_batch. This makes 'module' filter in regression work correctly.",
            }),
          }],
        };
      }

      const properties = modulePath
        ? await getModuleProperties(projectDir, modulePath)
        : await getAllProperties(projectDir);

      if (properties.length === 0) {
        return {
          content: [{
            type: "text" as const,
            text: JSON.stringify({
              total: 0,
              message: "No stored properties found. Properties are saved automatically when ghci_quickcheck passes.",
            }),
          }],
        };
      }

      if (action === "list") {
        // Group by semantic target (tests_module ?? module) for display
        const byModule: Record<string, Array<{ property: string; law?: string; passCount: number; lastPassed: string; tests_module?: string }>> = {};
        for (const p of properties) {
          const key = p.tests_module ?? p.module;
          if (!byModule[key]) byModule[key] = [];
          byModule[key]!.push({
            property: p.property,
            ...(p.law ? { law: p.law } : {}),
            passCount: p.passCount,
            lastPassed: p.lastPassed,
            ...(p.tests_module && p.tests_module !== p.module
              ? { tests_module: p.tests_module }
              : {}),
          });
        }
        return {
          content: [{
            type: "text" as const,
            text: JSON.stringify({ total: properties.length, modules: byModule }),
          }],
        };
      }

      // Run all properties
      const session = await ctx.getSession();
      const results: Array<{ property: string; module: string; tests_module?: string; success: boolean; error?: string }> = [];
      let passed = 0;
      let failed = 0;

      for (const prop of properties) {
        const resultStr = await handleQuickCheck(
          session,
          { property: prop.property, tests: 100 },
          undefined,
          projectDir
        );
        const parsed = JSON.parse(resultStr);
        if (parsed.success) {
          passed++;
          results.push({
            property: prop.property,
            module: prop.module,
            ...(prop.tests_module ? { tests_module: prop.tests_module } : {}),
            success: true,
          });
        } else {
          failed++;
          results.push({
            property: prop.property,
            module: prop.module,
            ...(prop.tests_module ? { tests_module: prop.tests_module } : {}),
            success: false,
            error: parsed.counterexample ?? parsed.error ?? "Failed",
          });
        }
      }

      const regressions = results.filter((r) => !r.success);
      return {
        content: [{
          type: "text" as const,
          text: JSON.stringify({
            total: properties.length,
            passed,
            failed,
            regressions,
            ...(regressions.length > 0
              ? { _guidance: [`${regressions.length} regression(s) found — fix before continuing`] }
              : {}),
          }),
        }],
      };
    }
  );
}
