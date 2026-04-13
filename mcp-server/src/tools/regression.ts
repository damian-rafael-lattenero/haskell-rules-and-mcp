/**
 * ghci_regression — re-run persisted QuickCheck properties as a regression suite.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { ToolContext } from "./registry.js";
import { getAllProperties, getModuleProperties } from "../property-store.js";
import { handleQuickCheck } from "./quickcheck.js";

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_regression",
    "Re-run persisted QuickCheck properties as a regression suite. " +
      "Properties are saved automatically when they pass during development. " +
      "Use action='list' to see stored properties, action='run' to re-run them all.",
    {
      action: z.enum(["list", "run"]).optional().describe(
        '"list": show stored properties. "run" (default): re-run all and report regressions.'
      ),
      module: z.string().optional().describe(
        "Filter by module path. If omitted, runs all properties."
      ),
    },
    async ({ action = "run", module: modulePath }) => {
      const projectDir = ctx.getProjectDir();
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
        // Group by module
        const byModule: Record<string, Array<{ property: string; law?: string; passCount: number; lastPassed: string }>> = {};
        for (const p of properties) {
          if (!byModule[p.module]) byModule[p.module] = [];
          byModule[p.module]!.push({
            property: p.property,
            ...(p.law ? { law: p.law } : {}),
            passCount: p.passCount,
            lastPassed: p.lastPassed,
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
      const results: Array<{ property: string; module: string; success: boolean; error?: string }> = [];
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
          results.push({ property: prop.property, module: prop.module, success: true });
        } else {
          failed++;
          results.push({
            property: prop.property,
            module: prop.module,
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
