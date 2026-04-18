/**
 * ghci_regression — re-run persisted QuickCheck properties as a regression suite.
 *
 * Also exposes `runRegression` as a reusable helper so other tools (notably
 * `ghci_workflow(action="gate")`) can invoke the regression pass without
 * going through the MCP tool registration layer.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { type ToolContext, registerStrictTool } from "./registry.js";
import { getAllProperties, getModuleProperties, type PropertyRecord } from "../property-store.js";
import { handleQuickCheck } from "./quickcheck.js";
import type { GhciSession } from "../ghci-session.js";

export interface RegressionOutcome {
  total: number;
  passed: number;
  failed: number;
  regressions: Array<{
    property: string;
    module: string;
    tests_module?: string;
    success: boolean;
    error?: string;
  }>;
  /** Wall-clock duration in milliseconds. */
  durationMs: number;
}

/**
 * Re-run persisted properties against the given session. Reusable from
 * `handleWorkflowGate` and other orchestrators. Pure wrt the property store —
 * does not save/update records.
 */
export async function runRegression(
  session: GhciSession,
  projectDir: string,
  opts: { module?: string } = {}
): Promise<RegressionOutcome> {
  const start = Date.now();
  const properties: PropertyRecord[] = opts.module
    ? await getModuleProperties(projectDir, opts.module)
    : await getAllProperties(projectDir);

  if (properties.length === 0) {
    return {
      total: 0,
      passed: 0,
      failed: 0,
      regressions: [],
      durationMs: Date.now() - start,
    };
  }

  let passed = 0;
  let failed = 0;
  const regressions: RegressionOutcome["regressions"] = [];

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
    } else {
      failed++;
      regressions.push({
        property: prop.property,
        module: prop.module,
        ...(prop.tests_module ? { tests_module: prop.tests_module } : {}),
        success: false,
        error: parsed.counterexample ?? parsed.error ?? "Failed",
      });
    }
  }

  return {
    total: properties.length,
    passed,
    failed,
    regressions,
    durationMs: Date.now() - start,
  };
}

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

      if (action === "list") {
        const properties: PropertyRecord[] = modulePath
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

        const byModule: Record<string, Array<{ property: string; law?: string; label?: string; passCount: number; lastPassed: string; tests_module?: string }>> = {};
        for (const p of properties) {
          const key = p.tests_module ?? p.module;
          if (!byModule[key]) byModule[key] = [];
          byModule[key]!.push({
            property: p.property,
            ...(p.label ? { label: p.label } : {}),
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

      // action === "run"
      const session = await ctx.getSession();
      const outcome = await runRegression(session, projectDir, { module: modulePath });

      if (outcome.total === 0) {
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

      return {
        content: [{
          type: "text" as const,
          text: JSON.stringify({
            total: outcome.total,
            passed: outcome.passed,
            failed: outcome.failed,
            regressions: outcome.regressions,
            ...(outcome.regressions.length > 0
              ? { _guidance: [`${outcome.regressions.length} regression(s) found — fix before continuing`] }
              : {}),
          }),
        }],
      };
    }
  );
}
