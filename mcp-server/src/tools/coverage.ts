import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import type { ToolContext } from "./registry.js";

export function parseCoverage(stdout: string): Array<{ metric: string; percent: number }> {
  const rows: Array<{ metric: string; percent: number }> = [];
  const lines = stdout.split(/\r?\n/);
  for (const line of lines) {
    const match = line.match(/(\d+(?:\.\d+)?)%\s+(.+)/);
    if (!match) continue;
    rows.push({
      percent: Number(match[1]),
      metric: match[2].trim(),
    });
  }
  return rows;
}

export async function handleCoverage(
  projectDir: string,
  args: { min_percent?: number; timeout_ms?: number }
): Promise<string> {
  const timeout = args.timeout_ms ?? 180_000;
  return new Promise((resolve) => {
    execFile(
      "cabal",
      ["test", "--enable-coverage"],
      { cwd: projectDir, timeout, env: process.env },
      (error, stdout, stderr) => {
        const output = `${stdout ?? ""}\n${stderr ?? ""}`.trim();
        const metrics = parseCoverage(output);
        const overall = metrics.length > 0 ? Math.min(...metrics.map((m) => m.percent)) : undefined;
        const minPercent = args.min_percent;
        const meetsThreshold = minPercent === undefined || (overall !== undefined && overall >= minPercent);

        if (error) {
          resolve(
            JSON.stringify({
              success: false,
              command: "cabal test --enable-coverage",
              error: error.message,
              output,
              metrics,
              ...(overall !== undefined ? { overallPercent: overall } : {}),
            })
          );
          return;
        }

        resolve(
          JSON.stringify({
            success: true,
            command: "cabal test --enable-coverage",
            metrics,
            ...(overall !== undefined ? { overallPercent: overall } : {}),
            ...(minPercent !== undefined ? { minPercent, meetsThreshold } : {}),
            summary:
              overall === undefined
                ? "Coverage run completed but no parseable coverage percentages were found in output."
                : `Coverage run completed. Lowest reported metric: ${overall.toFixed(2)}%.`,
          })
        );
      }
    );
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "cabal_coverage",
    "Run cabal test with HPC coverage enabled and return structured coverage percentages parsed from output.",
    {
      min_percent: z.number().optional().describe("Optional minimum coverage threshold. If set, response includes meetsThreshold."),
      timeout_ms: z.number().optional().describe("Optional timeout in milliseconds. Default: 180000."),
    },
    async ({ min_percent, timeout_ms }) => {
      const result = await handleCoverage(ctx.getProjectDir(), { min_percent, timeout_ms });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
