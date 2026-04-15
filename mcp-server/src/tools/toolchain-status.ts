import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { ToolContext } from "./registry.js";
import {
  ensureTool,
  getBundledToolStatus,
  type BundledToolFailureReason,
} from "./tool-installer.js";
import { getToolchainTupleMatrix } from "./auto-download.js";

type SupportedTool = "hlint" | "fourmolu" | "ormolu" | "hls";

interface RuntimeStatusRow {
  tool: SupportedTool;
  available: boolean;
  source?: "host" | "bundled" | "installed";
  binaryPath?: string;
  version?: string;
  error?: string;
  bundledReason?: BundledToolFailureReason;
  bundledMessage?: string;
}

export async function handleToolchainStatus(
  args: { include_matrix?: boolean; include_runtime?: boolean }
): Promise<string> {
  const includeMatrix = args.include_matrix ?? true;
  const includeRuntime = args.include_runtime ?? true;
  const tools: SupportedTool[] = ["hlint", "fourmolu", "ormolu", "hls"];

  const runtimeRows: RuntimeStatusRow[] = [];
  if (includeRuntime) {
    for (const tool of tools) {
      const ensured = await ensureTool(tool);
      const bundled = await getBundledToolStatus(tool);
      runtimeRows.push({
        tool,
        available: ensured.available,
        source: ensured.source,
        binaryPath: ensured.binaryPath,
        version: ensured.version,
        error: ensured.error,
        bundledReason: bundled.reason,
        bundledMessage: bundled.message,
      });
    }
  }

  const matrix = includeMatrix ? getToolchainTupleMatrix() : [];
  const matrixSummary = includeMatrix
    ? {
        total: matrix.length,
        autoDownloadConfigured: matrix.filter((r) => r.autoDownloadConfigured).length,
        withVerifiableChecksum: matrix.filter((r) => r.checksumConfigured).length,
      }
    : undefined;

  return JSON.stringify({
    success: true,
    runtime: {
      platform: process.platform,
      arch: process.arch,
      tools: runtimeRows,
    },
    ...(includeMatrix
      ? {
          releaseMatrix: matrix,
          releaseMatrixSummary: matrixSummary,
        }
      : {}),
  });
}

export function register(server: McpServer, _ctx: ToolContext): void {
  server.tool(
    "ghci_toolchain_status",
    "Diagnostic report for optional toolchain availability (hlint/fourmolu/ormolu/hls). " +
      "Shows current runtime resolution and a cross-platform release matrix including checksum readiness.",
    {
      include_matrix: z.boolean().optional().describe("Include cross-platform release matrix diagnostics. Default: true."),
      include_runtime: z.boolean().optional().describe("Include current runtime availability checks. Default: true."),
    },
    async ({ include_matrix, include_runtime }) => {
      const result = await handleToolchainStatus({ include_matrix, include_runtime });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
