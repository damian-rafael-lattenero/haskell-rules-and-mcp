import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { type ToolContext, registerStrictTool } from "./registry.js";
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
  args: { include_matrix?: boolean; include_runtime?: boolean },
  ctx?: ToolContext
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

    if (ctx) {
      const hlintRow = runtimeRows.find((r) => r.tool === "hlint");
      if (hlintRow) {
        ctx.setOptionalToolAvailability("lint", hlintRow.available ? "available" : "unavailable");
      }
      const fourmoluRow = runtimeRows.find((r) => r.tool === "fourmolu");
      const ormoluRow = runtimeRows.find((r) => r.tool === "ormolu");
      const formatAvailable = (fourmoluRow?.available ?? false) || (ormoluRow?.available ?? false);
      if (fourmoluRow || ormoluRow) {
        ctx.setOptionalToolAvailability("format", formatAvailable ? "available" : "unavailable");
      }
      const hlsRow = runtimeRows.find((r) => r.tool === "hls");
      if (hlsRow) {
        ctx.setOptionalToolAvailability("hls", hlsRow.available ? "available" : "unavailable");
      }
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

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_toolchain_status",
    "Diagnostic report for optional toolchain availability (hlint/fourmolu/ormolu/hls). " +
      "Shows current runtime resolution and a cross-platform release matrix including checksum readiness. " +
      "Propagates results to workflow state so _guidance reflects tool availability.",
    {
      include_matrix: z.boolean().optional().describe("Include cross-platform release matrix diagnostics. Default: true."),
      include_runtime: z.boolean().optional().describe("Include current runtime availability checks. Default: true."),
    },
    async ({ include_matrix, include_runtime }) => {
      const result = await handleToolchainStatus({ include_matrix, include_runtime }, ctx);
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
