import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { type ToolContext, registerStrictTool, zBool } from "./registry.js";
import { getBundledToolStatus, TOOL_PATH } from "./tool-installer.js";
import { awaitTool } from "./toolchain-warmup.js";
import { analyzeBasicFormatRules } from "../parsers/basic-format-rules.js";

/**
 * Detect which formatter is available. Prefers fourmolu over ormolu.
 * Returns the binary name, or null if neither is installed.
 * Now uses ensureTool to enable auto-download if needed.
 */
interface FormatterResolution {
  binaryPath: string;
  source: "bundled" | "host" | "installed";
  version?: string;
}

async function detectFormatter(): Promise<FormatterResolution | null> {
  // Uses `awaitTool` so warmup promises are shared when warmup has already
  // started the download — avoids redundant fetches under concurrent tool calls.
  for (const cmd of ["fourmolu", "ormolu"] as const) {
    const resolved = await awaitTool(cmd);
    if (resolved.available && resolved.binaryPath) {
      return {
        binaryPath: resolved.binaryPath,
        source: resolved.source ?? "host",
        version: resolved.version,
      };
    }
  }
  return null;
}

async function detectFormatterVersion(binaryPath: string): Promise<string | undefined> {
  return new Promise((resolve) => {
    execFile(
      binaryPath,
      ["--version"],
      { env: { ...process.env, PATH: TOOL_PATH }, timeout: 10_000 },
      (err, stdout, stderr) => {
        if (err) {
          resolve(undefined);
          return;
        }
        resolve((stdout || stderr).trim().split("\n")[0]);
      }
    );
  });
}

function runFormatter(
  cmd: string,
  args: string[],
  cwd: string
): Promise<{ stdout: string; stderr: string; code: number }> {
  return new Promise((resolve) => {
    execFile(
      cmd,
      args,
      { cwd, env: { ...process.env, PATH: TOOL_PATH }, timeout: 30_000 },
      (error, stdout, stderr) => {
        resolve({
          stdout: stdout ?? "",
          stderr: stderr ?? "",
          code: error?.code === undefined ? (error ? 1 : 0) : (typeof error.code === "number" ? error.code : 1),
        });
      }
    );
  });
}

/**
 * Degraded formatter used when fourmolu/ormolu are unavailable. Applies
 * lexically-safe fixes (trailing whitespace, CRLF → LF, missing final
 * newline) and returns an envelope with `degraded: true, gateEligible:
 * false` — it does NOT unlock the module-complete format gate, so callers
 * always know when a real formatter ran vs. this fallback.
 */
export async function handleFormatBasic(
  projectDir: string,
  args: { module_path: string; write?: boolean }
): Promise<string> {
  const absPath = path.resolve(projectDir, args.module_path);
  let original = "";
  try {
    original = await readFile(absPath, "utf-8");
  } catch {
    return JSON.stringify({
      success: false,
      format_tool: "basic-format-rules",
      degraded: true,
      gateEligible: false,
      error: `File not found: ${args.module_path}`,
    });
  }

  const analysis = analyzeBasicFormatRules(original);

  if (args.write && analysis.changed) {
    try {
      await writeFile(absPath, analysis.fixed, "utf-8");
    } catch (err) {
      return JSON.stringify({
        success: false,
        format_tool: "basic-format-rules",
        degraded: true,
        gateEligible: false,
        error: err instanceof Error ? err.message : String(err),
      });
    }
  }

  return JSON.stringify({
    success: true,
    format_tool: "basic-format-rules",
    degraded: true,
    gateEligible: false,
    changed: analysis.changed,
    issues: analysis.issues,
    issueCount: analysis.issues.length,
    written: !!(args.write && analysis.changed),
    ...(args.write
      ? {}
      : analysis.changed
        ? { formatted: analysis.fixed }
        : { message: "No basic formatting issues" }),
    _hint:
      "This is a heuristic fallback applied ONLY when fourmolu/ormolu are unavailable. It covers trailing whitespace, CRLF line endings, tabs in indentation, and missing final newline. For full formatting (layout, alignment, module-header canonicalization), install fourmolu or ormolu.",
  });
}

export async function handleFormat(
  projectDir: string,
  args: { module_path: string; write?: boolean }
): Promise<string> {
  const formatterResolution = await detectFormatter();
  let formatter = formatterResolution?.binaryPath ?? null;
  let formatterSource: "bundled" | "host" | "installed" = formatterResolution?.source ?? "host";
  let formatterVersion: string | undefined = formatterResolution?.version;

  if (!formatter) {
    const bundledFourmolu = await getBundledToolStatus("fourmolu");
    const bundledOrmolu = await getBundledToolStatus("ormolu");
    const bundledFailure = bundledFourmolu.available ? bundledOrmolu : bundledFourmolu;
    return JSON.stringify({
      success: false,
      unavailable: true,
      formatter: "fourmolu|ormolu",
      source: "none",
      reason: bundledFailure.reason ?? "not-found",
      error:
        "No formatter available (not found in host PATH or bundled toolchain).",
      _hint:
        bundledFailure.reason === "checksum-missing" || bundledFailure.reason === "checksum-mismatch"
          ? "Fix the bundled formatter manifest entry (sha256/version/provenance) or use a host installation."
          : "Provide fourmolu/ormolu in host PATH, or bundle one in vendor-tools for this platform.",
    });
  } else if (!formatterVersion) {
    formatterVersion = await detectFormatterVersion(formatter);
  }

  const absPath = path.resolve(projectDir, args.module_path);

  if (args.write) {
    const result = await runFormatter(formatter, ["--mode", "inplace", absPath], projectDir);
    if (result.code !== 0) {
      return JSON.stringify({
        success: false,
        formatter: path.basename(formatter),
        error: result.stderr || "Formatting failed",
      });
    }
    return JSON.stringify({
      success: true,
      format_tool: path.basename(formatter),
      source: formatterSource,
      version: formatterVersion,
      binaryPath: formatter,
      written: true,
      message: `Formatted ${args.module_path} in place`,
    });
  }

  // Dry-run: get formatted output and compare
  const result = await runFormatter(formatter, ["--mode", "stdout", absPath], projectDir);
  if (result.code !== 0) {
    return JSON.stringify({
      success: false,
      formatter: path.basename(formatter),
      error: result.stderr || "Formatting failed",
    });
  }

  const original = await readFile(absPath, "utf-8");
  const formatted = result.stdout;
  const changed = original !== formatted;

  return JSON.stringify({
    success: true,
    format_tool: path.basename(formatter),
    source: formatterSource,
    version: formatterVersion,
    binaryPath: formatter,
    changed,
    ...(changed ? { formatted } : { message: "Already formatted" }),
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_format",
    "Format a Haskell source file using ormolu or fourmolu. " +
      "By default shows the formatted output without writing (dry-run). " +
      "Set write=true to format in place. Uses host formatter first, then bundled formatter. If unavailable, returns unavailable without fallback formatting.",
    {
      module_path: z.string().describe('Path to the module to format. Examples: "src/MyModule.hs"'),
      write: zBool().optional().describe("If true, write formatted output to file. Default: false (dry-run)."),
    },
    async ({ module_path, write }) => {
      const primary = await handleFormat(ctx.getProjectDir(), { module_path, write });
      let parsed = JSON.parse(primary);

      // When fourmolu/ormolu are unavailable, synthesize a degraded envelope
      // from the basic-format-rules fallback so the caller always has
      // something actionable (trailing whitespace / CRLF / missing newline)
      // while keeping gateEligible:false so the module-complete format gate
      // stays locked until a real formatter runs.
      if (parsed.unavailable === true) {
        ctx.setOptionalToolAvailability("format", "unavailable");
        const basic = JSON.parse(await handleFormatBasic(ctx.getProjectDir(), { module_path, write }));
        const merged = {
          ...basic,
          _warning:
            "fourmolu/ormolu are unavailable. Falling back to basic-format-rules. " +
            "This does NOT satisfy the module-complete format gate.",
          _primary_failure: {
            formatter: parsed.formatter,
            reason: parsed.reason,
            error: parsed.error,
          },
        };
        return { content: [{ type: "text" as const, text: JSON.stringify(merged) }] };
      }

      // Mark completion gate only when a real formatter ran (fourmolu/ormolu).
      try {
        ctx.setOptionalToolAvailability("format", "available");
        if (parsed.success && parsed.format_tool && !parsed.fallback && !parsed.degraded) {
          const activeModule = ctx.getWorkflowState().activeModule ?? module_path;
          const mod = ctx.getModuleProgress(activeModule);
          if (mod) {
            ctx.updateModuleProgress(activeModule, {
              completionGates: { ...mod.completionGates, format: true },
            });
          }
        }
      } catch { /* non-fatal */ }
      return { content: [{ type: "text" as const, text: primary }] };
    }
  );
}
