import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import { readFile } from "node:fs/promises";
import path from "node:path";
import type { ToolContext } from "./registry.js";
import { ensureTool, TOOL_PATH } from "./tool-installer.js";
import { analyzeBasicLintRules } from "../parsers/basic-lint-rules.js";

export interface LintSuggestion {
  hint: string;
  severity: string;
  from: string;
  to: string;
  file: string;
  startLine: number;
  startColumn: number;
  endLine: number;
  endColumn: number;
  note: string[];
}

export async function handleLint(
  projectDir: string,
  args: { module_path: string },
  session?: unknown
): Promise<string> {
  void session;
  const hlint = await ensureTool("hlint");
  if (!hlint.available) {
    return JSON.stringify({
      success: false,
      lint_tool: "hlint",
      unavailable: true,
      source: "none",
      reason: hlint.bundledReason ?? "not-found",
      error: hlint.message,
      _hint:
        hlint.bundledReason === "checksum-missing" || hlint.bundledReason === "checksum-mismatch"
          ? "Fix the bundled hlint manifest entry (sha256/version/provenance) or use a host installation."
          : "Install hlint in host PATH or provide a verified bundled hlint in vendor-tools for this platform.",
    });
  }

  const absPath = path.resolve(projectDir, args.module_path);
  const hlintCmd = hlint.binaryPath ?? "hlint";

  return new Promise<string>((resolve) => {
    execFile(
      hlintCmd,
      ["--json", absPath],
      { env: { ...process.env, PATH: TOOL_PATH }, timeout: 30_000 },
      (error, stdout, stderr) => {
        // hlint returns exit code 1 when it finds suggestions — that's normal
        const output = stdout || "[]";
        try {
          const hints = JSON.parse(output) as Array<{
            hint: string;
            severity: string;
            from: string;
            to: string;
            file: string;
            startLine: number;
            startColumn: number;
            endLine: number;
            endColumn: number;
            note: string[];
          }>;

          const suggestions: LintSuggestion[] = hints.map((h) => ({
            hint: h.hint,
            severity: h.severity,
            from: h.from,
            to: h.to,
            file: h.file,
            startLine: h.startLine,
            startColumn: h.startColumn,
            endLine: h.endLine,
            endColumn: h.endColumn,
            note: h.note ?? [],
          }));

          resolve(
            JSON.stringify({
              success: true,
              lint_tool: "hlint",
              source: hlint.source ?? "host",
              version: hlint.version,
              binaryPath: hlint.binaryPath,
              count: suggestions.length,
              suggestions,
              summary:
                suggestions.length === 0
                  ? "No suggestions"
                  : `${suggestions.length} suggestion(s)`,
            })
          );
        } catch {
          resolve(
            JSON.stringify({
              success: false,
              error: `Failed to parse hlint output: ${stderr || output.slice(0, 200)}`,
            })
          );
        }
      }
    );
  });
}

export async function handleLintBasic(
  projectDir: string,
  args: { module_path: string }
): Promise<string> {
  const absPath = path.resolve(projectDir, args.module_path);
  let code = "";
  try {
    code = await readFile(absPath, "utf8");
  } catch {
    return JSON.stringify({
      success: false,
      lint_tool: "basic-lint-rules",
      error: `File not found: ${args.module_path}`,
    });
  }

  const suggestions = analyzeBasicLintRules(code, args.module_path);
  return JSON.stringify({
    success: true,
    lint_tool: "basic-lint-rules",
    degraded: true,
    gateEligible: false,
    count: suggestions.length,
    suggestions,
    summary:
      suggestions.length === 0
        ? "No basic lint suggestions"
        : `${suggestions.length} basic suggestion(s)`,
    _hint:
      "This is a fallback heuristic lint and does not satisfy the module-complete lint gate. Use ghci_lint with hlint when available.",
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_lint",
    "Run hlint on a Haskell source file and return structured suggestions. " +
      "Each suggestion includes the hint, severity, original code, suggested replacement, and location. " +
      "Uses host hlint first, then bundled hlint. If unavailable, returns unavailable without fallback execution.",
    {
      module_path: z.string().describe('Path to the module to lint. Examples: "src/MyModule.hs"'),
    },
    async ({ module_path }) => {
      const session = await ctx.getSession();
      const result = await handleLint(ctx.getProjectDir(), { module_path }, session);
      // Mark completion gate only when a real linter ran (hlint), not the
      // GHC-warnings fallback.  The fallback is useful for surfacing issues but
      // cannot replace a full hlint analysis — marking it complete would give a
      // false "clean code" signal when hlint is not installed.
      try {
        const parsed = JSON.parse(result);
        ctx.setOptionalToolAvailability("lint", parsed.unavailable ? "unavailable" : "available");
        if (parsed.success && parsed.lint_tool === "hlint") {
          const activeModule = ctx.getWorkflowState().activeModule ?? module_path;
          const mod = ctx.getModuleProgress(activeModule);
          if (mod) {
            ctx.updateModuleProgress(activeModule, {
              completionGates: { ...mod.completionGates, lint: true },
            });
          }
        }
      } catch { /* non-fatal */ }
      return { content: [{ type: "text" as const, text: result }] };
    }
  );

  server.tool(
    "ghci_lint_basic",
    "Run lightweight heuristic lint checks when hlint is unavailable. " +
      "Reports basic anti-patterns but does NOT satisfy the module-complete lint gate.",
    {
      module_path: z.string().describe('Path to the module to lint. Examples: "src/MyModule.hs"'),
    },
    async ({ module_path }) => {
      const result = await handleLintBasic(ctx.getProjectDir(), { module_path });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
