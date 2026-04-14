import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import path from "node:path";
import { readFile } from "node:fs/promises";
import type { GhciSession } from "../ghci-session.js";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { categorizeWarnings } from "../parsers/warning-categorizer.js";
import { analyzeBasicLintRules } from "../parsers/basic-lint-rules.js";
import type { ToolContext } from "./registry.js";
import { ensureTool, TOOL_PATH } from "./tool-installer.js";

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

async function ghciLintFallback(
  session: GhciSession,
  modulePath: string
): Promise<string> {
  try {
    await session.execute(":set -Wall -Wcompat -Wincomplete-uni-patterns -Wincomplete-record-updates");
    const loadResult = await session.loadModule(modulePath);
    const allDiags = parseGhcErrors(loadResult.output);
    const warnings = allDiags.filter(
      (e) => e.severity === "warning" && e.code !== "GHC-32850"
    );
    const { actions } = categorizeWarnings(warnings);

    return JSON.stringify({
      success: true,
      fallback: true,
      source: "ghc-warnings",
      count: actions.length,
      suggestions: actions.map((a) => ({
        hint: a.category,
        severity: a.confidence,
        suggestedAction: a.suggestedAction,
        file: a.warning.file,
        startLine: a.warning.line,
        startColumn: a.warning.column,
      })),
      installHint:
        "Install hlint for 200+ additional code quality hints: cabal install hlint",
    });
  } finally {
    await session.execute(":set -Wall").catch(() => {});
  }
}

async function basicLintFallback(
  projectDir: string,
  modulePath: string
): Promise<string> {
  const absPath = path.resolve(projectDir, modulePath);
  try {
    const code = await readFile(absPath, "utf8");
    const suggestions = analyzeBasicLintRules(code, modulePath);
    return JSON.stringify({
      success: true,
      fallback: true,
      source: "basic-lint-rules",
      count: suggestions.length,
      suggestions,
      installHint:
        "Install hlint for deeper analysis. MCP will prefer bundled hlint when available.",
    });
  } catch {
    return JSON.stringify({
      success: false,
      fallback: true,
      source: "basic-lint-rules",
      error: `Could not read module for lint fallback: ${modulePath}`,
    });
  }
}

export async function handleLint(
  projectDir: string,
  args: { module_path: string },
  session?: GhciSession
): Promise<string> {
  const hlint = await ensureTool("hlint");
  if (!hlint.available) {
    // Auto-installation started (or already in progress / failed).
    // Provide the GHC-warnings fallback so the LLM gets some signal now,
    // but flag it clearly so it knows to retry once hlint is ready.
    if (session) {
      const ghcFallback = JSON.parse(await ghciLintFallback(session, args.module_path));
      const basicFallback = JSON.parse(await basicLintFallback(projectDir, args.module_path));
      const mergedSuggestions = [
        ...(Array.isArray(ghcFallback.suggestions) ? ghcFallback.suggestions : []),
        ...(Array.isArray(basicFallback.suggestions) ? basicFallback.suggestions : []),
      ];
      return JSON.stringify({
        success: true,
        fallback: true,
        source: "ghc-warnings+basic-lint-rules",
        count: mergedSuggestions.length,
        suggestions: mergedSuggestions,
        installHint:
          "Bundled hlint not available for this platform yet. Host/auto-install hlint will be used when ready.",
        _hlint_status: hlint.installing ? "installing" : hlint.failed ? "failed" : "unavailable",
        _hlint_message: hlint.message,
      });
    }
    const basicFallback = JSON.parse(await basicLintFallback(projectDir, args.module_path));
    return JSON.stringify({
      ...basicFallback,
      _hlint_status: hlint.installing ? "installing" : hlint.failed ? "failed" : "unavailable",
      _hlint_message: hlint.message,
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

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_lint",
    "Run hlint on a Haskell source file and return structured suggestions. " +
      "Each suggestion includes the hint, severity, original code, suggested replacement, and location. " +
      "Requires hlint to be installed.",
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
}
