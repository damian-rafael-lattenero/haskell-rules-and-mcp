import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import path from "node:path";
import type { GhciSession } from "../ghci-session.js";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { categorizeWarnings } from "../parsers/warning-categorizer.js";
import type { ToolContext } from "./registry.js";

const GHCUP_BIN = path.join(process.env.HOME ?? "/Users", ".ghcup", "bin");
const CABAL_BIN = path.join(process.env.HOME ?? "/Users", ".cabal", "bin");
const TOOL_PATH = `${GHCUP_BIN}:${CABAL_BIN}:${process.env.PATH}`;

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

function hlintAvailable(): Promise<boolean> {
  return new Promise((resolve) => {
    execFile("which", ["hlint"], { env: { ...process.env, PATH: TOOL_PATH } }, (err) => {
      resolve(!err);
    });
  });
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

export async function handleLint(
  projectDir: string,
  args: { module_path: string },
  session?: GhciSession
): Promise<string> {
  const available = await hlintAvailable();
  if (!available) {
    if (session) {
      return ghciLintFallback(session, args.module_path);
    }
    return JSON.stringify({
      success: false,
      error: "hlint not found. Install it: cabal install hlint",
    });
  }

  const absPath = path.resolve(projectDir, args.module_path);

  return new Promise<string>((resolve) => {
    execFile(
      "hlint",
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
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
