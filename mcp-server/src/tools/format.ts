import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import { readFile, writeFile, stat } from "node:fs/promises";
import path from "node:path";
import type { ToolContext } from "./registry.js";

const GHCUP_BIN = path.join(process.env.HOME ?? "/Users", ".ghcup", "bin");
const CABAL_BIN = path.join(process.env.HOME ?? "/Users", ".cabal", "bin");
const TOOL_PATH = `${GHCUP_BIN}:${CABAL_BIN}:${process.env.PATH}`;

/**
 * Detect which formatter is available. Prefers fourmolu over ormolu.
 */
async function detectFormatter(): Promise<string | null> {
  for (const cmd of ["fourmolu", "ormolu"]) {
    const found = await commandExists(cmd);
    if (found) return cmd;
  }
  return null;
}

function commandExists(cmd: string): Promise<boolean> {
  return new Promise((resolve) => {
    execFile("which", [cmd], { env: { ...process.env, PATH: TOOL_PATH } }, (err) => {
      resolve(!err);
    });
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

async function basicStyleChecks(filePath: string): Promise<string> {
  let stats;
  try {
    stats = await stat(filePath);
  } catch {
    return JSON.stringify({ success: false, error: `File not found: ${filePath}` });
  }
  if (stats.size > 1_000_000) {
    return JSON.stringify({ success: false, error: "File too large for basic checks (>1MB)" });
  }

  const content = await readFile(filePath, "utf-8");
  const lines = content.split("\n");
  const issues: Array<{ line: number; issue: string; severity: string }> = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (/\t/.test(line)) {
      issues.push({ line: i + 1, issue: "Tab character found (use spaces)", severity: "warning" });
    }
    if (/[ \t]+$/.test(line)) {
      issues.push({ line: i + 1, issue: "Trailing whitespace", severity: "suggestion" });
    }
    if (line.length > 100) {
      issues.push({ line: i + 1, issue: `Line too long (${line.length} chars, max 100)`, severity: "suggestion" });
    }
  }

  if (content.length > 0 && !content.endsWith("\n")) {
    issues.push({ line: lines.length, issue: "Missing final newline", severity: "warning" });
  }

  return JSON.stringify({
    success: true,
    fallback: true,
    source: "basic-style-checks",
    count: issues.length,
    issues,
    installSuggestions: [
      "cabal install fourmolu",
      "ghcup install fourmolu",
      "brew install fourmolu",
    ],
  });
}

export async function handleFormat(
  projectDir: string,
  args: { module_path: string; write?: boolean }
): Promise<string> {
  const formatter = await detectFormatter();
  if (!formatter) {
    const absPath = path.resolve(projectDir, args.module_path);
    return basicStyleChecks(absPath);
  }

  const absPath = path.resolve(projectDir, args.module_path);

  if (args.write) {
    const result = await runFormatter(formatter, ["--mode", "inplace", absPath], projectDir);
    if (result.code !== 0) {
      return JSON.stringify({
        success: false,
        formatter,
        error: result.stderr || "Formatting failed",
      });
    }
    return JSON.stringify({
      success: true,
      formatter,
      written: true,
      message: `Formatted ${args.module_path} in place`,
    });
  }

  // Dry-run: get formatted output and compare
  const result = await runFormatter(formatter, ["--mode", "stdout", absPath], projectDir);
  if (result.code !== 0) {
    return JSON.stringify({
      success: false,
      formatter,
      error: result.stderr || "Formatting failed",
    });
  }

  const original = await readFile(absPath, "utf-8");
  const formatted = result.stdout;
  const changed = original !== formatted;

  return JSON.stringify({
    success: true,
    formatter,
    changed,
    ...(changed ? { formatted } : { message: "Already formatted" }),
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_format",
    "Format a Haskell source file using ormolu or fourmolu. " +
      "By default shows the formatted output without writing (dry-run). " +
      "Set write=true to format in place. Requires ormolu or fourmolu to be installed.",
    {
      module_path: z.string().describe('Path to the module to format. Examples: "src/MyModule.hs"'),
      write: z.boolean().optional().describe("If true, write formatted output to file. Default: false (dry-run)."),
    },
    async ({ module_path, write }) => {
      const result = await handleFormat(ctx.getProjectDir(), { module_path, write });
      // Mark completion gate
      try {
        const parsed = JSON.parse(result);
        if (parsed.success) {
          const activeModule = ctx.getWorkflowState().activeModule ?? module_path;
          const mod = ctx.getModuleProgress(activeModule);
          if (mod) {
            ctx.updateModuleProgress(activeModule, {
              completionGates: { ...mod.completionGates, format: true },
            });
          }
        }
      } catch { /* non-fatal */ }
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
