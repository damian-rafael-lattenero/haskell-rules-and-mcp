import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import { readFile, writeFile, stat } from "node:fs/promises";
import path from "node:path";
import type { ToolContext } from "./registry.js";
import { ensureTool, resolveToolBinary, TOOL_PATH } from "./tool-installer.js";

/**
 * Detect which formatter is available. Prefers fourmolu over ormolu.
 * Returns the binary name, or null if neither is installed.
 */
async function detectFormatter(): Promise<string | null> {
  for (const cmd of ["fourmolu", "ormolu"] as const) {
    const resolved = await resolveToolBinary(cmd);
    if (resolved) return resolved.binaryPath;
  }
  return null;
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

async function basicStyleChecks(filePath: string, write?: boolean): Promise<string> {
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
    const line = lines[i]!;
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

  if (write) {
    // Apply auto-fixable issues: strip trailing whitespace, convert tabs to spaces, add final newline
    let fixesApplied = 0;
    let fixed = lines
      .map((line) => {
        let out = line;
        // Tabs → 2 spaces
        if (/\t/.test(out)) {
          out = out.replace(/\t/g, "  ");
          fixesApplied++;
        }
        // Trailing whitespace
        const stripped = out.replace(/[ \t]+$/, "");
        if (stripped !== out) {
          out = stripped;
          fixesApplied++;
        }
        return out;
      })
      .join("\n");

    // Ensure final newline
    if (content.length > 0 && !fixed.endsWith("\n")) {
      fixed += "\n";
      fixesApplied++;
    }

    await writeFile(filePath, fixed, "utf-8");

    return JSON.stringify({
      success: true,
      fallback: true,
      written: true,
      fixesApplied,
      source: "basic-style-checks",
      message: fixesApplied > 0
        ? `Applied ${fixesApplied} fix(es): trailing whitespace, tabs→spaces, final newline`
        : "File already clean — no fixes needed",
      _formatWarning:
        "fourmolu not installed: only basic whitespace/tab fixes applied. " +
        "For full Haskell formatting, install fourmolu: ghcup install fourmolu",
      installSuggestions: [
        "ghcup install fourmolu",
        "cabal install fourmolu",
        "brew install fourmolu",
      ],
    });
  }

  return JSON.stringify({
    success: true,
    fallback: true,
    source: "basic-style-checks",
    count: issues.length,
    issues,
    _formatWarning:
      "fourmolu not installed: analysis limited to basic whitespace/tab checks. " +
      "For full Haskell formatting, install fourmolu: ghcup install fourmolu",
    installSuggestions: [
      "ghcup install fourmolu",
      "cabal install fourmolu",
      "brew install fourmolu",
    ],
  });
}

export async function handleFormat(
  projectDir: string,
  args: { module_path: string; write?: boolean }
): Promise<string> {
  let formatter = await detectFormatter();
  let formatterSource: "bundled" | "host" | "installed" = "host";
  let formatterVersion: string | undefined;

  if (!formatter) {
    // Trigger auto-installation of fourmolu (preferred over ormolu).
    const fourmolu = await ensureTool("fourmolu");

    if (fourmolu.available) {
      formatter = fourmolu.binaryPath ?? null;
      formatterSource = fourmolu.source ?? "installed";
      formatterVersion = fourmolu.version;
    } else if (fourmolu.installing || fourmolu.failed) {
      // Return basic style check results while fourmolu is being installed,
      // but flag the status so the LLM knows to retry for real formatting.
      const absPath = path.resolve(projectDir, args.module_path);
      const basic = JSON.parse(await basicStyleChecks(absPath, args.write));
      return JSON.stringify({
        ...basic,
        _formatter_status: fourmolu.installing ? "installing" : "failed",
        _formatter_message: fourmolu.message,
      });
    }

    // Still not available (e.g. first call returned installing, now it's done).
    formatter = await detectFormatter();
    if (!formatter) {
      const absPath = path.resolve(projectDir, args.module_path);
      return basicStyleChecks(absPath, args.write);
    }
    const resolved = await resolveToolBinary("fourmolu") ?? await resolveToolBinary("ormolu");
    if (resolved) formatterSource = resolved.source;
  } else {
    const fourmoluResolved = await resolveToolBinary("fourmolu");
    const ormoluResolved = await resolveToolBinary("ormolu");
    if (fourmoluResolved && fourmoluResolved.binaryPath === formatter) {
      formatterSource = fourmoluResolved.source;
    } else if (ormoluResolved && ormoluResolved.binaryPath === formatter) {
      formatterSource = ormoluResolved.source;
    }
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
      // Mark completion gate only when a real formatter ran (fourmolu/ormolu).
      // The basic-style-checks fallback only fixes whitespace and tabs — it
      // cannot be considered a full formatting pass and should not unlock the
      // format gate, which would give a false "formatted" signal.
      try {
        const parsed = JSON.parse(result);
        if (parsed.success && parsed.format_tool && !parsed.fallback) {
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
