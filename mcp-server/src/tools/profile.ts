/**
 * ghci_profile — Performance analysis and optimization hints.
 *
 * Actions:
 *   suggest — static heuristic analysis of Haskell source code
 *   time    — run cabal with profiling and parse .prof output
 *   heap    — run cabal with heap profiling
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile } from "node:fs/promises";
import { execFile } from "node:child_process";
import path from "node:path";
import type { ToolContext } from "./registry.js";

// ─── Optimization suggestion heuristics ──────────────────────────────────────

export interface OptimizationSuggestion {
  line: number;
  issue: string;
  suggestion: string;
  severity: "warning" | "info";
}

/**
 * Analyze Haskell source code with static heuristics and return optimization suggestions.
 * These are pattern-based, not semantic — they catch common anti-patterns.
 */
export function suggestOptimizations(code: string): OptimizationSuggestion[] {
  const suggestions: OptimizationSuggestion[] = [];
  const lines = code.split("\n");

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    const lineNum = i + 1;
    const trimmed = line.trim();

    // Skip comments and empty lines
    if (trimmed.startsWith("--") || trimmed === "") continue;

    // Pattern 1: String (++) concatenation in functional context (likely O(n²))
    if (/\+\+/.test(line) && /(String|Char|\[")|foldr|map|concat/.test(line)) {
      suggestions.push({
        line: lineNum,
        issue: "String concatenation with (++) may be O(n²) in loops or folds",
        suggestion: "Consider using Data.Text or a difference list (ShowS) for efficient string building",
        severity: "warning",
      });
    }

    // Pattern 2: Naive tail recursion without accumulator (last call sums, products)
    // Detects: f (x:xs) = x + f xs  or  x * f xs  (no accumulator)
    if (/=\s*\w+\s*[+*]\s*\w+'\s*\w+/.test(line) || 
        /\w+\s*[+*]\s*\w+'\s+\w+/.test(line)) {
      suggestions.push({
        line: lineNum,
        issue: "Recursive function without accumulator may not be tail-call optimized",
        suggestion: "Use an accumulator parameter for tail-call optimization: f acc (x:xs) = f (acc + x) xs",
        severity: "warning",
      });
    }

    // Pattern 3: head on a list (partial function, can throw)
    if (/\bhead\b/.test(line) && !trimmed.startsWith("--")) {
      suggestions.push({
        line: lineNum,
        issue: "head is a partial function that throws on empty list",
        suggestion: "Use pattern matching or Data.List.NonEmpty, or check for empty list first",
        severity: "warning",
      });
    }

    // Pattern 4: fromJust (partial function)
    if (/\bfromJust\b/.test(line)) {
      suggestions.push({
        line: lineNum,
        issue: "fromJust throws on Nothing — partial function",
        suggestion: "Use maybe, fromMaybe, or pattern matching to handle the Nothing case safely",
        severity: "warning",
      });
    }

    // Pattern 5: Data.Map.lookup without maybe (potential incomplete pattern)
    if (/Data\.Map\.lookup|Map\.lookup/.test(line) && !/maybe|case|fromMaybe/.test(line)) {
      suggestions.push({
        line: lineNum,
        issue: "Map.lookup result may not be handled for the Nothing case",
        suggestion: "Use fromMaybe or case expression to handle both Just and Nothing",
        severity: "info",
      });
    }
  }

  // Deduplicate suggestions at the same line with same issue
  const seen = new Set<string>();
  return suggestions.filter((s) => {
    const key = `${s.line}:${s.issue}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

// ─── .prof file parser ────────────────────────────────────────────────────────

export interface CostCentre {
  name: string;
  module: string;
  src: string;
  timePercent: number;
  allocPercent: number;
}

export interface ProfResult {
  success: boolean;
  totalTime?: string;
  totalAlloc?: string;
  topCostCentres: CostCentre[];
  error?: string;
}

/**
 * Parse GHC .prof file content and extract cost centre data.
 */
export function parseProfFile(content: string): ProfResult {
  if (!content.trim()) {
    return { success: true, topCostCentres: [] };
  }

  const lines = content.split("\n");
  let totalTime: string | undefined;
  let totalAlloc: string | undefined;
  const costCentres: CostCentre[] = [];
  let inSummarySection = false;

  for (const line of lines) {
    const trimmed = line.trim();

    // Extract total time/alloc
    const timeMatch = trimmed.match(/total time\s+=\s+([\d.]+\s+\w+)/);
    if (timeMatch) totalTime = timeMatch[1]!;

    const allocMatch = trimmed.match(/total alloc\s+=\s+([\d,]+\s+\w+)/);
    if (allocMatch) totalAlloc = allocMatch[1]!;

    // Detect the summary section (COST CENTRE MODULE ... %time %alloc header)
    if (/^COST CENTRE\s+MODULE/.test(trimmed) && /%time/.test(trimmed) && !/%alloc.*%alloc/.test(trimmed)) {
      inSummarySection = true;
      continue;
    }

    // End of summary section (blank line or the detailed tree header)
    if (inSummarySection && (trimmed === "" || /individual.*inherited/.test(trimmed))) {
      if (costCentres.length > 0) inSummarySection = false;
      continue;
    }

    if (inSummarySection && trimmed !== "") {
      // Format: COST_CENTRE MODULE SRC %time %alloc
      const parts = trimmed.split(/\s+/);
      if (parts.length >= 4) {
        const name = parts[0]!;
        const module_ = parts[1]!;
        const src = parts[2]!;
        const timeStr = parts[parts.length - 2];
        const allocStr = parts[parts.length - 1];
        const timePercent = parseFloat(timeStr ?? "0");
        const allocPercent = parseFloat(allocStr ?? "0");

        if (!isNaN(timePercent) && !isNaN(allocPercent) && name !== "MAIN") {
          costCentres.push({
            name,
            module: module_,
            src,
            timePercent,
            allocPercent,
          });
        }
      }
    }
  }

  // Sort by time descending and take top 10
  const topCostCentres = costCentres
    .sort((a, b) => b.timePercent - a.timePercent)
    .slice(0, 10);

  return {
    success: true,
    totalTime,
    totalAlloc,
    topCostCentres,
  };
}

// ─── Tool handler ─────────────────────────────────────────────────────────────

const GHCUP_BIN = path.join(process.env.HOME ?? "/Users", ".ghcup", "bin");
const CABAL_BIN = path.join(process.env.HOME ?? "/Users", ".cabal", "bin");
const TOOL_PATH = `${GHCUP_BIN}:${CABAL_BIN}:${process.env.PATH}`;

export async function handleProfile(
  projectDir: string,
  args: { action: string; module_path?: string; executable?: string }
): Promise<string> {
  if (args.action === "suggest") {
    if (!args.module_path) {
      return JSON.stringify({
        success: false,
        error: "module_path is required for action 'suggest'",
      });
    }

    const absPath = path.resolve(projectDir, args.module_path);
    let code: string;
    try {
      code = await readFile(absPath, "utf-8");
    } catch {
      return JSON.stringify({ success: false, error: `File not found: ${args.module_path}` });
    }

    const suggestions = suggestOptimizations(code);
    return JSON.stringify({
      success: true,
      action: "suggest",
      module_path: args.module_path,
      suggestions,
      summary: suggestions.length === 0
        ? "No obvious performance issues detected"
        : `${suggestions.length} potential issue(s) found`,
    });
  }

  if (args.action === "time" || args.action === "heap") {
    const exe = args.executable ?? "main";
    const rtsFlag = args.action === "time" ? "-p" : "-hc";
    const cabalArgs = ["run", exe, "--enable-profiling", "--", `+RTS ${rtsFlag} -RTS`];

    return new Promise<string>((resolve) => {
      execFile(
        "cabal",
        cabalArgs,
        { cwd: projectDir, env: { ...process.env, PATH: TOOL_PATH }, timeout: 120_000 },
        async (error, stdout, stderr) => {
          const fullOutput = `${stdout}\n${stderr}`.trim();

          // Find the .prof file
          const profFile = path.join(projectDir, `${exe}.prof`);
          let profResult: ProfResult = { success: false, topCostCentres: [] };
          try {
            const profContent = await readFile(profFile, "utf-8");
            profResult = parseProfFile(profContent);
          } catch {
            // .prof file may not exist or may have different name
          }

          resolve(JSON.stringify({
            success: !error,
            action: args.action,
            ...(profResult.totalTime ? { totalTime: profResult.totalTime } : {}),
            ...(profResult.totalAlloc ? { totalAlloc: profResult.totalAlloc } : {}),
            topCostCentres: profResult.topCostCentres,
            raw: fullOutput.slice(0, 2000),
            ...(error ? { error: stderr.slice(0, 500) } : {}),
          }));
        }
      );
    });
  }

  return JSON.stringify({
    success: false,
    error: `Unknown action '${args.action}'. Valid actions: suggest, time, heap`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_profile",
    "Performance analysis for Haskell code. " +
      "Actions: 'suggest' for static heuristic analysis of a source file (no GHC needed); " +
      "'time' to run with GHC time profiling (+RTS -p) and show top cost centres; " +
      "'heap' to run with heap profiling (+RTS -hc). " +
      "For suggest, provide module_path. For time/heap, optionally provide executable name.",
    {
      action: z.enum(["suggest", "time", "heap"]).describe(
        '"suggest": static analysis for common performance issues. "time": GHC time profiling. "heap": GHC heap profiling.'
      ),
      module_path: z.string().optional().describe(
        'Module to analyze (required for suggest). Example: "src/Lib/Core.hs"'
      ),
      executable: z.string().optional().describe(
        'Executable name for time/heap profiling. Default: "main". Example: "my-app"'
      ),
    },
    async ({ action, module_path, executable }) => {
      const result = await handleProfile(ctx.getProjectDir(), { action, module_path, executable });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
