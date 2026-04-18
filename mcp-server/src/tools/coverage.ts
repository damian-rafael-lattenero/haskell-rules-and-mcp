import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { execFile } from "node:child_process";
import { readdir, readFile, stat } from "node:fs/promises";
import path from "node:path";
import { type ToolContext, registerStrictTool } from "./registry.js";

export function parseCoverage(stdout: string): Array<{ metric: string; percent: number; fraction?: string }> {
  const rows: Array<{ metric: string; percent: number; fraction?: string }> = [];
  const lines = stdout.split(/\r?\n/);
  // Match both the cabal-embedded format and the explicit `hpc report` format.
  // HPC lines look like:
  //   " 100% expressions used (21/21)"
  //   " 50 % boolean coverage (1/2)"
  const hpcLine = /^\s*(\d+(?:\.\d+)?)\s*%\s+(.+?)(?:\s+\((\d+\/\d+)\))?\s*$/;
  for (const line of lines) {
    const m = line.match(hpcLine);
    if (!m) continue;
    const percent = Number(m[1]);
    if (!Number.isFinite(percent)) continue;
    rows.push({
      percent,
      metric: m[2]!.trim(),
      ...(m[3] ? { fraction: m[3] } : {}),
    });
  }
  return rows;
}

/**
 * Parse the summary row of `hpc markup`'s `hpc_index.html`. HPC renders a
 * `<table>` where the bottom row is "Program Coverage Total" with columns for
 * expressions / boolean / alternatives / local decls / top-level decls. Each
 * cell has the shape `<td>NN%</td><td>used/total</td>` (or percentages with
 * a colored tick). We extract the metric label from the column headers in
 * order and pair with the percent cells in the total row.
 *
 * Input is untrusted file content — we do not execute any script from it,
 * only run regexes over the text. Tests cover malformed/short inputs.
 */
export function parseHpcIndexHtml(html: string): Array<{ metric: string; percent: number; fraction?: string }> {
  // Strip tags to plain text rows. HPC's index HTML is simple and predictable.
  const text = html.replace(/<[^>]*>/g, "\n").replace(/&nbsp;/g, " ");
  const rows: Array<{ metric: string; percent: number; fraction?: string }> = [];
  // Known metric labels in HPC's index output, in the order HPC writes them.
  const metricCandidates = [
    "expressions",
    "boolean coverage",
    "guards",
    "'if' conditions",
    "qualifiers",
    "alternatives",
    "local declarations",
    "top-level declarations",
  ];
  // Very tolerant: look for NN% followed later by a fraction (a/b).
  const percentRe = /(\d+(?:\.\d+)?)\s*%/g;
  const fractionRe = /(\d+)\s*\/\s*(\d+)/g;
  const percents = [...text.matchAll(percentRe)].map((m) => Number(m[1]));
  const fractions = [...text.matchAll(fractionRe)].map((m) => `${m[1]}/${m[2]}`);
  // HPC index typically has one percent + one fraction per metric. We zip
  // conservatively — if the counts don't align, fall back to percents only.
  for (let i = 0; i < Math.min(percents.length, metricCandidates.length); i++) {
    const pct = percents[i]!;
    if (!Number.isFinite(pct)) continue;
    rows.push({
      percent: pct,
      metric: metricCandidates[i]!,
      ...(fractions[i] ? { fraction: fractions[i]! } : {}),
    });
  }
  return rows;
}

function execFileAsync(
  cmd: string,
  args: string[],
  options: { cwd?: string; timeout?: number; env?: NodeJS.ProcessEnv }
): Promise<{ code: number; stdout: string; stderr: string; error?: Error }> {
  return new Promise((resolve) => {
    execFile(cmd, args, options, (error, stdout, stderr) => {
      resolve({
        code: error?.code === undefined ? (error ? 1 : 0) : (typeof error.code === "number" ? error.code : 1),
        stdout: stdout ?? "",
        stderr: stderr ?? "",
        error: error ?? undefined,
      });
    });
  });
}

/**
 * Recursively find all `.tix` files under a directory.
 * Used to locate HPC coverage data produced by `cabal test --enable-coverage`.
 * No symlink following (path stays inside the project).
 */
async function findTixFiles(dir: string, maxDepth = 10): Promise<string[]> {
  const tix: string[] = [];
  async function walk(current: string, depth: number): Promise<void> {
    if (depth > maxDepth) return;
    let entries: string[];
    try {
      entries = await readdir(current);
    } catch {
      return;
    }
    for (const entry of entries) {
      const full = path.join(current, entry);
      let st;
      try {
        st = await stat(full);
      } catch {
        continue;
      }
      if (st.isDirectory()) {
        await walk(full, depth + 1);
      } else if (st.isFile() && entry.endsWith(".tix")) {
        tix.push(full);
      }
    }
  }
  await walk(dir, 0);
  return tix;
}

/** Find any `hpc_index.html` under a directory (cabal's coverage report). */
async function findHpcIndexHtml(dir: string, maxDepth = 10): Promise<string[]> {
  const found: string[] = [];
  async function walk(current: string, depth: number): Promise<void> {
    if (depth > maxDepth) return;
    let entries: string[];
    try {
      entries = await readdir(current);
    } catch {
      return;
    }
    for (const entry of entries) {
      const full = path.join(current, entry);
      let st;
      try { st = await stat(full); } catch { continue; }
      if (st.isDirectory()) {
        await walk(full, depth + 1);
      } else if (st.isFile() && entry === "hpc_index.html") {
        found.push(full);
      }
    }
  }
  await walk(dir, 0);
  return found;
}

export async function handleCoverage(
  projectDir: string,
  args: { min_percent?: number; timeout_ms?: number }
): Promise<string> {
  const timeout = args.timeout_ms ?? 180_000;
  const testRun = await execFileAsync(
    "cabal",
    ["test", "--enable-coverage"],
    { cwd: projectDir, timeout, env: process.env }
  );
  const cabalOutput = `${testRun.stdout}\n${testRun.stderr}`.trim();

  // 1st pass: cabal's own stdout.
  let metrics = parseCoverage(cabalOutput);
  let reportSource: "cabal-test" | "hpc-report" | "hpc-html" = "cabal-test";
  let hpcReport: string | undefined;
  let htmlIndexPath: string | undefined;

  const distDir = path.join(projectDir, "dist-newstyle");

  // 2nd pass: `hpc report <latest.tix>` if cabal didn't emit a parseable block.
  if (metrics.length === 0 && testRun.code === 0) {
    const tixFiles = await findTixFiles(distDir).catch(() => [] as string[]);
    if (tixFiles.length > 0) {
      const tixWithStats = await Promise.all(
        tixFiles.map(async (t) => {
          try {
            const s = await stat(t);
            return { path: t, mtime: s.mtimeMs };
          } catch {
            return { path: t, mtime: 0 };
          }
        })
      );
      tixWithStats.sort((a, b) => b.mtime - a.mtime);
      const latestTix = tixWithStats[0]!.path;
      const report = await execFileAsync(
        "hpc",
        ["report", latestTix],
        { cwd: projectDir, timeout: 60_000, env: process.env }
      );
      if (report.code === 0) {
        hpcReport = `${report.stdout}\n${report.stderr}`.trim();
        metrics = parseCoverage(hpcReport);
        if (metrics.length > 0) reportSource = "hpc-report";
      }
    }
  }

  // 3rd pass: cabal sometimes only writes the HTML markup (no tix left on
  // disk, or the tix is empty). Parse `hpc_index.html` directly as a last
  // resort — purely file-local, no commands executed from its content.
  if (metrics.length === 0 && testRun.code === 0) {
    const htmlIndices = await findHpcIndexHtml(distDir).catch(() => [] as string[]);
    if (htmlIndices.length > 0) {
      const withStats = await Promise.all(
        htmlIndices.map(async (p) => {
          try { const s = await stat(p); return { path: p, mtime: s.mtimeMs }; }
          catch { return { path: p, mtime: 0 }; }
        })
      );
      withStats.sort((a, b) => b.mtime - a.mtime);
      const latest = withStats[0]!.path;
      try {
        const html = await readFile(latest, "utf-8");
        const htmlMetrics = parseHpcIndexHtml(html);
        if (htmlMetrics.length > 0) {
          metrics = htmlMetrics;
          reportSource = "hpc-html";
          htmlIndexPath = latest;
        }
      } catch {
        // non-fatal — falls through to the "no metrics" hint
      }
    }
  }

  const overall = metrics.length > 0 ? Math.min(...metrics.map((m) => m.percent)) : undefined;
  const minPercent = args.min_percent;
  const meetsThreshold = minPercent === undefined || (overall !== undefined && overall >= minPercent);

  if (testRun.error) {
    return JSON.stringify({
      success: false,
      command: "cabal test --enable-coverage",
      error: testRun.error.message,
      output: cabalOutput,
      metrics,
      ...(overall !== undefined ? { overallPercent: overall } : {}),
    });
  }

  // Actionable hint when no source yielded metrics. HPC only emits tix when
  // the test-suite's executable is compiled with `-fhpc`; cabal does this for
  // the library under test automatically on `--enable-coverage`, but the
  // test-suite itself may need the flag if it also has modules to measure.
  const noMetricsHint =
    "No coverage metrics were parseable from cabal stdout, `hpc report`, or `hpc_index.html`. " +
    "Ensure the test-suite's `build-depends` includes the library under test so HPC instruments it, " +
    "or add `ghc-options: -fhpc` to the test-suite stanza to force instrumentation. " +
    "If the issue persists, inspect `dist-newstyle/` for tix or hpc-html artifacts manually.";

  return JSON.stringify({
    success: true,
    command: "cabal test --enable-coverage",
    reportSource,
    metrics,
    ...(overall !== undefined ? { overallPercent: overall } : {}),
    ...(minPercent !== undefined ? { minPercent, meetsThreshold } : {}),
    ...(hpcReport ? { hpcReport } : {}),
    ...(htmlIndexPath ? { htmlIndexPath } : {}),
    summary:
      overall === undefined
        ? "Coverage run completed but no parseable coverage percentages were found."
        : `Coverage run completed. Lowest reported metric: ${overall.toFixed(2)}% (source: ${reportSource}).`,
    ...(overall === undefined ? { _hint: noMetricsHint } : {}),
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
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
