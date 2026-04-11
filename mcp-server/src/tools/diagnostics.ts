import { GhciSession } from "../ghci-session.js";
import { parseGhcErrors, GhcError } from "../parsers/error-parser.js";

interface HoleSummary {
  hole: string;
  expectedType: string;
  line: number;
  column: number;
  relevantBindings: string[];
  topFits: string[];
}

interface DiagnosticReport {
  success: boolean;
  module: string;
  compiled: boolean;
  errors: GhcError[];
  warnings: GhcError[];
  holes: HoleSummary[];
  summary: string;
}

/**
 * Full diagnostic check for a module. Runs two passes:
 *  1. Strict pass (-fno-defer-type-errors) to detect real type errors
 *  2. If no errors, deferred pass to collect typed-hole information
 *
 * Returns a unified report: errors, warnings, typed holes.
 */
export async function handleDiagnostics(
  session: GhciSession,
  args: { module_path: string }
): Promise<string> {
  const moduleName = inferModuleName(args.module_path);

  // Pass 1: Strict — find real errors
  await session.execute(":set -fno-defer-type-errors");
  const strictResult = await session.loadModule(args.module_path);
  await session.execute(":set -fdefer-type-errors");

  const allDiags = parseGhcErrors(strictResult.output);
  const errors = allDiags.filter((e) => e.severity === "error");

  // Separate typed-hole warnings from regular warnings
  // Typed holes have code GHC-88464 in strict mode they become errors,
  // but in deferred mode they're warnings. We need to check the raw output.
  const regularWarnings = allDiags.filter(
    (e) => e.severity === "warning" && e.code !== "GHC-88464"
  );

  if (errors.length > 0) {
    // Separate true errors from hole errors
    const holeErrors = errors.filter((e) => e.code === "GHC-88464");
    const realErrors = errors.filter((e) => e.code !== "GHC-88464");

    if (realErrors.length > 0) {
      // Real type errors — report and stop
      return JSON.stringify({
        success: false,
        module: moduleName,
        compiled: false,
        errors: realErrors,
        warnings: regularWarnings,
        holes: holeErrors.map((e) => ({
          hole: "_",
          expectedType: extractHoleType(e.message),
          line: e.line,
          column: e.column,
          relevantBindings: [],
          topFits: [],
        })),
        summary: `${realErrors.length} error(s), ${holeErrors.length} hole(s), ${regularWarnings.length} warning(s)`,
      } satisfies DiagnosticReport);
    }
  }

  // Pass 2: Deferred — collect hole details from warnings
  await session.execute(":set -fmax-valid-hole-fits=6");
  const deferredResult = await session.loadModule(args.module_path);
  const holes = parseHolesFromOutput(deferredResult.output);

  const report: DiagnosticReport = {
    success: true,
    module: moduleName,
    compiled: true,
    errors: [],
    warnings: regularWarnings,
    holes,
    summary: holes.length > 0
      ? `Compiled OK. ${holes.length} hole(s), ${regularWarnings.length} warning(s)`
      : regularWarnings.length > 0
        ? `Compiled OK. ${regularWarnings.length} warning(s)`
        : "Compiled OK. No issues.",
  };

  return JSON.stringify(report);
}

function inferModuleName(filePath: string): string {
  return filePath
    .replace(/^src\//, "")
    .replace(/\.hs$/, "")
    .replace(/\//g, ".");
}

function extractHoleType(message: string): string {
  const match = message.match(/Found hole:.*?::\s+(.+?)(?:\n|$)/);
  return match ? match[1]!.trim() : "unknown";
}

/**
 * Parse typed-hole warnings from deferred-mode GHCi output.
 * These appear as [GHC-88464] warnings with rich context.
 */
function parseHolesFromOutput(output: string): HoleSummary[] {
  const holes: HoleSummary[] = [];
  const lines = output.split("\n");

  let i = 0;
  while (i < lines.length) {
    const line = lines[i]!;

    // Find hole warning headers
    const headerMatch = line.match(
      /^.+?:(\d+):(\d+).*?warning:.*?\[GHC-88464\]/
    );
    if (!headerMatch) {
      i++;
      continue;
    }

    const holeLine = parseInt(headerMatch[1]!, 10);
    const holeCol = parseInt(headerMatch[2]!, 10);

    // Collect the full warning block
    let block = line + "\n";
    i++;
    while (i < lines.length) {
      const next = lines[i]!;
      // Next block starts with a new file:line:col header
      if (/^\S+:\d+:\d+/.test(next)) break;
      block += next + "\n";
      i++;
    }

    // Extract hole name and type
    const holeMatch = block.match(/Found hole:\s+(\S+)\s+::\s+(.+?)(?:\n|$)/);
    const holeName = holeMatch ? holeMatch[1]! : "_";
    const expectedType = holeMatch ? holeMatch[2]!.trim().replace(/\s*Where:.*$/, "") : "unknown";

    // Extract relevant bindings
    const bindings: string[] = [];
    const bindSection = block.match(
      /Relevant bindings include\n([\s\S]*?)(?:Valid hole fits|$)/
    );
    if (bindSection) {
      for (const bLine of bindSection[1]!.split("\n")) {
        const bMatch = bLine.trim().match(/^(\S+)\s+::\s+(.+?)\s+\(bound/);
        if (bMatch) {
          bindings.push(`${bMatch[1]} :: ${bMatch[2]}`);
        }
      }
    }

    // Extract top fits
    const fits: string[] = [];
    const fitSection = block.match(
      /Valid hole fits include\n([\s\S]*?)(?:\s*\||\s*$)/
    );
    if (fitSection) {
      for (const fLine of fitSection[1]!.split("\n")) {
        const fMatch = fLine.trim().match(/^(\S+)\s+::\s+(.+?)(?:\s+\(bound|\s*$)/);
        if (fMatch && !fLine.trim().startsWith("with ") && !fLine.trim().startsWith("(")) {
          fits.push(`${fMatch[1]} :: ${fMatch[2]}`);
        }
      }
    }

    holes.push({
      hole: holeName,
      expectedType,
      line: holeLine,
      column: holeCol,
      relevantBindings: bindings,
      topFits: fits,
    });
  }

  return holes;
}
