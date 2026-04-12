import { GhciSession } from "../ghci-session.js";
import { parseGhcErrors, GhcError } from "../parsers/error-parser.js";
import { categorizeWarnings, WarningAction } from "../parsers/warning-categorizer.js";
import {
  parseCabalModules,
  moduleToFilePath,
  getLibrarySrcDir,
} from "../parsers/cabal-parser.js";

export interface HoleSummary {
  hole: string;
  expectedType: string;
  line: number;
  column: number;
  relevantBindings: string[];
  topFits: string[];
}

export const loadModuleTool = {
  name: "ghci_load",
  description:
    "Load or reload a Haskell module in GHCi. " +
    "Without a module_path, reloads all currently loaded modules (:r). " +
    "With a module_path, loads that specific module (:l). " +
    "With load_all=true, reads the .cabal file and loads ALL library modules. " +
    "With diagnostics=true, runs dual-pass compilation (strict errors + typed holes) and categorizes warnings with suggested actions. " +
    "Returns parsed compilation errors, categorized warnings, and typed holes.",
  inputSchema: {
    type: "object" as const,
    properties: {
      module_path: {
        type: "string",
        description:
          'Optional path to a module to load. If omitted, reloads current modules. Examples: "src/Lib.hs", "src/MyModule.hs"',
      },
      load_all: {
        type: "boolean",
        description:
          "If true, reads the .cabal file and loads ALL library modules into GHCi at once.",
      },
      diagnostics: {
        type: "boolean",
        description:
          "If true, runs dual-pass compilation (strict + deferred) to separate real errors from typed holes, " +
          "and categorizes warnings with suggested fix actions. " +
          "Defaults to true for module_path/load_all, false for plain reload.",
      },
    },
    required: [],
  },
};

export async function handleLoadModule(
  session: GhciSession,
  args: { module_path?: string; load_all?: boolean; diagnostics?: boolean },
  projectDir?: string
): Promise<string> {
  // Determine if diagnostics should run
  const runDiagnostics = args.diagnostics ?? !!(args.module_path || args.load_all);

  if (args.load_all && projectDir) {
    return handleLoadAll(session, projectDir, runDiagnostics);
  }

  if (args.module_path) {
    return handleLoadSingle(session, args.module_path, runDiagnostics);
  }

  // Plain reload
  return handleReload(session, runDiagnostics);
}

async function handleReload(
  session: GhciSession,
  runDiagnostics: boolean
): Promise<string> {
  const result = await session.reload();
  const allDiags = parseGhcErrors(result.output);
  const errors = allDiags.filter((e) => e.severity === "error");
  const warnings = allDiags.filter((e) => e.severity === "warning");
  const { actions, uncategorized } = categorizeWarnings(warnings);

  if (runDiagnostics && errors.length === 0) {
    const holes = await collectHoles(session, result.output);
    return buildResponse(true, errors, warnings, actions, uncategorized, holes, result.output);
  }

  return buildResponse(
    errors.length === 0, errors, warnings, actions, uncategorized, [], result.output
  );
}

async function handleLoadSingle(
  session: GhciSession,
  modulePath: string,
  runDiagnostics: boolean
): Promise<string> {
  if (!runDiagnostics) {
    const result = await session.loadModule(modulePath);
    const allDiags = parseGhcErrors(result.output);
    const errors = allDiags.filter((e) => e.severity === "error");
    const warnings = allDiags.filter((e) => e.severity === "warning");
    const { actions, uncategorized } = categorizeWarnings(warnings);
    return buildResponse(
      errors.length === 0, errors, warnings, actions, uncategorized, [], result.output
    );
  }

  // Dual-pass: strict first
  await session.execute(":set -fno-defer-type-errors");
  const strictResult = await session.loadModule(modulePath);
  await session.execute(":set -fdefer-type-errors");

  const allDiags = parseGhcErrors(strictResult.output);
  const strictErrors = allDiags.filter((e) => e.severity === "error");
  const warnings = allDiags.filter(
    (e) => e.severity === "warning" && e.code !== "GHC-88464"
  );

  // Separate real errors from hole errors
  const realErrors = strictErrors.filter((e) => e.code !== "GHC-88464");
  const { actions, uncategorized } = categorizeWarnings(warnings);

  if (realErrors.length > 0) {
    return buildResponse(false, realErrors, warnings, actions, uncategorized, [], strictResult.output);
  }

  // Pass 2: deferred — collect holes
  await session.execute(":set -fmax-valid-hole-fits=6");
  const deferredResult = await session.loadModule(modulePath);
  const holes = parseHolesFromOutput(deferredResult.output);

  // Re-collect warnings from deferred pass (may have more detail)
  const deferredDiags = parseGhcErrors(deferredResult.output);
  const deferredWarnings = deferredDiags.filter(
    (e) => e.severity === "warning" && e.code !== "GHC-88464"
  );
  const deferredCat = categorizeWarnings(deferredWarnings);

  return buildResponse(
    true, [], deferredWarnings, deferredCat.actions, deferredCat.uncategorized, holes, deferredResult.output
  );
}

async function handleLoadAll(
  session: GhciSession,
  projectDir: string,
  runDiagnostics: boolean
): Promise<string> {
  const cabalModules = await parseCabalModules(projectDir);
  const srcDir = await getLibrarySrcDir(projectDir);
  const paths = cabalModules.library.map((mod) =>
    moduleToFilePath(mod, srcDir)
  );

  if (paths.length === 0) {
    return JSON.stringify({
      success: false,
      errors: [],
      warnings: [],
      warningActions: [],
      holes: [],
      modules: [],
      summary: "No library modules found in .cabal file",
      raw: "",
    });
  }

  const result = await session.loadModules(paths, cabalModules.library);
  const allDiags = parseGhcErrors(result.output);
  const errors = allDiags.filter((e) => e.severity === "error");
  const warnings = allDiags.filter((e) => e.severity === "warning");
  const { actions, uncategorized } = categorizeWarnings(warnings);

  if (runDiagnostics && errors.length === 0) {
    // For load_all, holes come from the deferred output (already deferred by default)
    const holes = parseHolesFromOutput(result.output);
    return buildResponse(
      true, errors, warnings, actions, uncategorized, holes, result.output, paths
    );
  }

  return buildResponse(
    errors.length === 0, errors, warnings, actions, uncategorized, [], result.output, paths
  );
}

function buildResponse(
  success: boolean,
  errors: GhcError[],
  warnings: GhcError[],
  warningActions: WarningAction[],
  uncategorizedWarnings: GhcError[],
  holes: HoleSummary[],
  raw: string,
  modules?: string[]
): string {
  const parts: string[] = [];
  if (errors.length > 0) parts.push(`${errors.length} error(s)`);
  if (holes.length > 0) parts.push(`${holes.length} hole(s)`);
  if (warningActions.length > 0) parts.push(`${warningActions.length} actionable warning(s)`);
  if (uncategorizedWarnings.length > 0) parts.push(`${uncategorizedWarnings.length} other warning(s)`);

  const summary = success
    ? parts.length > 0
      ? `Compiled OK. ${parts.join(", ")}`
      : modules
        ? `Loaded ${modules.length} modules`
        : "Compiled OK. No issues."
    : parts.join(", ");

  return JSON.stringify({
    success,
    errors,
    warnings,
    warningActions: warningActions.map((a) => ({
      category: a.category,
      suggestedAction: a.suggestedAction,
      confidence: a.confidence,
      file: a.warning.file,
      line: a.warning.line,
    })),
    holes,
    ...(modules ? { modules } : {}),
    summary,
    raw,
  });
}

// --- Hole parsing (moved from diagnostics.ts) ---

async function collectHoles(
  _session: GhciSession,
  deferredOutput: string
): Promise<HoleSummary[]> {
  return parseHolesFromOutput(deferredOutput);
}

function parseHolesFromOutput(output: string): HoleSummary[] {
  const holes: HoleSummary[] = [];
  const lines = output.split("\n");

  let i = 0;
  while (i < lines.length) {
    const line = lines[i]!;

    const headerMatch = line.match(
      /^.+?:(\d+):(\d+).*?warning:.*?\[GHC-88464\]/
    );
    if (!headerMatch) {
      i++;
      continue;
    }

    const holeLine = parseInt(headerMatch[1]!, 10);
    const holeCol = parseInt(headerMatch[2]!, 10);

    let block = line + "\n";
    i++;
    while (i < lines.length) {
      const next = lines[i]!;
      if (/^\S+:\d+:\d+/.test(next)) break;
      block += next + "\n";
      i++;
    }

    const holeMatch = block.match(/Found hole:\s+(\S+)\s+::\s+(.+?)(?:\n|$)/);
    const holeName = holeMatch ? holeMatch[1]! : "_";
    const expectedType = holeMatch
      ? holeMatch[2]!.trim().replace(/\s*Where:.*$/, "")
      : "unknown";

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

    const fits: string[] = [];
    const fitSection = block.match(
      /Valid hole fits include\n([\s\S]*?)(?:\s*\||\s*$)/
    );
    if (fitSection) {
      for (const fLine of fitSection[1]!.split("\n")) {
        const fMatch = fLine
          .trim()
          .match(/^(\S+)\s+::\s+(.+?)(?:\s+\(bound|\s*$)/);
        if (
          fMatch &&
          !fLine.trim().startsWith("with ") &&
          !fLine.trim().startsWith("(")
        ) {
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
