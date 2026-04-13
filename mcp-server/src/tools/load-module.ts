import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession, GhciResult } from "../ghci-session.js";
import type { ToolContext } from "./registry.js";
import { parseGhcErrors, GhcError } from "../parsers/error-parser.js";
import { categorizeWarnings, WarningAction } from "../parsers/warning-categorizer.js";
import {
  parseCabalModules,
  moduleToFilePath,
  getLibrarySrcDir,
} from "../parsers/cabal-parser.js";
import { parseHoleSummaries, HoleSummary } from "../parsers/hole-parser.js";
import { parseBrowseOutput, inferModuleName } from "../parsers/browse-parser.js";
import { derivePhase } from "../workflow-state.js";

export type { HoleSummary } from "../parsers/hole-parser.js";

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

// --- Shared dual-pass compilation helper ---

interface DualPassResult {
  errors: GhcError[];
  warnings: GhcError[];
  actions: WarningAction[];
  uncategorized: GhcError[];
  holes: HoleSummary[];
  rawOutput: string;
}

interface CompileOptions {
  /** Filter out GHC-32850 (-Wmissing-home-modules) noise from single-module loads. */
  filterMissingHomeModules?: boolean;
}

async function dualPassCompile(
  session: GhciSession,
  loadFn: () => Promise<GhciResult>,
  options?: CompileOptions
): Promise<DualPassResult> {
  const filterCodes = ["GHC-88464"];
  if (options?.filterMissingHomeModules) filterCodes.push("GHC-32850");
  const isFiltered = (e: GhcError) => filterCodes.includes(e.code ?? "");

  // Pass 1: strict — catch real type errors
  await session.execute(":set -fno-defer-type-errors");
  const strictResult = await loadFn();
  await session.execute(":set -fdefer-type-errors");

  const allDiags = parseGhcErrors(strictResult.output);
  const strictErrors = allDiags.filter(
    (e) => e.severity === "error" && !isFiltered(e)
  );
  const warnings = allDiags.filter(
    (e) => e.severity === "warning" && !isFiltered(e)
  );
  const { actions, uncategorized } = categorizeWarnings(warnings);

  if (strictErrors.length > 0) {
    return {
      errors: strictErrors,
      warnings,
      actions,
      uncategorized,
      holes: [],
      rawOutput: strictResult.output,
    };
  }

  // Pass 2: deferred — collect typed holes
  await session.execute(":set -fmax-valid-hole-fits=10");
  await session.execute(":set -frefinement-level-hole-fits=1");
  const deferredResult = await loadFn();
  await session.execute(":set -frefinement-level-hole-fits=0");
  const holes = parseHoleSummaries(deferredResult.output);

  const deferredDiags = parseGhcErrors(deferredResult.output);
  const deferredWarnings = deferredDiags.filter(
    (e) => e.severity === "warning" && !isFiltered(e)
  );
  const deferredCat = categorizeWarnings(deferredWarnings);

  return {
    errors: [],
    warnings: deferredWarnings,
    actions: deferredCat.actions,
    uncategorized: deferredCat.uncategorized,
    holes,
    rawOutput: deferredResult.output,
  };
}

// --- Single-pass helper (no dual-pass, used when diagnostics=false) ---

function singlePassDiagnostics(output: string, options?: CompileOptions): {
  errors: GhcError[];
  warnings: GhcError[];
  actions: WarningAction[];
  uncategorized: GhcError[];
} {
  const allDiags = parseGhcErrors(output);
  const errors = allDiags.filter((e) => e.severity === "error");
  const warnings = allDiags.filter((e) =>
    e.severity === "warning" &&
    !(options?.filterMissingHomeModules && e.code === "GHC-32850")
  );
  const { actions, uncategorized } = categorizeWarnings(warnings);
  return { errors, warnings, actions, uncategorized };
}

// --- Handler: reload ---

async function handleReload(
  session: GhciSession,
  runDiagnostics: boolean
): Promise<string> {
  if (runDiagnostics) {
    const dp = await dualPassCompile(session, () => session.reload());
    return buildResponse(
      dp.errors.length === 0, dp.errors, dp.warnings,
      dp.actions, dp.uncategorized, dp.holes, dp.rawOutput
    );
  }

  const result = await session.reload();
  const { errors, warnings, actions, uncategorized } = singlePassDiagnostics(result.output);
  return buildResponse(
    errors.length === 0, errors, warnings, actions, uncategorized, [], result.output
  );
}

// --- Handler: load single module ---

async function handleLoadSingle(
  session: GhciSession,
  modulePath: string,
  runDiagnostics: boolean
): Promise<string> {
  const filterOpts: CompileOptions = { filterMissingHomeModules: true };

  let errors: GhcError[], warnings: GhcError[], actions: WarningAction[];
  let uncategorized: GhcError[], holes: HoleSummary[], rawOutput: string;

  if (runDiagnostics) {
    const dp = await dualPassCompile(session, () => session.loadModule(modulePath), filterOpts);
    ({ errors, warnings, actions, uncategorized, holes, rawOutput } = dp);
  } else {
    const result = await session.loadModule(modulePath);
    const diags = singlePassDiagnostics(result.output, filterOpts);
    errors = diags.errors;
    warnings = diags.warnings;
    actions = diags.actions;
    uncategorized = diags.uncategorized;
    holes = [];
    rawOutput = result.output;
  }

  const success = errors.length === 0;

  // Auto-scope: bring loaded dependencies into interactive scope
  // so ghci_eval/ghci_type work without needing load_all
  let modulesInScope: string[] | undefined;
  if (success) {
    try {
      const showModResult = await session.showModules();
      const loaded = parseShowModules(showModResult.output);
      if (loaded.length > 0) {
        const starNames = loaded.map(n => `*${n}`).join(" ");
        await session.execute(`:m + ${starNames}`);
        modulesInScope = loaded;
      }
    } catch { /* non-fatal: scope info is best-effort */ }
  }

  return buildResponse(success, errors, warnings, actions, uncategorized, holes, rawOutput, undefined, modulesInScope);
}

// --- Handler: load all modules ---

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

  if (runDiagnostics) {
    const dp = await dualPassCompile(session, () =>
      session.loadModules(paths, cabalModules.library)
    );
    return buildResponse(
      dp.errors.length === 0, dp.errors, dp.warnings,
      dp.actions, dp.uncategorized, dp.holes, dp.rawOutput, paths
    );
  }

  const result = await session.loadModules(paths, cabalModules.library);
  const { errors, warnings, actions, uncategorized } = singlePassDiagnostics(result.output);
  return buildResponse(
    errors.length === 0, errors, warnings, actions, uncategorized, [], result.output, paths
  );
}

// --- Parse :show modules output ---

/**
 * Parse GHCi `:show modules` output to extract module names currently in scope.
 * Format: "Parser.Core    ( src/Parser/Core.hs, interpreted )"
 */
export function parseShowModules(output: string): string[] {
  return output
    .split("\n")
    .map((line) => line.match(/^(\S+)\s+\(/)?.[1])
    .filter((name): name is string => !!name);
}

// --- Response builder ---

function buildResponse(
  success: boolean,
  errors: GhcError[],
  warnings: GhcError[],
  warningActions: WarningAction[],
  uncategorizedWarnings: GhcError[],
  holes: HoleSummary[],
  raw: string,
  modules?: string[],
  modulesInScope?: string[]
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

  // Contextual next-step guidance
  let _nextStep: string | undefined;
  if (errors.length > 0) {
    _nextStep = "Fix the type errors above, then run ghci_load(diagnostics=true) again.";
  } else if (holes.length > 0) {
    _nextStep = `Implement ${holes.length} typed hole(s). Use ghci_type on subexpressions if the expected type is unclear.`;
  } else if (warningActions.length > 0) {
    _nextStep = `Fix ${warningActions.length} warning(s) — zero tolerance policy. See warningActions for specific fixes.`;
  } else if (success) {
    _nextStep = "Compilation clean. Test with ghci_eval or verify properties with ghci_quickcheck.";
  }

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
    ...(modulesInScope ? { modulesInScope } : {}),
    summary,
    raw,
    ...(_nextStep ? { _nextStep } : {}),
  });
}

/**
 * Extract "Not in scope" names from GHC error messages.
 * Looks for patterns like: Not in scope: 'isDigit' or Not in scope: type constructor or class 'Alternative'
 */
export function extractNotInScopeNames(errors: Array<{ message: string }>): string[] {
  const names = new Set<string>();
  for (const err of errors) {
    // Match: Not in scope: 'name' or Not in scope: type constructor or class 'Name'
    const matches = err.message.matchAll(/[Nn]ot in scope:.*?['\u2018]([^'\u2019]+)['\u2019]/g);
    for (const m of matches) {
      if (m[1]) names.add(m[1]);
    }
  }
  return [...names];
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_load",
    "Load or reload Haskell modules in GHCi. Returns parsed compilation errors and warnings. " +
      "Without module_path: reloads current modules (:r). " +
      "With module_path: loads that specific module. " +
      "With load_all=true: reads .cabal and loads ALL library modules at once (lighter than cabal_build).",
    {
      module_path: z.string().optional().describe(
        'Path to a module to load. If omitted, reloads current modules. Examples: "src/Lib.hs"'
      ),
      load_all: z.boolean().optional().describe(
        "If true, reads the .cabal file and loads ALL library modules into GHCi at once."
      ),
      diagnostics: z.boolean().optional().describe(
        "If true, runs dual-pass compilation (strict errors + typed holes) and categorizes warnings with suggested fix actions. " +
          "Defaults to true for module_path/load_all, false for plain reload."
      ),
    },
    async ({ module_path, load_all, diagnostics }) => {
      const session = await ctx.getSession();
      const result = await handleLoadModule(session, { module_path, load_all, diagnostics }, ctx.getProjectDir());

      // Update workflow state: set activeModule and track load results
      const parsed = JSON.parse(result);
      if (module_path) {
        const state = ctx.getWorkflowState();
        state.activeModule = module_path;
        ctx.updateModuleProgress(module_path, {
          lastLoad: {
            success: parsed.success,
            errors: parsed.errors?.length ?? 0,
            warnings: parsed.warnings?.length ?? 0,
          },
        });
      }
      ctx.logToolExecution("ghci_load", parsed.success);

      // After successful compilation, report which modules are in scope
      // This helps the LLM know if it needs load_all before running QuickCheck
      if (parsed.success) {
        try {
          const showModResult = await session.showModules();
          const inScope = parseShowModules(showModResult.output);
          if (inScope.length > 0) {
            parsed.modulesInScope = inScope;
          }
        } catch {
          // Non-fatal: scope info is informational
        }

        // Track function counts via :browse so the workflow tracker works
        // even when the developer writes implementations directly (no = undefined stubs)
        if (module_path) {
          try {
            const modName = inferModuleName(module_path);
            const browseResult = await session.execute(`:browse ${modName}`);
            // Safety guard: truncate if output is unexpectedly large
            if (browseResult.output.length > 50_000) {
              browseResult.output = browseResult.output.slice(0, 50_000) +
                "\n... (truncated — output too large)";
            }
            if (browseResult.success) {
              const defs = parseBrowseOutput(browseResult.output);
              const fnCount = defs.filter((d) => d.kind === "function").length;
              if (fnCount > 0) {
                const holeCount = parsed.holes?.length ?? 0;
                const implemented = Math.max(0, fnCount - holeCount);
                ctx.updateModuleProgress(module_path, {
                  functionsTotal: fnCount,
                  functionsImplemented: implemented,
                  phase: derivePhase({
                    functionsTotal: fnCount,
                    functionsImplemented: implemented,
                  } as any),
                });
              }
            }
          } catch {
            // Non-fatal: function tracking is informational
          }
        }
      }

      // Suggest imports for "Not in scope" errors (max 5 to avoid latency)
      if (parsed.errors?.length > 0) {
        try {
          const { lookupImportForName } = await import("./add-import.js");
          const scopeNames = extractNotInScopeNames(parsed.errors);
          const suggestions: Array<{ name: string; import: string; module: string }> = [];
          for (const name of scopeNames.slice(0, 5)) {
            const suggestion = await lookupImportForName(name);
            if (suggestion) suggestions.push(suggestion);
          }
          if (suggestions.length > 0) {
            parsed.importSuggestions = suggestions;
          }
        } catch {
          // Non-fatal: Hoogle may be unavailable
        }
      }

      // Inject notices and contextual guidance
      const notice = await ctx.getRulesNotice();
      if (notice) parsed._notice = notice;
      const { deriveGuidance } = await import("../workflow-state.js");
      const guidance = deriveGuidance(ctx.getWorkflowState(), "ghci_load");
      if (guidance.length > 0) parsed._guidance = guidance;
      return { content: [{ type: "text" as const, text: JSON.stringify(parsed) }] };
    }
  );
}
