import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession, type GhciResult } from "../ghci-session.js";
import { parseQuickCheckOutput, parseScopeError } from "../parsers/quickcheck-parser.js";
import { parseEvalOutput } from "../parsers/eval-output-parser.js";
import {
  parseCabalModules,
  moduleToFilePath,
  getLibrarySrcDir,
} from "../parsers/cabal-parser.js";
import type { ToolContext } from "./registry.js";
import { suggestFunctionProperties } from "../laws/function-laws.js";

// Re-export for consumers
export type { QuickCheckResult } from "../parsers/quickcheck-parser.js";

let quickCheckAvailable: boolean | null = null;

/**
 * Ensure QuickCheck is imported in the GHCi session.
 * Caches the result so we only try once per session.
 */
async function ensureQuickCheck(session: GhciSession): Promise<boolean> {
  if (quickCheckAvailable === true) return true;

  const importResult = await session.execute("import Test.QuickCheck");
  if (importResult.success && !importResult.output.toLowerCase().includes("could not find module")) {
    quickCheckAvailable = true;
    return true;
  }

  const setResult = await session.execute(":set -package QuickCheck");
  if (setResult.success && !setResult.output.toLowerCase().includes("unknown package")) {
    const importRetry = await session.execute("import Test.QuickCheck");
    if (importRetry.success) {
      quickCheckAvailable = true;
      return true;
    }
  }

  quickCheckAvailable = false;
  return false;
}

/**
 * Reset QuickCheck availability check (call on session restart).
 */
export function resetQuickCheckState(): void {
  quickCheckAvailable = null;
  hiddenNames.clear();
}

/** Names hidden from QuickCheck import to resolve ambiguous occurrences. */
const hiddenNames: Set<string> = new Set();

/**
 * Re-import Test.QuickCheck with hiding clause for ambiguous names.
 */
async function reimportWithHiding(session: GhciSession): Promise<void> {
  if (hiddenNames.size === 0) {
    await session.execute("import Test.QuickCheck");
  } else {
    const hiding = [...hiddenNames].join(", ");
    await session.execute(`import Test.QuickCheck hiding (${hiding})`);
  }
}

/**
 * Function that loads all project modules into GHCi.
 * Injected as a dependency so it can be mocked in tests.
 */
export type LoadAllFn = (session: GhciSession) => Promise<boolean>;

/**
 * Create a loadAll function from a project directory.
 */
export function createLoadAllFromProjectDir(projectDir: string): LoadAllFn {
  return async (session: GhciSession) => {
    const cabalModules = await parseCabalModules(projectDir);
    const srcDir = await getLibrarySrcDir(projectDir);
    const paths = cabalModules.library.map((mod) =>
      moduleToFilePath(mod, srcDir)
    );
    if (paths.length > 0) {
      await session.loadModules(paths, cabalModules.library);
      return true;
    }
    return false;
  };
}

/**
 * Run a QuickCheck command with automatic scope resolution.
 * On "not in scope": loads all project modules and retries.
 * On "Ambiguous occurrence": hides conflicting names from QuickCheck import and retries.
 * Max 2 retries to avoid infinite loops.
 */
export async function runPropertyWithAutoResolve(
  session: GhciSession,
  command: string,
  loadAll?: LoadAllFn
): Promise<{ result: GhciResult; autoResolved: boolean }> {
  const MAX_RETRIES = 2;
  let lastResult = await session.execute(command);
  let autoResolved = false;

  for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
    const scopeErr = parseScopeError(lastResult.output);
    if (!scopeErr) break; // No scope error — done

    if (scopeErr.type === "not-in-scope" && loadAll) {
      // Load all project modules to bring everything into scope
      try {
        const loaded = await loadAll(session);
        if (loaded) {
          // Re-import QuickCheck (loadModules clears imports)
          await reimportWithHiding(session);
          lastResult = await session.execute(command);
          autoResolved = true;
        } else {
          break;
        }
      } catch {
        break; // Can't load modules — return original error
      }
    } else if (scopeErr.type === "ambiguous") {
      // Hide conflicting names and re-import
      for (const name of scopeErr.names) {
        hiddenNames.add(name);
      }
      await reimportWithHiding(session);
      lastResult = await session.execute(command);
      autoResolved = true;
    } else {
      break; // not-in-scope but no loadAll — can't auto-resolve
    }
  }

  return { result: lastResult, autoResolved };
}

/**
 * Run a QuickCheck property in GHCi.
 */
export async function handleQuickCheck(
  session: GhciSession,
  args: {
    property: string;
    tests?: number;
    verbose?: boolean;
    incremental?: boolean;
    function_name?: string;
  },
  ctx?: { getWorkflowState?: () => { activeModule: string | null; modules: Map<string, unknown> }; updateModuleProgress?: (path: string, updates: Record<string, unknown>) => void; getModuleProgress?: (path: string) => { propertiesPassed: string[]; propertiesFailed: string[]; functionsImplemented: number; functionsTotal: number } | undefined },
  projectDir?: string
): Promise<string> {
  const available = await ensureQuickCheck(session);
  if (!available) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: args.property,
      error:
        "QuickCheck not available. Add 'QuickCheck >= 2.14' to build-depends in the .cabal file, then run 'cabal build' and restart the GHCi session.",
    });
  }

  const trimmed = args.property.trim();
  if (trimmed.startsWith(":")) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: args.property,
      error: "Property cannot start with ':' (looks like a GHCi command)",
    });
  }
  if (trimmed.length > 2000) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: args.property,
      error: "Property too long (max 2000 characters)",
    });
  }

  // Property suggestion mode
  if (trimmed === "suggest" && args.function_name) {
    const typeResult = await session.typeOf(args.function_name);
    const typeStr = typeResult.success ? typeResult.output : "";
    const rawSuggestions = suggestPropertiesFromType(args.function_name, typeStr);

    // Validate each suggested property compiles by type-checking with :t
    const suggestions: Array<{ law: string; property: string }> = [];
    const rejected: Array<{ law: string; property: string; reason: string }> = [];
    for (const s of rawSuggestions) {
      const checkResult = await session.execute(`:t (${s.property})`);
      if (checkResult.success && !checkResult.output.toLowerCase().includes("error")) {
        suggestions.push(s);
      } else {
        rejected.push({ ...s, reason: "Property doesn't type-check" });
      }
    }

    return JSON.stringify({
      success: true,
      mode: "suggest",
      function: args.function_name,
      type: typeStr,
      suggestedProperties: suggestions,
      ...(rejected.length > 0 ? { rejectedProperties: rejected } : {}),
      hint: suggestions.length > 0
        ? `Found ${suggestions.length} valid propert(ies)${rejected.length > 0 ? ` (${rejected.length} rejected — didn't type-check)` : ""}. Run each with ghci_quickcheck.`
        : "No automatic suggestions — write a custom property based on the function's contract.",
    });
  }

  const maxTests = args.tests ?? 100;
  const checkFn = args.verbose ? "verboseCheckWith" : "quickCheckWith";
  // Normalize the property: ensure lambdas are wrapped in parentheses
  // to avoid GHCi parse errors with bare \x -> ... at top level.
  let normalizedProp = args.property;
  if (normalizedProp.startsWith("\\") && !normalizedProp.startsWith("(")) {
    normalizedProp = `(${normalizedProp})`;
  }
  const command = `${checkFn} (stdArgs { maxSuccess = ${maxTests} }) ${normalizedProp}`;

  // Run with automatic scope resolution (load_all on "not in scope", hiding on "Ambiguous")
  const loadAll = projectDir ? createLoadAllFromProjectDir(projectDir) : undefined;
  const { result, autoResolved } = await runPropertyWithAutoResolve(session, command, loadAll);
  const evalParsed = parseEvalOutput(result.output);
  let parsed = parseQuickCheckOutput(evalParsed.result, args.property);

  // If parsing failed and the output doesn't look like QC output at all,
  // retry once — the buffer may have had a stale entry from a previous command.
  if (
    parsed.error?.startsWith("Couldn't parse QuickCheck output") &&
    !evalParsed.result.includes("+++") &&
    !evalParsed.result.includes("***")
  ) {
    const retryResult = await session.execute(command);
    const retryParsed = parseEvalOutput(retryResult.output);
    parsed = parseQuickCheckOutput(retryParsed.result, args.property);
  }

  // Detect if the failure was a compilation error (type mismatch, not in scope)
  // vs a genuine logic failure (counterexample found).
  const isCompilationError = !parsed.success && (
    result.output.toLowerCase().includes("not in scope") ||
    result.output.toLowerCase().includes("couldn't match") ||
    result.output.toLowerCase().includes("no instance for") ||
    result.output.toLowerCase().includes("parse error") ||
    (parsed.error?.includes("Exception:") ?? false)
  );

  // Track in workflow state if incremental
  if (args.incremental && ctx?.getWorkflowState && ctx?.getModuleProgress) {
    let activeMod = ctx.getWorkflowState().activeModule;

    if (!activeMod) {
      const state = ctx.getWorkflowState();
      const entries = Array.from(state.modules.entries());
      if (entries.length > 0) {
        activeMod = entries[entries.length - 1]![0];
      }
    }

    if (activeMod) {
      const mod = ctx.getModuleProgress(activeMod);
      if (mod && ctx.updateModuleProgress) {
        if (parsed.success) {
          if (!mod.propertiesPassed.includes(args.property)) {
            ctx.updateModuleProgress(activeMod, {
              propertiesPassed: [...mod.propertiesPassed, args.property],
            });
          }
        } else if (!isCompilationError) {
          // Only track as failed if it's a genuine logic failure,
          // not a syntax/type error in the property expression
          if (!mod.propertiesFailed.includes(args.property)) {
            ctx.updateModuleProgress(activeMod, {
              propertiesFailed: [...mod.propertiesFailed, args.property],
            });
          }
        }
        // Compilation errors are NOT counted as failed properties
      }
    }
  }

  if (args.incremental) {
    return JSON.stringify({
      ...parsed,
      ...(isCompilationError ? { compilationError: true } : {}),
      ...(autoResolved ? { _autoResolved: true } : {}),
      incremental: true,
      hint: parsed.success
        ? "Incremental property passed. Continue implementing next function."
        : "Incremental property FAILED. Fix before continuing.",
    });
  }

  return JSON.stringify({
    ...parsed,
    ...(autoResolved ? { _autoResolved: true } : {}),
  });
}

/** Suggest QuickCheck properties based on a function's type signature. */
function suggestPropertiesFromType(
  funcName: string,
  typeStr: string
): Array<{ law: string; property: string }> {
  const suggestions: Array<{ law: string; property: string }> = [];

  // Domain-specific suggestions (kept for backward compatibility with HM project)
  if (/Subst\s*->\s*\w+\s*->\s*\w+/.test(typeStr) && funcName === "apply") {
    suggestions.push({
      law: "identity",
      property: `\\t -> ${funcName} emptySubst t == t`,
    });
  }
  if (funcName === "composeSubst" || funcName === "compose") {
    suggestions.push({
      law: "composition distributes over apply",
      property: `\\s1 s2 t -> apply (${funcName} s1 s2) t == apply s1 (apply s2 t)`,
    });
  }
  if (funcName === "unify" && typeStr.includes("Either")) {
    suggestions.push({
      law: "correctness",
      property: `\\t1 t2 -> case ${funcName} t1 t2 of { Right s -> apply s t1 == apply s t2; Left _ -> True }`,
    });
    suggestions.push({
      law: "reflexivity",
      property: `\\t -> case ${funcName} t t of { Right s -> s == emptySubst; Left _ -> False }`,
    });
  }

  // Generic heuristic-based suggestions from function-laws engine
  const genericSuggestions = suggestFunctionProperties(funcName, typeStr);
  for (const gs of genericSuggestions) {
    // Avoid duplicating suggestions that already exist
    if (!suggestions.some((s) => s.law === gs.law)) {
      suggestions.push({ law: gs.law, property: gs.property });
    }
  }

  return suggestions;
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_quickcheck",
    "Run a QuickCheck property in GHCi. The property should be a Haskell expression of type `Testable prop => prop`. " +
      "Returns structured results: pass/fail, test count, counterexample if any. " +
      "Requires QuickCheck to be available as a project dependency.",
    {
      property: z.string().describe(
        'QuickCheck property expression. Examples: "\\xs -> reverse (reverse xs) == (xs :: [Int])". ' +
          'Use "suggest" with function_name to get property suggestions.'
      ),
      tests: z.number().optional().describe("Number of tests to run (default 100)"),
      verbose: z.boolean().optional().describe("If true, print each test case (default false)"),
      incremental: z.boolean().optional().describe(
        "If true, this is an incremental check during implementation (FLOW 4 step 9). " +
          "Results are tracked in workflow state per-module."
      ),
      function_name: z.string().optional().describe(
        'Function just implemented. When property="suggest", returns suggested properties ' +
          "based on the function's type signature."
      ),
    },
    async ({ property, tests, verbose, incremental, function_name }) => {
      const session = await ctx.getSession();
      const result = await handleQuickCheck(
        session,
        { property, tests, verbose, incremental, function_name },
        ctx,
        ctx.getProjectDir()
      );
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}

/**
 * Run multiple QuickCheck properties in a single tool call.
 * Loads all modules first, then runs each property sequentially.
 */
export async function handleQuickCheckBatch(
  session: GhciSession,
  args: { properties: string[]; tests?: number; incremental?: boolean },
  ctx?: { getWorkflowState?: () => { activeModule: string | null; modules: Map<string, unknown> }; updateModuleProgress?: (path: string, updates: Record<string, unknown>) => void; getModuleProgress?: (path: string) => { propertiesPassed: string[]; propertiesFailed: string[]; functionsImplemented: number; functionsTotal: number } | undefined },
  projectDir?: string
): Promise<string> {
  if (args.properties.length === 0) {
    return JSON.stringify({ success: true, count: 0, results: [] });
  }

  // Load all modules first to ensure everything is in scope
  if (projectDir) {
    try {
      const loadAll = createLoadAllFromProjectDir(projectDir);
      await loadAll(session);
    } catch {
      // Non-fatal — individual properties will report scope errors
    }
  }

  // Ensure QuickCheck is available
  const available = await ensureQuickCheck(session);
  if (!available) {
    return JSON.stringify({
      success: false,
      count: 0,
      results: [],
      error: "QuickCheck not available. Add 'QuickCheck >= 2.14' to build-depends.",
    });
  }

  const results: Array<{ property: string; success: boolean; passed: number; error?: string; counterexample?: string }> = [];
  let allPassed = true;

  for (const property of args.properties) {
    const singleResult = await handleQuickCheck(
      session,
      { property, tests: args.tests, incremental: args.incremental },
      ctx,
      projectDir
    );
    const parsed = JSON.parse(singleResult);
    results.push({
      property,
      success: parsed.success,
      passed: parsed.passed ?? 0,
      ...(parsed.error ? { error: parsed.error } : {}),
      ...(parsed.counterexample ? { counterexample: parsed.counterexample } : {}),
    });
    if (!parsed.success) allPassed = false;
  }

  return JSON.stringify({
    success: allPassed,
    count: results.length,
    results,
  });
}

export function registerBatch(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_quickcheck_batch",
    "Run multiple QuickCheck properties in a single call. Loads all project modules first, " +
      "then runs each property sequentially. Returns an array of results. " +
      "Use this to reduce round-trips when testing multiple properties.",
    {
      properties: z.array(z.string()).describe(
        "Array of QuickCheck property expressions to test."
      ),
      tests: z.number().optional().describe("Number of tests per property (default 100)"),
      incremental: z.boolean().optional().describe(
        "If true, track results in workflow state per-module."
      ),
    },
    async ({ properties, tests, incremental }) => {
      const session = await ctx.getSession();
      const result = await handleQuickCheckBatch(
        session,
        { properties, tests, incremental },
        ctx,
        ctx.getProjectDir()
      );
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
