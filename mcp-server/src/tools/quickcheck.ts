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
import { type ToolContext, registerStrictTool } from "./registry.js";
import { suggestFunctionProperties } from "../laws/function-laws.js";
import { saveProperty } from "../property-store.js";
import { validatePropertyText } from "../parsers/property-validator.js";

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
  loadAll?: LoadAllFn,
  preCommands?: string[]
): Promise<{ result: GhciResult; autoResolved: boolean }> {
  const MAX_RETRIES = 2;

  // Execute pre-commands (let-bindings) then the main command
  const runPreAndCommand = async (): Promise<GhciResult> => {
    if (preCommands) {
      for (const pre of preCommands) {
        await session.execute(pre);
      }
    }
    return session.execute(command);
  };

  let lastResult = await runPreAndCommand();
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
          lastResult = await runPreAndCommand();
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
      lastResult = await runPreAndCommand();
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
    /** Preferred alias — takes precedence over `module` */
    module_path?: string;
    module?: string;
    /**
     * Semantic module this property is testing.
     * Distinct from module_path (load context).
     * Used for regression filtering in properties.json.
     */
    tests_module?: string;
    /**
     * Roundtrip mode: automatically generates roundtrip property from pretty/parse functions.
     * Format: "pretty_fn,parse_fn" or "pretty_fn,parse_fn,normalize_fn"
     */
    roundtrip?: string;
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

  // Roundtrip mode: generate property from pretty/parse functions
  let actualProperty = args.property;
  if (args.roundtrip) {
    const parts = args.roundtrip.split(",").map((s) => s.trim());
    if (parts.length < 2) {
      return JSON.stringify({
        success: false,
        passed: 0,
        error: "roundtrip parameter must be 'pretty_fn,parse_fn' or 'pretty_fn,parse_fn,normalize_fn'",
      });
    }
    const [prettyFn, parseFn, normalizeFn] = parts;
    if (normalizeFn) {
      actualProperty = `\\e -> fmap ${normalizeFn} (${parseFn} (${prettyFn} e)) == Just (${normalizeFn} e)`;
    } else {
      actualProperty = `\\e -> ${parseFn} (${prettyFn} e) == Just e`;
    }
  }

  const trimmed = actualProperty.trim();
  const validation = validatePropertyText(actualProperty);
  if (!validation.ok) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: actualProperty,
      error: validation.issues[0]?.message ?? "Invalid property",
      validationIssues: validation.issues,
    });
  }

  // Property suggestion mode
  if (trimmed === "suggest" && args.function_name) {
    const typeResult = await session.typeOf(args.function_name);
    const typeStr = typeResult.success ? typeResult.output : "";

    // Check if argument types have Arbitrary instances before suggesting properties
    const missingArbitrary = await checkArbitraryInstances(session, typeStr);
    if (missingArbitrary.length > 0) {
      return JSON.stringify({
        success: true,
        mode: "suggest",
        function: args.function_name,
        type: typeStr,
        missingArbitrary,
        suggestedProperties: [],
        _guidance: [
          "Generate Arbitrary instances first: " +
          missingArbitrary.map(t => `ghci_arbitrary(type_name="${t}")`).join(", "),
        ],
      });
    }

    // Type-level suggestions (existing)
    const rawSuggestions = suggestPropertiesFromType(args.function_name, typeStr);
    rawSuggestions.push(...suggestNameBasedProperties(args.function_name));

    // Constructor-level suggestions (NEW): inspect ADT input types
    try {
      const { suggestConstructorProperties } = await import("../laws/constructor-laws.js");
      const { parseInfoOutput } = await import("../parsers/type-parser.js");
      const { parseConstructors } = await import("../parsers/constructor-parser.js");
      const { splitTopLevelArrows } = await import("../laws/function-laws.js");

      const cleanedType = typeStr.replace(/^\S+\s*::\s*/, "").trim();
      const argTypes = splitTopLevelArrows(cleanedType);
      const returnType = argTypes[argTypes.length - 1] ?? "";

      for (let i = 0; i < argTypes.length - 1; i++) {
        const typeName = argTypes[i]!.trim().split(/\s/)[0]!;
        if (!/^[A-Z]/.test(typeName)) continue;
        try {
          const info = await session.infoOf(typeName);
          const parsed = parseInfoOutput(info.output);
          if ((parsed.kind === "data" || parsed.kind === "newtype") && parsed.definition) {
            const ctors = parseConstructors(parsed.definition);
            if (ctors.length > 0) {
              const otherArgs = argTypes.slice(0, -1).filter((_, idx) => idx !== i).map(a => a.trim());
              const ctorSuggestions = suggestConstructorProperties(
                args.function_name, returnType, otherArgs, ctors, typeName, i
              );
              rawSuggestions.push(...ctorSuggestions);
            }
          }
        } catch { /* skip if type inspection fails */ }
      }
    } catch { /* constructor-laws not available */ }

    // Validate each suggested property compiles by type-checking with :t
    const suggestions: Array<{ law: string; property: string }> = [];
    const rejected: Array<{ law: string; property: string; reason: string }> = [];
    for (const s of rawSuggestions) {
      const checkResult = await session.execute(`:t (${s.property})`);
      if (checkResult.success && !checkResult.output.toLowerCase().includes("error")) {
        suggestions.push(s);
      } else {
        // Extract specific reason from GHCi error
        const reason = extractRejectionReason(checkResult.output);
        rejected.push({ ...s, reason });
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
  let normalizedProp = actualProperty;
  if (normalizedProp.startsWith("\\") && !normalizedProp.startsWith("(")) {
    normalizedProp = `(${normalizedProp})`;
  }

  const typeCheckResult = await session.execute(`:t (${normalizedProp})`);
  const typeCheckOutput = typeCheckResult.output.toLowerCase();
  if (
    !typeCheckResult.success ||
    typeCheckOutput.includes("ambiguous") ||
    typeCheckOutput.includes("not in scope") ||
    typeCheckOutput.includes("couldn't match") ||
    typeCheckOutput.includes("parse error")
  ) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: actualProperty,
      error: "Property failed type-check validation before execution.",
      typecheckOutput: typeCheckResult.output,
      _nextStep: "Fix the property expression (types/scope/ambiguity), then re-run ghci_quickcheck.",
    });
  }

  // Use a let-binding to isolate the property from the quickCheck command.
  // This prevents quote/escaping issues when properties contain string literals.
  const propId = `__qcProp`;
  const preCommands = [`let ${propId} = ${normalizedProp}`];
  const command = `${checkFn} (stdArgs { maxSuccess = ${maxTests} }) ${propId}`;

  // Run with automatic scope resolution (load_all on "not in scope", hiding on "Ambiguous")
  const loadAll = projectDir ? createLoadAllFromProjectDir(projectDir) : undefined;
  const { result, autoResolved } = await runPropertyWithAutoResolve(session, command, loadAll, preCommands);
  const evalParsed = parseEvalOutput(result.output);
  let parsed = parseQuickCheckOutput(evalParsed.result, actualProperty);

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

  // Persist passing properties to disk for regression testing
  let propertySaved = false;
  let propertyStoreCount = 0;
  if (parsed.success && !isCompilationError && projectDir) {
    const activeMod = args.module_path ?? args.module ?? ctx?.getWorkflowState?.()?.activeModule ?? "unknown";
    try {
      await saveProperty(projectDir, {
        property: actualProperty, // Save the actual property (generated or original)
        module: activeMod,
        functionName: args.function_name,
        tests_module: args.tests_module,
      });
      propertySaved = true;
      const { getAllProperties } = await import("../property-store.js");
      propertyStoreCount = (await getAllProperties(projectDir)).length;
    } catch {
      // Non-fatal: persistence failure shouldn't break the tool
    }
  }

  // Track in workflow state (always, not just when incremental=true, so guidance is accurate)
  if (ctx?.getWorkflowState && ctx?.getModuleProgress) {
    let activeMod = args.module_path ?? args.module ?? ctx.getWorkflowState().activeModule;

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
          if (!mod.propertiesPassed.includes(actualProperty)) {
            ctx.updateModuleProgress(activeMod, {
              propertiesPassed: [...mod.propertiesPassed, actualProperty],
            });
          }
        } else if (!isCompilationError) {
          // Only track as failed if it's a genuine logic failure,
          // not a syntax/type error in the property expression
          if (!mod.propertiesFailed.includes(actualProperty)) {
            ctx.updateModuleProgress(activeMod, {
              propertiesFailed: [...mod.propertiesFailed, actualProperty],
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
      ...(propertySaved ? { _propertySaved: true, _propertyStoreCount: propertyStoreCount } : {}),
      ...(args.roundtrip ? { roundtrip: true, generated_property: actualProperty } : {}),
      incremental: true,
      hint: parsed.success
        ? "Incremental property passed. Continue implementing next function."
        : "Incremental property FAILED. Fix before continuing.",
      _nextStep: parsed.success
        ? "Incremental property passed. Continue implementing the next function."
        : "Incremental property FAILED. Fix the implementation before continuing.",
    });
  }

  return JSON.stringify({
    ...parsed,
    ...(autoResolved ? { _autoResolved: true } : {}),
    ...(propertySaved ? { _propertySaved: true, _propertyStoreCount: propertyStoreCount } : {}),
    ...(args.roundtrip ? { roundtrip: true, generated_property: actualProperty } : {}),
    ...(!parsed.success && !isCompilationError
      ? {
          _guidance: [
            parsed.counterexample
              ? `QuickCheck found counterexample: ${parsed.counterexample}. Use ghci_trace with the failing function under these inputs before changing the implementation.`
              : "QuickCheck failed. Use ghci_trace to inspect the failing execution path before changing the implementation.",
          ],
        }
      : {}),
    _nextStep: parsed.success
      ? "Property passed. Test more properties or move to the next function."
      : isCompilationError
        ? "Property has a type/syntax error. Fix the property expression and retry."
        : "Property FAILED. Debug with ghci_trace or fix the implementation.",
    // Hint for roundtrip failures — common cause is normalization
    ...(!parsed.success && !isCompilationError && (isLikelyRoundtrip(actualProperty) || args.roundtrip)
      ? { _hint: "Roundtrip property failed. Common cause: normalization differences (e.g. Neg (Lit n) vs Lit (negate n)). Consider using roundtrip parameter with normalize function: roundtrip='pretty,parse,normalize'" }
      : {}),
  });
}

/**
 * Detect if a property looks like a roundtrip test (parse/pretty, encode/decode, etc.)
 */
function isLikelyRoundtrip(property: string): boolean {
  const p = property.toLowerCase();
  return (
    (p.includes("parse") && p.includes("pretty")) ||
    (p.includes("decode") && p.includes("encode")) ||
    (p.includes("from") && p.includes("to")) ||
    p.includes("roundtrip")
  );
}

/**
 * Extract concrete argument types from a function's type signature and check
 * if they have Arbitrary instances by querying GHCi directly.
 * No hardcoded list — every type is verified dynamically.
 */
export async function checkArbitraryInstances(
  session: GhciSession,
  typeStr: string
): Promise<string[]> {
  if (!typeStr) return [];

  const parts = typeStr.split("::");
  const typePart = parts.length > 1 ? parts.slice(1).join("::").trim() : typeStr.trim();

  const segments = splitTopLevelArrows(typePart);
  const argTypes = segments.slice(0, -1);

  // Extract concrete type names (capitalized, not type variables)
  const concreteTypes = new Set<string>();
  for (const arg of argTypes) {
    const trimmed = arg.trim();
    const match = /^[\[(]?\s*([A-Z][\w']*)/.exec(trimmed);
    if (match) {
      concreteTypes.add(match[1]!);
    }
  }

  // Check each concrete type for Arbitrary instance
  const missing: string[] = [];
  for (const typeName of concreteTypes) {
    const result = await session.execute(`:t (arbitrary :: Gen ${typeName})`);
    if (!result.success || result.output.toLowerCase().includes("no instance") || result.output.toLowerCase().includes("not in scope")) {
      missing.push(typeName);
    }
  }

  return missing;
}

/** Split a type string on top-level arrows, respecting parentheses and brackets. */
function splitTopLevelArrows(typeStr: string): string[] {
  const segments: string[] = [];
  let depth = 0;
  let current = "";
  for (let i = 0; i < typeStr.length; i++) {
    const c = typeStr[i]!;
    if (c === "(" || c === "[") depth++;
    else if (c === ")" || c === "]") depth--;
    else if (c === "-" && typeStr[i + 1] === ">" && depth === 0) {
      segments.push(current);
      current = "";
      i++; // skip >
      continue;
    }
    current += c;
  }
  if (current.trim()) segments.push(current);
  return segments;
}

/**
 * Extract a specific rejection reason from GHCi error output.
 */
function extractRejectionReason(output: string): string {
  const lower = output.toLowerCase();
  // Check for missing Arbitrary instance
  const arbitraryMatch = /no instance for \(Arbitrary\s+(\S+)\)/i.exec(output);
  if (arbitraryMatch) return `Missing Arbitrary instance for ${arbitraryMatch[1]}`;
  // Check for missing Eq instance
  const eqMatch = /no instance for \(Eq\s+(\S+)\)/i.exec(output);
  if (eqMatch) return `Missing Eq instance for ${eqMatch[1]}`;
  // Check for not in scope
  if (lower.includes("not in scope")) return "Name not in scope — load all modules first";
  // Check for type mismatch
  if (lower.includes("couldn't match")) return "Type mismatch in property expression";
  // Generic fallback
  return "Property doesn't type-check";
}

/** Suggest QuickCheck properties based on a function's type signature. */
function suggestPropertiesFromType(
  funcName: string,
  typeStr: string
): Array<{ law: string; property: string }> {
  // All suggestions come from the generic engine — no domain-specific hardcoding
  return suggestFunctionProperties(funcName, typeStr).map((gs) => ({
    law: gs.law,
    property: gs.property,
  }));
}

function suggestNameBasedProperties(
  funcName: string
): Array<{ law: string; property: string }> {
  const n = funcName.toLowerCase();
  const out: Array<{ law: string; property: string }> = [];

  if (n.includes("parse")) {
    out.push({
      law: "parser totality (no exceptions)",
      property: `\\input -> seq (${funcName} input) True`,
    });
  }

  if (n.includes("simplify") || n.includes("normalize")) {
    out.push({
      law: "idempotence",
      property: `\\x -> ${funcName} (${funcName} x) == ${funcName} x`,
    });
  }

  return out;
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
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
      module: z.string().optional().describe(
        'Module path this property belongs to (e.g. "src/Parser/Run.hs"). ' +
          "Used for accurate property tracking in properties.json. " +
          "If omitted, uses the last module loaded with ghci_load."
      ),
      module_path: z.string().optional().describe(
        'Alias for module. Preferred spelling — module_path takes precedence when both are provided. ' +
          'Examples: "src/Expr/Eval.hs", "src/Parser/Run.hs"'
      ),
      tests_module: z.string().optional().describe(
        'Module this property is semantically testing. Distinct from module_path (load context). ' +
          'Used so regression filtering works by semantic target, not just load context. ' +
          'Example: if Arbitrary lives in src/Expr/Syntax.hs but the property tests Expr.Eval, ' +
          'set module_path="src/Expr/Syntax.hs" and tests_module="src/Expr/Eval.hs".'
      ),
      roundtrip: z.string().optional().describe(
        'Roundtrip mode: auto-generates roundtrip property from pretty/parse functions. ' +
          'Format: "pretty_fn,parse_fn" or "pretty_fn,parse_fn,normalize_fn". ' +
          'Examples: roundtrip="pretty,parse" generates "\\e -> parse (pretty e) == Just e". ' +
          'With normalize: roundtrip="pretty,parse,normalize" generates ' +
          '"\\e -> fmap normalize (parse (pretty e)) == Just (normalize e)". ' +
          'Useful for testing serialization/deserialization roundtrips.'
      ),
    },
    async ({ property, tests, verbose, incremental, function_name, module: mod, module_path, tests_module, roundtrip }) => {
      const session = await ctx.getSession();
      const result = await handleQuickCheck(
        session,
        { property, tests, verbose, incremental, function_name, module: mod, module_path, tests_module, roundtrip },
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
  args: { properties: string[]; tests?: number; incremental?: boolean; module?: string; module_path?: string; tests_module?: string },
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
      { property, tests: args.tests, incremental: args.incremental, module: args.module, module_path: args.module_path, tests_module: args.tests_module },
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
  registerStrictTool(server, ctx, 
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
      module: z.string().optional().describe(
        'Module path these properties belong to (e.g. "src/Parser/Run.hs"). Used for accurate property tracking.'
      ),
      module_path: z.string().optional().describe(
        'Alias for module. Preferred spelling — takes precedence when both are provided.'
      ),
      tests_module: z.string().optional().describe(
        'Module these properties are semantically testing. Stored in properties.json so ' +
          'ghci_regression can filter by the correct module. ' +
          'Example: tests_module="src/Expr/Eval.hs" when testing Eval functions, ' +
          'even if module_path points to Syntax.hs where Arbitrary lives.'
      ),
      auto_collect: z.boolean().optional().describe(
        "If true and properties array is empty, auto-collects all saved properties from properties.json. " +
        "Use with module to filter by module."
      ),
    },
    async ({ properties, tests, incremental, module: mod, module_path, tests_module, auto_collect }) => {
      const resolvedMod = module_path ?? mod;
      let propsToRun = properties;
      // Auto-collect from property store if requested
      if (auto_collect && (!properties || properties.length === 0)) {
        try {
          const { getAllProperties, getModuleProperties } = await import("../property-store.js");
          const stored = resolvedMod
            ? await getModuleProperties(ctx.getProjectDir(), resolvedMod)
            : await getAllProperties(ctx.getProjectDir());
          propsToRun = stored.map(p => p.property);
        } catch { /* fallback to empty */ }
      }
      const session = await ctx.getSession();
      const result = await handleQuickCheckBatch(
        session,
        { properties: propsToRun, tests, incremental, module: mod, module_path, tests_module },
        ctx,
        ctx.getProjectDir()
      );
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
