import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseQuickCheckOutput } from "../parsers/quickcheck-parser.js";
import { parseEvalOutput } from "../parsers/eval-output-parser.js";
import type { ToolContext } from "./registry.js";

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
  ctx?: { getWorkflowState?: () => { activeModule: string | null }; updateModuleProgress?: (path: string, updates: Record<string, unknown>) => void; getModuleProgress?: (path: string) => { propertiesPassed: string[]; propertiesFailed: string[]; functionsImplemented: number; functionsTotal: number } | undefined }
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
    const suggestions = suggestPropertiesFromType(args.function_name, typeStr);
    return JSON.stringify({
      success: true,
      mode: "suggest",
      function: args.function_name,
      type: typeStr,
      suggestedProperties: suggestions,
      hint: suggestions.length > 0
        ? `Found ${suggestions.length} suggested propert(ies). Run each with ghci_quickcheck.`
        : "No automatic suggestions — write a custom property based on the function's contract.",
    });
  }

  const maxTests = args.tests ?? 100;
  const checkFn = args.verbose ? "verboseCheckWith" : "quickCheckWith";
  const command = `${checkFn} (stdArgs { maxSuccess = ${maxTests} }) (${args.property})`;

  const result = await session.execute(command);
  const evalParsed = parseEvalOutput(result.output);
  const parsed = parseQuickCheckOutput(evalParsed.result, args.property);

  // Track in workflow state if incremental
  if (args.incremental && ctx?.getWorkflowState && ctx?.getModuleProgress) {
    const activeMod = ctx.getWorkflowState().activeModule;
    if (activeMod) {
      const mod = ctx.getModuleProgress(activeMod);
      if (mod && ctx.updateModuleProgress) {
        if (parsed.success) {
          ctx.updateModuleProgress(activeMod, {
            propertiesPassed: [...mod.propertiesPassed, args.property],
          });
        } else {
          ctx.updateModuleProgress(activeMod, {
            propertiesFailed: [...mod.propertiesFailed, args.property],
          });
        }
      }
    }
  }

  if (args.incremental) {
    return JSON.stringify({
      ...parsed,
      incremental: true,
      hint: parsed.success
        ? "Incremental property passed. Continue implementing next function."
        : "Incremental property FAILED. Fix before continuing.",
    });
  }

  return JSON.stringify(parsed);
}

/** Suggest QuickCheck properties based on a function's type signature. */
function suggestPropertiesFromType(
  funcName: string,
  typeStr: string
): Array<{ law: string; property: string }> {
  const suggestions: Array<{ law: string; property: string }> = [];

  // Identity law: f emptyX x == x
  if (/Subst\s*->\s*\w+\s*->\s*\w+/.test(typeStr) && funcName === "apply") {
    suggestions.push({
      law: "identity",
      property: `\\t -> ${funcName} emptySubst t == t`,
    });
  }

  // Composition law
  if (funcName === "composeSubst" || funcName === "compose") {
    suggestions.push({
      law: "composition distributes over apply",
      property: `\\s1 s2 t -> apply (${funcName} s1 s2) t == apply s1 (apply s2 t)`,
    });
  }

  // Unification correctness
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

  // Generic: if it returns the same type as input, check identity-like properties
  const match = typeStr.match(/(\w+)\s*->\s*(\w+)$/);
  if (match && match[1] === match[2] && funcName !== "apply") {
    suggestions.push({
      law: "involution (if applicable)",
      property: `\\x -> ${funcName} (${funcName} x) == ${funcName} x`,
    });
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
        ctx
      );
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
