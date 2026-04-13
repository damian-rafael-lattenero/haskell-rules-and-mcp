import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { GhciSession } from "../ghci-session.js";
import { parseQuickCheckOutput } from "../parsers/quickcheck-parser.js";
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
  args: { property: string; tests?: number; verbose?: boolean }
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

  const maxTests = args.tests ?? 100;
  const checkFn = args.verbose ? "verboseCheckWith" : "quickCheckWith";
  const command = `${checkFn} (stdArgs { maxSuccess = ${maxTests} }) (${args.property})`;

  const result = await session.execute(command);
  return JSON.stringify(parseQuickCheckOutput(result.output, args.property));
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_quickcheck",
    "Run a QuickCheck property in GHCi. The property should be a Haskell expression of type `Testable prop => prop`. " +
      "Returns structured results: pass/fail, test count, counterexample if any. " +
      "Requires QuickCheck to be available as a project dependency.",
    {
      property: z.string().describe(
        'QuickCheck property expression. Examples: "\\xs -> reverse (reverse xs) == (xs :: [Int])", ' +
          '"\\x -> x + 0 == (x :: Int)"'
      ),
      tests: z.number().optional().describe("Number of tests to run (default 100)"),
      verbose: z.boolean().optional().describe("If true, print each test case (default false)"),
    },
    async ({ property, tests, verbose }) => {
      const session = await ctx.getSession();
      const result = await handleQuickCheck(session, { property, tests, verbose });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
