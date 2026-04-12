import { GhciSession } from "../ghci-session.js";

interface QuickCheckResult {
  success: boolean;
  passed: number;
  property: string;
  counterexample?: string;
  shrinks?: number;
  error?: string;
}

let quickCheckAvailable: boolean | null = null;

/**
 * Ensure QuickCheck is imported in the GHCi session.
 * Caches the result so we only try once per session.
 */
async function ensureQuickCheck(session: GhciSession): Promise<boolean> {
  if (quickCheckAvailable === true) return true;

  // Try importing — works if QuickCheck is a project dependency
  const importResult = await session.execute("import Test.QuickCheck");
  if (importResult.success && !importResult.output.toLowerCase().includes("could not find module")) {
    quickCheckAvailable = true;
    return true;
  }

  // Try setting the package flag first (works if installed globally)
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
    } satisfies QuickCheckResult);
  }

  // Basic input validation
  const trimmed = args.property.trim();
  if (trimmed.startsWith(":")) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: args.property,
      error: "Property cannot start with ':' (looks like a GHCi command)",
    } satisfies QuickCheckResult);
  }
  if (trimmed.length > 2000) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: args.property,
      error: "Property too long (max 2000 characters)",
    } satisfies QuickCheckResult);
  }

  const maxTests = args.tests ?? 100;
  const checkFn = args.verbose ? "verboseCheckWith" : "quickCheckWith";
  const command = `${checkFn} (stdArgs { maxSuccess = ${maxTests} }) (${args.property})`;

  const result = await session.execute(command);
  return JSON.stringify(parseQuickCheckOutput(result.output, args.property));
}

/**
 * Parse QuickCheck output into structured result.
 * Exported for unit testing.
 */
export function parseQuickCheckOutput(
  output: string,
  property: string
): QuickCheckResult {
  // Parse success: "+++ OK, passed 100 tests."
  const passMatch = output.match(/\+\+\+ OK, passed (\d+) tests?/);
  if (passMatch) {
    return {
      success: true,
      passed: parseInt(passMatch[1]!, 10),
      property,
    };
  }

  // Parse exception (check BEFORE general failure — exception is more specific)
  const exnMatch = output.match(
    /\*\*\* Failed! Exception:\s*['\u2018]?(.+?)['\u2019]?\s*\(after/
  );
  if (exnMatch) {
    return {
      success: false,
      passed: 0,
      property,
      error: `Exception: ${exnMatch[1]!.trim()}`,
    };
  }

  // Parse failure: "*** Failed! Falsifiable (after N tests and M shrinks):"
  const failMatch = output.match(
    /\*\*\* Failed!.*?\(after (\d+) tests?(?: and (\d+) shrinks?)?\):\s*\n([\s\S]*?)(?:\n\n|$)/
  );
  if (failMatch) {
    return {
      success: false,
      passed: parseInt(failMatch[1]!, 10) - 1,
      property,
      shrinks: failMatch[2] ? parseInt(failMatch[2], 10) : 0,
      counterexample: failMatch[3]?.trim() ?? "unknown",
    };
  }

  // Parse "gave up" — QuickCheck discarded too many tests (common with ==> preconditions)
  const gaveUpMatch = output.match(
    /\*\*\* Gave up! Passed only (\d+) tests?;\s*(\d+) discarded/
  );
  if (gaveUpMatch) {
    return {
      success: false,
      passed: parseInt(gaveUpMatch[1]!, 10),
      property,
      error: `Gave up after ${gaveUpMatch[1]} tests (${gaveUpMatch[2]} discarded). Too many inputs rejected by precondition (==>). Consider relaxing the precondition or using a custom generator.`,
    };
  }

  // Fallback: couldn't parse output
  return {
    success: false,
    passed: 0,
    property,
    error: `Couldn't parse QuickCheck output: ${output.slice(0, 300)}`,
  };
}
