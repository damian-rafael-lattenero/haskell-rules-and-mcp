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

  const maxTests = args.tests ?? 100;
  const checkFn = args.verbose ? "verboseCheckWith" : "quickCheckWith";
  const command = `${checkFn} (stdArgs { maxSuccess = ${maxTests} }) (${args.property})`;

  const result = await session.execute(command);
  const output = result.output;

  // Parse success: "+++ OK, passed 100 tests."
  const passMatch = output.match(/\+\+\+ OK, passed (\d+) tests?/);
  if (passMatch) {
    return JSON.stringify({
      success: true,
      passed: parseInt(passMatch[1]!, 10),
      property: args.property,
    } satisfies QuickCheckResult);
  }

  // Parse failure: "*** Failed! Falsifiable (after N tests and M shrinks):"
  const failMatch = output.match(
    /\*\*\* Failed!.*?\(after (\d+) tests?(?: and (\d+) shrinks?)?\):\s*\n([\s\S]*?)(?:\n\n|$)/
  );
  if (failMatch) {
    return JSON.stringify({
      success: false,
      passed: parseInt(failMatch[1]!, 10) - 1,
      property: args.property,
      shrinks: failMatch[2] ? parseInt(failMatch[2], 10) : 0,
      counterexample: failMatch[3]?.trim() ?? "unknown",
    } satisfies QuickCheckResult);
  }

  // Parse exception
  const exnMatch = output.match(/\*\*\* Failed! Exception:\s*['\u2018]?(.+?)['\u2019]?\s*\(after/);
  if (exnMatch) {
    return JSON.stringify({
      success: false,
      passed: 0,
      property: args.property,
      error: `Exception: ${exnMatch[1]!.trim()}`,
    } satisfies QuickCheckResult);
  }

  // Fallback: couldn't parse output
  return JSON.stringify({
    success: false,
    passed: 0,
    property: args.property,
    error: `Couldn't parse QuickCheck output: ${output.slice(0, 300)}`,
  } satisfies QuickCheckResult);
}
