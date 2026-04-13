/**
 * Parser for QuickCheck output.
 *
 * Handles: pass, fail (with counterexample), exception, gave-up scenarios.
 */

export interface QuickCheckResult {
  success: boolean;
  passed: number;
  property: string;
  counterexample?: string;
  shrinks?: number;
  error?: string;
}

/**
 * Parse QuickCheck output into structured result.
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

  // Parse "gave up" — QuickCheck discarded too many tests
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

/**
 * Detect scope-related errors in GHCi output.
 * Returns structured info to enable auto-resolution (load_all or hiding).
 */
export interface ScopeError {
  type: "not-in-scope" | "ambiguous";
  names: string[];
}

export function parseScopeError(output: string): ScopeError | null {
  const lower = output.toLowerCase();

  // Check for "Ambiguous occurrence" first (more specific)
  const ambiguousMatches = output.matchAll(/[Aa]mbiguous occurrence\s+['\u2018](\S+?)['\u2019]/g);
  const ambiguousNames = [...ambiguousMatches].map((m) => m[1]!);
  if (ambiguousNames.length > 0) {
    return { type: "ambiguous", names: [...new Set(ambiguousNames)] };
  }

  // Check for "not in scope" / "Variable not in scope"
  if (lower.includes("not in scope")) {
    // GHC formats: "Variable not in scope: 'name'" or "Variable not in scope: name :: Type"
    const scopeMatches = output.matchAll(
      /not in scope:?\s*(?:['\u2018](\S+?)['\u2019]|(\w+)\s*::)/gi
    );
    const scopeNames = [...scopeMatches]
      .map((m) => m[1] ?? m[2])
      .filter((n): n is string => !!n);
    return { type: "not-in-scope", names: [...new Set(scopeNames)] };
  }

  return null;
}
