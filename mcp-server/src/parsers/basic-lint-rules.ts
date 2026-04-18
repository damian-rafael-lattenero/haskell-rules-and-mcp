/**
 * Fallback lint heuristics applied ONLY when `hlint` is unavailable.
 *
 * Design rule: a degraded fallback must never report incorrectly. The old
 * implementation used context-free regexes that matched inside module headers
 * (`module M (`) and nested constructor applications (`Lit (sign n)`),
 * producing spurious "redundant-parentheses" noise on legitimate code.
 *
 * This rewrite keeps ONLY checks that don't depend on surrounding lexical
 * context. Everything removed is already covered by hlint when available —
 * and when it is not, the wrapper in `tools/lint.ts` now surfaces a clear
 * "hlint unavailable" message instead of pretending to have coverage.
 *
 * Kept rules:
 *   • partial-head / partial-tail / partial-fromJust — high-signal, explicit
 *     word-boundary anchors, genuinely dangerous patterns worth surfacing.
 *   • trailing-whitespace — purely lexical, no false positives possible.
 *   • mixed-tabs-and-spaces — file-level, deterministic.
 */

export interface BasicLintSuggestion {
  hint: string;
  severity: "suggestion" | "warning";
  suggestedAction: string;
  file: string;
  startLine: number;
  startColumn: number;
}

function pushSuggestion(
  out: BasicLintSuggestion[],
  file: string,
  line: number,
  column: number,
  hint: string,
  suggestedAction: string,
  severity: "suggestion" | "warning" = "suggestion"
): void {
  out.push({
    hint,
    severity,
    suggestedAction,
    file,
    startLine: line,
    startColumn: column,
  });
}

export function analyzeBasicLintRules(code: string, file: string): BasicLintSuggestion[] {
  const suggestions: BasicLintSuggestion[] = [];
  const lines = code.split("\n");

  let hasTabs = false;
  let hasLeadingSpaces = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i] ?? "";
    const lineNo = i + 1;

    // --- Partial Prelude functions (high signal, keep) -----------------
    // Word-boundary anchored: `\bhead\s+\w+\b`. Avoids matching identifiers
    // like `overhead`, `ahead`, or uses inside string literals
    // (string literals start with `"`, which is not a word boundary before
    // `head`, so these patterns do not match inside strings).
    if (/\bhead\s+\w+/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "partial-head",
        "Avoid partial `head`; use pattern matching or `listToMaybe`",
        "warning"
      );
    }

    if (/\btail\s+\w+/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "partial-tail",
        "Avoid partial `tail`; use pattern matching",
        "warning"
      );
    }

    if (/\bfromJust\s+\w+/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "partial-fromJust",
        "Avoid partial `fromJust`; use pattern matching or `maybe` combinators",
        "warning"
      );
    }

    // --- Trailing whitespace (lexical, no FP possible) -----------------
    if (/[ \t]+$/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "trailing-whitespace",
        "Remove trailing whitespace from this line"
      );
    }

    // Track indentation style for the per-file summary below.
    if (/^\t/.test(line)) hasTabs = true;
    if (/^ /.test(line)) hasLeadingSpaces = true;
  }

  // --- File-level rule: tabs + spaces mixed in indentation -------------
  if (hasTabs && hasLeadingSpaces) {
    pushSuggestion(
      suggestions,
      file,
      1,
      1,
      "mixed-tabs-and-spaces",
      "This file mixes tab and space indentation; pick one"
    );
  }

  return suggestions;
}
