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

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i] ?? "";
    const lineNo = i + 1;

    if (/if\s+.+\s+then\s+True\s+else\s+False/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "redundant-if-true-false",
        "Replace `if cond then True else False` with `cond`"
      );
    }

    if (/if\s+.+\s+then\s+False\s+else\s+True/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "redundant-if-false-true",
        "Replace `if cond then False else True` with `not cond`"
      );
    }

    if (/\(\s*[A-Za-z_][A-Za-z0-9_']*\s*\)/.test(line) && !/^\s*(data|type|newtype)\b/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "redundant-parentheses",
        "Remove unnecessary parentheses around simple identifier"
      );
    }

    if (/\$\s*$/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "trailing-dollar",
        "Avoid trailing `$`; move expression to next line or remove `$`"
      );
    }

    if (/==\s*True\b/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "compare-to-true",
        "Replace `x == True` with `x`"
      );
    }

    if (/==\s*False\b/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "compare-to-false",
        "Replace `x == False` with `not x`"
      );
    }

    if (/\/=\s*True\b/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "neq-true",
        "Replace `x /= True` with `not x`"
      );
    }

    if (/\/=\s*False\b/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "neq-false",
        "Replace `x /= False` with `x`"
      );
    }

    if (/\+\+\s*\[\]/.test(line) || /\[\]\s*\+\+/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "redundant-list-append-empty",
        "Appending `[]` is redundant; remove it"
      );
    }

    if (/\+\+\s*""/.test(line) || /""\s*\+\+/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "redundant-string-append-empty",
        "Appending empty string is redundant; remove it"
      );
    }

    if (/length\s+\w+\s*==\s*0/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "length-eq-zero",
        "Prefer `null xs` over `length xs == 0`"
      );
    }

    if (/length\s+\w+\s*>\s*0/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "length-gt-zero",
        "Prefer `not (null xs)` over `length xs > 0`"
      );
    }

    if (/head\s+\w+/.test(line)) {
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

    if (/tail\s+\w+/.test(line)) {
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

    if (/fromJust\s+\w+/.test(line)) {
      pushSuggestion(
        suggestions,
        file,
        lineNo,
        1,
        "partial-fromJust",
        "Avoid partial `fromJust`; use pattern matching or maybe combinators",
        "warning"
      );
    }
  }

  return suggestions;
}
