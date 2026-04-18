/**
 * Basic, lexically-safe formatting heuristics applied ONLY when
 * fourmolu/ormolu are unavailable. Same philosophy as the lint fallback: never
 * claim coverage you cannot deliver. These rules only touch whitespace and
 * line endings — they do not reshape code structure, so they can be applied
 * safely without a Haskell parser.
 *
 * Reported issues include line/column, suggested fix, and a textual `fixed`
 * rendering of the corrected file, so callers can preview or write it back.
 * The envelope returned by `handleFormatBasic` sets `degraded: true` and
 * `gateEligible: false` — unlike a real formatter this does not unlock the
 * module-complete format gate.
 */

export interface BasicFormatIssue {
  kind:
    | "trailing-whitespace"
    | "crlf-line-endings"
    | "tabs-in-indentation"
    | "missing-final-newline";
  line: number;
  message: string;
}

export interface BasicFormatAnalysis {
  issues: BasicFormatIssue[];
  /** Content that results from applying every fix. Always safe to write. */
  fixed: string;
  /** True iff `fixed !== original`. */
  changed: boolean;
}

export function analyzeBasicFormatRules(code: string): BasicFormatAnalysis {
  const issues: BasicFormatIssue[] = [];
  const usesCrlf = code.includes("\r\n");
  // Normalize line endings first so subsequent per-line checks operate on
  // content without stray CRs.
  const normalized = usesCrlf ? code.replace(/\r\n/g, "\n") : code;
  if (usesCrlf) {
    issues.push({
      kind: "crlf-line-endings",
      line: 1,
      message: "File uses CRLF line endings; convert to LF",
    });
  }

  const lines = normalized.split("\n");
  const fixedLines: string[] = [];

  for (let i = 0; i < lines.length; i++) {
    let line = lines[i] ?? "";
    const lineNo = i + 1;

    // Tabs in indentation → flag, but leave them (conversion to spaces is
    // project-policy-dependent; we surface the issue without imposing width).
    if (/^\t/.test(line)) {
      issues.push({
        kind: "tabs-in-indentation",
        line: lineNo,
        message: "Tab character in indentation; consider spaces (typically 2)",
      });
    }

    // Trailing whitespace → always safe to strip.
    if (/[ \t]+$/.test(line)) {
      issues.push({
        kind: "trailing-whitespace",
        line: lineNo,
        message: "Trailing whitespace",
      });
      line = line.replace(/[ \t]+$/, "");
    }

    fixedLines.push(line);
  }

  // Re-join. `split("\n")` produces a trailing "" when the original ends in "\n";
  // we preserve that invariant. If the original did NOT end in "\n", we add one.
  let fixed = fixedLines.join("\n");
  const originallyEndedWithNewline = normalized.endsWith("\n");
  if (!originallyEndedWithNewline && normalized.length > 0) {
    issues.push({
      kind: "missing-final-newline",
      line: lines.length,
      message: "File does not end with a newline; append one",
    });
    fixed = fixed + "\n";
  }

  return {
    issues,
    fixed,
    changed: fixed !== code,
  };
}
