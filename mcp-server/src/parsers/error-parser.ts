export interface GhcError {
  file: string;
  line: number;
  column: number;
  endLine?: number;
  endColumn?: number;
  severity: "error" | "warning";
  code?: string;
  warningFlag?: string;
  message: string;
  expected?: string;
  actual?: string;
  context?: string;
}

/**
 * Parse GHC compiler output into structured error objects.
 *
 * GHC 9.12 error format example:
 *   src/Lib.hs:5:10: error: [GHC-83865]
 *       Couldn't match type 'Int' with 'String'
 *       Expected: String
 *         Actual: Int
 *       In the expression: length xs
 */
export function parseGhcErrors(output: string): GhcError[] {
  const errors: GhcError[] = [];

  // Phase 1: Find all error/warning header positions.
  // GHC 9.12 format: file:line:col[-endCol]: severity: [GHC-CODE] [-Wflag]
  const headerRegex =
    /^(.+?):(\d+):(\d+)(?:-(\d+))?: (error|warning):(?:\s*\[GHC-(\d+)\])?(?:\s*\[(-W[^\]]+)\])?[^\n]*/gm;

  interface HeaderInfo {
    match: RegExpExecArray;
    start: number;
    headerEnd: number;
  }
  const headers: HeaderInfo[] = [];
  let m;
  while ((m = headerRegex.exec(output)) !== null) {
    headers.push({ match: m, start: m.index, headerEnd: m.index + m[0].length });
  }

  // Phase 2: Extract body between consecutive headers (or end of string).
  for (let i = 0; i < headers.length; i++) {
    const { match, headerEnd } = headers[i]!;
    const [, file, line, col, endCol, severity, ghcCode, warnFlag] = match;

    const bodyEnd = i + 1 < headers.length ? headers[i + 1]!.start : output.length;
    const body = output.slice(headerEnd, bodyEnd).replace(/^\n/, "");

    const error: GhcError = {
      file: file!,
      line: parseInt(line!, 10),
      column: parseInt(col!, 10),
      severity: severity as "error" | "warning",
      message: body.trim(),
    };

    if (endCol) {
      error.endColumn = parseInt(endCol, 10);
    }

    if (ghcCode) {
      error.code = `GHC-${ghcCode}`;
    }

    if (warnFlag) {
      error.warningFlag = warnFlag;
    }

    // Extract Expected/Actual types
    const expectedMatch = body.match(
      /Expected(?:\s+type)?:\s+(.+)/i
    );
    const actualMatch = body.match(
      /Actual(?:\s+type)?:\s+(.+)/i
    );
    if (expectedMatch) error.expected = expectedMatch[1]!.trim();
    if (actualMatch) error.actual = actualMatch[1]!.trim();

    // Fallback: "Couldn\u2019t match expected type \u2018X\u2019 with actual type \u2018Y\u2019" format
    // GHC 9.12 uses Unicode quotes (\u2018/\u2019) and Unicode apostrophe (\u2019)
    if (!error.expected || !error.actual) {
      const couldntMatch = body.match(
        /Couldn['\u2019]t match (?:expected )?type\s+['\u2018](.+?)['\u2019]\s+with\s+(?:actual )?type\s+['\u2018](.+?)['\u2019]/
      );
      if (couldntMatch) {
        if (!error.expected) error.expected = couldntMatch[1]!.trim();
        if (!error.actual) error.actual = couldntMatch[2]!.trim();
      }
    }

    // Extract "In the expression:" context
    const contextMatch = body.match(
      /In the expression:\s+(.+)/i
    );
    if (contextMatch) error.context = contextMatch[1]!.trim();

    errors.push(error);
  }

  return errors;
}

/**
 * Format parsed errors into a readable summary.
 */
export function formatErrors(errors: GhcError[]): string {
  if (errors.length === 0) return "No errors found.";

  return errors
    .map((e) => {
      let msg = `${e.severity.toUpperCase()}${e.code ? ` [${e.code}]` : ""} at ${e.file}:${e.line}:${e.column}`;
      msg += `\n  ${e.message.split("\n")[0]}`;
      if (e.expected && e.actual) {
        msg += `\n  Expected: ${e.expected}`;
        msg += `\n  Actual:   ${e.actual}`;
      }
      if (e.context) {
        msg += `\n  In: ${e.context}`;
      }
      return msg;
    })
    .join("\n\n");
}
