export interface GhcError {
  file: string;
  line: number;
  column: number;
  endLine?: number;
  endColumn?: number;
  severity: "error" | "warning";
  code?: string;
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

  // Match error/warning blocks. GHC format:
  // file:line:col[-endCol]: severity: [GHC-CODE]
  const errorBlockRegex =
    /^(.+?):(\d+):(\d+)(?:-(\d+))?: (error|warning):(?:\s*\[GHC-(\d+)\])?\s*\n([\s\S]*?)(?=\n\S+:\d+:\d+|$)/gm;

  let match;
  while ((match = errorBlockRegex.exec(output)) !== null) {
    const [, file, line, col, endCol, severity, ghcCode, body] = match;
    const error: GhcError = {
      file: file!,
      line: parseInt(line!, 10),
      column: parseInt(col!, 10),
      severity: severity as "error" | "warning",
      message: body?.trim() ?? "",
    };

    if (endCol) {
      error.endColumn = parseInt(endCol, 10);
    }

    if (ghcCode) {
      error.code = `GHC-${ghcCode}`;
    }

    // Extract Expected/Actual types
    const expectedMatch = body?.match(
      /Expected(?:\s+type)?:\s+(.+)/i
    );
    const actualMatch = body?.match(
      /Actual(?:\s+type)?:\s+(.+)/i
    );
    if (expectedMatch) error.expected = expectedMatch[1]!.trim();
    if (actualMatch) error.actual = actualMatch[1]!.trim();

    // Extract "In the expression:" context
    const contextMatch = body?.match(
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
