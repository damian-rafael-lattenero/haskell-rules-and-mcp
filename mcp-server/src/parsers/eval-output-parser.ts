export interface ParsedEvalOutput {
  result: string;
  warnings: string[];
  raw: string;
}

/**
 * Parse GHCi evaluation output to separate GHC warnings from the actual result.
 *
 * GHC often prepends type-defaulting or other warnings to eval output:
 *   <interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]
 *       Defaulting the type variable ...
 *   42
 *
 * This function strips those warning blocks and returns the clean result.
 */
export function parseEvalOutput(raw: string): ParsedEvalOutput {
  const lines = raw.split("\n");
  const warningBlocks: string[][] = [];
  const resultLines: string[] = [];
  let currentWarning: string[] | null = null;
  let sawBlankInWarning = false;

  for (const line of lines) {
    // New GHC warning/error header: <loc>:<line>:<col>: warning: ...
    if (/^\S+:\d+:\d+.*\bwarning\b:/.test(line)) {
      if (currentWarning) warningBlocks.push(currentWarning);
      currentWarning = [line];
      sawBlankInWarning = false;
      continue;
    }

    // Warning continuation: indented lines, source pointer lines, or blank lines
    if (currentWarning) {
      if (line.trim() === "") {
        sawBlankInWarning = true;
        currentWarning.push(line);
        continue;
      }
      // After a blank line in a warning, only deeply-indented lines (4+ spaces)
      // or source pointers are continuations. Shallowly-indented lines are likely
      // eval results (e.g. pretty-printed data structures).
      const isContinuation = sawBlankInWarning
        ? line.startsWith("    ") || line.startsWith(" |")
        : line.startsWith("  ") || line.startsWith(" |");

      if (isContinuation) {
        currentWarning.push(line);
        continue;
      }
    }

    // Not part of a warning block
    if (currentWarning) {
      warningBlocks.push(currentWarning);
      currentWarning = null;
      sawBlankInWarning = false;
    }
    resultLines.push(line);
  }
  if (currentWarning) warningBlocks.push(currentWarning);

  // Trim only leading/trailing blank lines, preserving leading spaces on first content line
  const joined = resultLines.join("\n");
  const trimmed = joined.replace(/^(\s*\n)+/, "").replace(/\s+$/, "");

  return {
    result: trimmed,
    warnings: warningBlocks.map((block) => block.join("\n").trim()),
    raw,
  };
}
