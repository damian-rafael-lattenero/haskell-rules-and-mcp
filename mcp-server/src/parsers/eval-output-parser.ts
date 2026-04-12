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

  for (const line of lines) {
    // New GHC warning/error header: <loc>:<line>:<col>: warning: ...
    if (/^\S+:\d+:\d+.*\bwarning\b:/.test(line)) {
      if (currentWarning) warningBlocks.push(currentWarning);
      currentWarning = [line];
      continue;
    }

    // Warning continuation: indented lines, source pointer lines, or blank lines
    if (
      currentWarning &&
      (line.startsWith("  ") || line.startsWith(" |") || line.trim() === "")
    ) {
      currentWarning.push(line);
      continue;
    }

    // Not part of a warning block
    if (currentWarning) {
      warningBlocks.push(currentWarning);
      currentWarning = null;
    }
    resultLines.push(line);
  }
  if (currentWarning) warningBlocks.push(currentWarning);

  return {
    result: resultLines.join("\n").trim(),
    warnings: warningBlocks.map((block) => block.join("\n").trim()),
    raw,
  };
}
