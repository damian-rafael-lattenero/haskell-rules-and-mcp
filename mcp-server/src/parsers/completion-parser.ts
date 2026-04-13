/**
 * Parser for GHCi :complete output.
 *
 * Format:
 *   6 6 "ma"
 *   "map"
 *   "mapM"
 *   "mapM_"
 *   ...
 *
 * First line: totalCount displayedCount "prefix"
 * Remaining lines: one quoted completion per line.
 */

export interface CompletionResult {
  completions: string[];
  total: number;
  prefix: string;
}

export function parseCompletionOutput(output: string): CompletionResult {
  const lines = output.trim().split("\n");
  if (lines.length === 0 || lines[0]!.trim() === "") {
    return { completions: [], total: 0, prefix: "" };
  }

  const headerMatch = lines[0]!.match(/^(\d+)\s+(\d+)\s+"(.*)"/);
  const total = headerMatch ? parseInt(headerMatch[1]!, 10) : 0;
  const prefix = headerMatch ? headerMatch[3]! : "";

  const completions = lines
    .slice(1)
    .map((l) => l.trim())
    .filter((l) => l.length > 0)
    .map((l) => l.replace(/^"(.*)"$/, "$1"));

  return { completions, total, prefix };
}
