export interface TraceOutput {
  traceLines: string[];  // Lines from Debug.Trace (stripped of ">> " prefix)
  result: string;         // The actual evaluation result
  error?: string;         // Present if there's an exception or missing Show instance
}

/**
 * Parse GHCi evaluation output that may contain Debug.Trace lines.
 *
 * Lines starting with ">> " are trace output — they are collected with the
 * prefix stripped. Remaining non-empty lines form the result.
 * If the output contains "*** Exception:" or "No instance for (Show",
 * the error field is set.
 */
export function parseTraceOutput(output: string): TraceOutput {
  const lines = output.split("\n");
  const traceLines: string[] = [];
  const resultLines: string[] = [];

  for (const line of lines) {
    if (line.startsWith(">> ")) {
      traceLines.push(line.slice(3));
    } else if (line.trim() !== "") {
      resultLines.push(line);
    }
  }

  const result = resultLines.join("\n");
  const traceOutput: TraceOutput = { traceLines, result };

  if (/\*\*\* Exception:/.test(output)) {
    traceOutput.error = output.trim();
  } else if (/No instance for \(Show/.test(output)) {
    traceOutput.error = output.trim();
  }

  return traceOutput;
}
