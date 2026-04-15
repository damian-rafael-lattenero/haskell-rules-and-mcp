import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { GhciSession } from "../ghci-session.js";
import { parseTraceOutput } from "../parsers/trace-parser.js";
import type { ToolContext } from "./registry.js";

export async function handleTrace(
  session: GhciSession,
  args: { expression: string; trace_points?: string[]; parser_mode?: boolean }
): Promise<string> {
  // Step 1: Import Debug.Trace
  await session.execute("import Debug.Trace");

  // Step 2: Build the wrapped expression
  let wrapped: string;

  if (args.trace_points && args.trace_points.length > 0) {
    // Wrap each trace_point around the expression, innermost first
    wrapped = args.expression;
    for (const point of args.trace_points) {
      wrapped = `trace (">> ${point} = " ++ show (${point})) (${wrapped})`;
    }
  } else {
    // No trace points — use traceShowId
    wrapped = `traceShowId (${args.expression})`;
  }

  // Step 3: Execute the wrapped expression
  const result = await session.execute(wrapped);

  // Step 4: Parse the output
  const parsed = parseTraceOutput(result.output);

  // Step 5: Build the response with enhanced parser visualization
  const response: Record<string, unknown> = {
    success: result.success && !parsed.error,
    traceLines: parsed.traceLines,
    result: parsed.result,
    expression: wrapped,
  };

  if (parsed.error) {
    response.error = parsed.error;
  }

  // Parser mode: add call tree visualization
  if (args.parser_mode && parsed.traceLines.length > 0) {
    response.callTree = buildCallTree(parsed.traceLines);
    response._hint =
      "Parser trace shows call tree. Look for backtracking patterns (same parser called multiple times) " +
      "and deep recursion. Consider adding memoization or refactoring to reduce backtracking.";
  }

  return JSON.stringify(response);
}

/**
 * Build a simple call tree from trace lines for parser debugging.
 * Detects recursive calls and backtracking patterns.
 */
function buildCallTree(traceLines: string[]): {
  tree: string[];
  backtracking: string[];
  maxDepth: number;
} {
  const tree: string[] = [];
  const callCounts = new Map<string, number>();
  let maxDepth = 0;
  let currentDepth = 0;

  for (const line of traceLines) {
    // Extract function/parser name from trace line
    const match = /^>>\s*(\w+)/.exec(line);
    if (!match) {
      tree.push(line);
      continue;
    }

    const name = match[1];
    callCounts.set(name, (callCounts.get(name) ?? 0) + 1);

    // Estimate depth by counting leading spaces/indentation
    const indent = "  ".repeat(currentDepth);
    tree.push(`${indent}${line}`);

    // Track depth (heuristic: if same parser called again, it's recursion)
    if (line.includes("(")) currentDepth++;
    if (line.includes(")")) currentDepth = Math.max(0, currentDepth - 1);
    maxDepth = Math.max(maxDepth, currentDepth);
  }

  // Detect backtracking: parsers called multiple times
  const backtracking = Array.from(callCounts.entries())
    .filter(([_, count]) => count > 2)
    .map(([name, count]) => `${name} called ${count} times (possible backtracking)`);

  return { tree, backtracking, maxDepth };
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_trace",
    "Debug a Haskell expression by wrapping it with Debug.Trace to show intermediate values. " +
      "Use parser_mode=true for enhanced parser debugging with call tree visualization.",
    {
      expression: z.string().describe("The Haskell expression to trace"),
      trace_points: z
        .array(z.string())
        .optional()
        .describe("Sub-expressions to trace. If omitted, traceShowId is used."),
      parser_mode: z
        .boolean()
        .optional()
        .describe(
          "If true, enables parser-specific debugging with call tree visualization, " +
            "backtracking detection, and recursion depth analysis. " +
            "Useful when debugging recursive descent parsers or parser combinators."
        ),
    },
    async ({ expression, trace_points, parser_mode }) => {
      const session = await ctx.getSession();
      const result = await handleTrace(session, { expression, trace_points, parser_mode });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
