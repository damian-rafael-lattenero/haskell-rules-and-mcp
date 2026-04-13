import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { GhciSession } from "../ghci-session.js";
import { parseTraceOutput } from "../parsers/trace-parser.js";
import type { ToolContext } from "./registry.js";

export async function handleTrace(
  session: GhciSession,
  args: { expression: string; trace_points?: string[] }
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

  // Step 5: Build the response
  const response: Record<string, unknown> = {
    success: result.success && !parsed.error,
    traceLines: parsed.traceLines,
    result: parsed.result,
    expression: wrapped,
  };

  if (parsed.error) {
    response.error = parsed.error;
  }

  return JSON.stringify(response);
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_trace",
    "Debug a Haskell expression by wrapping it with Debug.Trace to show intermediate values.",
    {
      expression: z.string().describe("The Haskell expression to trace"),
      trace_points: z
        .array(z.string())
        .optional()
        .describe("Sub-expressions to trace. If omitted, traceShowId is used."),
    },
    async ({ expression, trace_points }) => {
      const session = await ctx.getSession();
      const result = await handleTrace(session, { expression, trace_points });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
