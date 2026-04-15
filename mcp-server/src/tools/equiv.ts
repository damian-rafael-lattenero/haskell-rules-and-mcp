import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { GhciSession } from "../ghci-session.js";
import type { ToolContext } from "./registry.js";

export interface EquivResult {
  equivalent: boolean;
  reason?: string;
  expr1Result?: string;
  expr2Result?: string;
}

/**
 * Check if two expressions are semantically equivalent by evaluating them.
 */
export async function checkEquivalence(
  session: GhciSession,
  expr1: string,
  expr2: string,
  context?: Record<string, string>
): Promise<EquivResult> {
  try {
    // Set up context bindings if provided
    if (context && Object.keys(context).length > 0) {
      for (const [k, v] of Object.entries(context)) {
        const bindResult = await session.execute(`let ${k} = ${v}`);
        if (!bindResult.success) {
          return {
            equivalent: false,
            reason: `Failed to bind ${k} = ${v}: ${bindResult.output}`,
            expr1Result: '',
            expr2Result: ''
          };
        }
      }
    }

    // Evaluate both expressions
    const result1 = await session.execute(expr1);
    const result2 = await session.execute(expr2);

    // Check if both evaluations succeeded
    if (!result1.success || !result2.success) {
      return {
        equivalent: false,
        reason: 'One or both expressions failed to evaluate',
        expr1Result: result1.success ? result1.output.trim() : `Error: ${result1.output}`,
        expr2Result: result2.success ? result2.output.trim() : `Error: ${result2.output}`
      };
    }

    // Compare results
    const output1 = result1.output.trim();
    const output2 = result2.output.trim();
    const equivalent = output1 === output2;

    return {
      equivalent,
      reason: equivalent ? undefined : `${output1} ≠ ${output2}`,
      expr1Result: output1,
      expr2Result: output2
    };
  } catch (error) {
    return {
      equivalent: false,
      reason: `Evaluation error: ${error instanceof Error ? error.message : String(error)}`
    };
  }
}

/**
 * Register the ghci_equiv tool.
 */
export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_equiv",
    "Check if two Haskell expressions are semantically equivalent by evaluating them in GHCi. " +
    "Useful for testing roundtrip properties, simplification correctness, and semantic equality. " +
    "Optionally provide a context with variable bindings.",
    {
      expr1: z.string().describe("First expression to evaluate"),
      expr2: z.string().describe("Second expression to evaluate"),
      context: z.record(z.string(), z.string()).optional().describe(
        "Optional variable bindings as key-value pairs. " +
        "Example: {\"x\": \"5\", \"y\": \"10\"} sets x=5 and y=10 before evaluation."
      )
    },
    async ({ expr1, expr2, context }) => {
      const session = await ctx.getSession();
      const result = await checkEquivalence(session, expr1, expr2, context);

      return {
        content: [{ type: "text" as const, text: JSON.stringify(result, null, 2) }]
      };
    }
  );
}
