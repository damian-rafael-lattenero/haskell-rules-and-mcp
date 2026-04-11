import { GhciSession } from "../ghci-session.js";
import { parseTypeOutput } from "../parsers/type-parser.js";

export const typeCheckTool = {
  name: "ghci_type",
  description:
    "Get the type of a Haskell expression using GHCi's :t command. " +
    "Use this to verify types of subexpressions before composing them, " +
    "or to understand what type a function expects/returns.",
  inputSchema: {
    type: "object" as const,
    properties: {
      expression: {
        type: "string",
        description:
          'The Haskell expression to type-check. Examples: "map (+1)", "foldr", "Just . show"',
      },
    },
    required: ["expression"],
  },
};

export async function handleTypeCheck(
  session: GhciSession,
  args: { expression: string }
): Promise<string> {
  const result = await session.typeOf(args.expression);
  if (!result.success) {
    return JSON.stringify({
      success: false,
      error: result.output,
    });
  }

  const parsed = parseTypeOutput(result.output);
  if (parsed) {
    return JSON.stringify({
      success: true,
      expression: parsed.expression,
      type: parsed.type,
    });
  }

  return JSON.stringify({
    success: true,
    raw: result.output,
  });
}
