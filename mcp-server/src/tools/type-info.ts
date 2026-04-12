import { GhciSession } from "../ghci-session.js";
import { parseInfoOutput } from "../parsers/type-parser.js";

export const typeInfoTool = {
  name: "ghci_info",
  description:
    "Get detailed information about a Haskell name (function, type, typeclass) using GHCi's :i command. " +
    "Shows the definition, kind, instances, and where it's defined. " +
    "Use this to understand typeclass hierarchies, data type constructors, or function signatures.",
  inputSchema: {
    type: "object" as const,
    properties: {
      name: {
        type: "string",
        description:
          'The name to look up. Examples: "Functor", "Map.Map", "Maybe", "(++)"',
      },
    },
    required: ["name"],
  },
};

export async function handleTypeInfo(
  session: GhciSession,
  args: { name: string }
): Promise<string> {
  const result = await session.infoOf(args.name);
  if (!result.success) {
    return JSON.stringify({
      success: false,
      error: result.output,
    });
  }

  // Detect deferred out-of-scope variables (same as ghci_type fix)
  if (/deferred-out-of-scope-variables|Variable not in scope|Not in scope/.test(result.output)) {
    return JSON.stringify({
      success: false,
      error: result.output,
    });
  }

  const parsed = parseInfoOutput(result.output);
  return JSON.stringify({
    success: true,
    ...parsed,
  });
}
