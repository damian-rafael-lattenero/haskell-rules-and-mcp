import { GhciSession } from "../ghci-session.js";
import { parseGhcErrors, formatErrors } from "../parsers/error-parser.js";

export const loadModuleTool = {
  name: "ghci_load",
  description:
    "Load or reload a Haskell module in GHCi. " +
    "Without a module_path, reloads all currently loaded modules (:r). " +
    "With a module_path, loads that specific module (:l). " +
    "Returns parsed compilation errors and warnings.",
  inputSchema: {
    type: "object" as const,
    properties: {
      module_path: {
        type: "string",
        description:
          'Optional path to a module to load. If omitted, reloads current modules. Examples: "src/Lib.hs", "src/MyModule.hs"',
      },
    },
    required: [],
  },
};

export async function handleLoadModule(
  session: GhciSession,
  args: { module_path?: string }
): Promise<string> {
  const result = args.module_path
    ? await session.loadModule(args.module_path)
    : await session.reload();

  const errors = parseGhcErrors(result.output);
  const errorCount = errors.filter((e) => e.severity === "error").length;
  const warningCount = errors.filter((e) => e.severity === "warning").length;

  return JSON.stringify({
    success: errorCount === 0,
    errors: errors.filter((e) => e.severity === "error"),
    warnings: errors.filter((e) => e.severity === "warning"),
    summary: errorCount === 0
      ? `Loaded successfully${warningCount > 0 ? ` with ${warningCount} warning(s)` : ""}`
      : `${errorCount} error(s), ${warningCount} warning(s)`,
    raw: result.output,
  });
}
