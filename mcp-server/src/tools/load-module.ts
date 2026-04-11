import { GhciSession } from "../ghci-session.js";
import { parseGhcErrors, formatErrors } from "../parsers/error-parser.js";
import {
  parseCabalModules,
  moduleToFilePath,
  getLibrarySrcDir,
} from "../parsers/cabal-parser.js";

export const loadModuleTool = {
  name: "ghci_load",
  description:
    "Load or reload a Haskell module in GHCi. " +
    "Without a module_path, reloads all currently loaded modules (:r). " +
    "With a module_path, loads that specific module (:l). " +
    "With load_all=true, reads the .cabal file and loads ALL library modules. " +
    "Returns parsed compilation errors and warnings.",
  inputSchema: {
    type: "object" as const,
    properties: {
      module_path: {
        type: "string",
        description:
          'Optional path to a module to load. If omitted, reloads current modules. Examples: "src/Lib.hs", "src/MyModule.hs"',
      },
      load_all: {
        type: "boolean",
        description:
          "If true, reads the .cabal file and loads ALL library modules into GHCi at once. Lighter than cabal_build (interpreted, not compiled).",
      },
    },
    required: [],
  },
};

export async function handleLoadModule(
  session: GhciSession,
  args: { module_path?: string; load_all?: boolean },
  projectDir?: string
): Promise<string> {
  let result;

  if (args.load_all && projectDir) {
    const cabalModules = await parseCabalModules(projectDir);
    const srcDir = await getLibrarySrcDir(projectDir);
    const paths = cabalModules.library.map((mod) =>
      moduleToFilePath(mod, srcDir)
    );

    if (paths.length === 0) {
      return JSON.stringify({
        success: false,
        errors: [],
        warnings: [],
        summary: "No library modules found in .cabal file",
        modules: [],
        raw: "",
      });
    }

    result = await session.loadModules(paths, cabalModules.library);

    const errors = parseGhcErrors(result.output);
    const errorCount = errors.filter((e) => e.severity === "error").length;
    const warningCount = errors.filter((e) => e.severity === "warning").length;

    return JSON.stringify({
      success: errorCount === 0,
      errors: errors.filter((e) => e.severity === "error"),
      warnings: errors.filter((e) => e.severity === "warning"),
      modules: paths,
      summary:
        errorCount === 0
          ? `Loaded ${paths.length} modules${warningCount > 0 ? ` with ${warningCount} warning(s)` : ""}`
          : `${errorCount} error(s), ${warningCount} warning(s) across ${paths.length} modules`,
      raw: result.output,
    });
  }

  result = args.module_path
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
