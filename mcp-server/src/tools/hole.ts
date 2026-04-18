/**
 * ghci_hole — Interactive typed-hole exploration.
 *
 * Loads a module with -fdefer-typed-holes enabled, parses all holes from the
 * GHC output (using the existing hole-parser), and returns structured results
 * with valid fits, relevant bindings, and expected types.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import type { GhciSession } from "../ghci-session.js";
import { parseTypedHoles } from "../parsers/hole-parser.js";
import type { TypedHole } from "../parsers/hole-parser.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

export async function handleHole(
  session: GhciSession,
  args: { module_path?: string; hole_name?: string }
): Promise<string> {
  const { module_path, hole_name } = args;

  if (!module_path) {
    return JSON.stringify({
      success: false,
      error: "module_path is required. Example: { module_path: 'src/MyModule.hs' }",
    });
  }

  try {
    // Enable deferred typed holes so GHCi reports them as warnings rather than errors
    await session.execute(":set -fdefer-typed-holes");
    await session.execute(":set -Wtyped-holes");

    const loadResult = await session.loadModule(module_path);
    const output = loadResult.output;

    // Disable deferred holes immediately after loading (cleanup)
    await session.execute(":unset -fdefer-typed-holes").catch(() => {});

    const allHoles: TypedHole[] = parseTypedHoles(output);

    // Filter by hole_name if requested
    const holes = hole_name
      ? allHoles.filter((h) => h.hole === hole_name)
      : allHoles;

    return JSON.stringify({
      success: true,
      module_path,
      hole_count: holes.length,
      holes: holes.map((h) => ({
        hole: h.hole,
        expectedType: h.expectedType,
        location: h.location,
        expression: h.expression,
        equation: h.equation,
        relevantBindings: h.relevantBindings,
        validFits: h.validFits,
        suppressed: h.suppressed,
      })),
      ...(holes.length === 0
        ? { _hint: "No typed holes found. Add _ or _name placeholders to your code to explore types." }
        : {
            _hint:
              holes.length === 1
                ? `Found 1 hole. Check validFits for candidates and relevantBindings for in-scope names.`
                : `Found ${holes.length} holes. Use hole_name parameter to filter to a specific one.`,
          }),
    });
  } catch (err) {
    // Always try to clean up the flag even on error
    await session.execute(":unset -fdefer-typed-holes").catch(() => {});
    return JSON.stringify({
      success: false,
      error: `Failed to analyze holes in ${module_path}: ${err instanceof Error ? err.message : String(err)}`,
    });
  }
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_hole",
    "Explore typed holes in a Haskell module interactively. " +
      "Loads the module with -fdefer-typed-holes enabled and returns structured information " +
      "about each hole: expected type, valid fits (candidates), and relevant in-scope bindings. " +
      "Use _ or _name placeholders in your code to explore what fits. " +
      "Optionally filter to a specific hole with hole_name.",
    {
      module_path: z.string().describe(
        'Path to the module to analyze. Examples: "src/MyModule.hs", "src/Expr/Eval.hs"'
      ),
      hole_name: z.string().optional().describe(
        'Optional: filter to a specific hole by name. Examples: "_result", "_body". ' +
          "Omit to return all holes in the module."
      ),
    },
    async ({ module_path, hole_name }) => {
      const session = await ctx.getSession();
      const result = await handleHole(session, { module_path, hole_name });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
