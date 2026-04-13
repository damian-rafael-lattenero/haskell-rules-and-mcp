import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile, unlink } from "node:fs/promises";
import path from "node:path";
import { GhciSession } from "../ghci-session.js";
import { parseTypedHoles } from "../parsers/hole-parser.js";
import { parseGhcErrors } from "../parsers/error-parser.js";
import type { ToolContext } from "./registry.js";

interface UndefinedFunction {
  name: string;
  line: number;
}

interface Suggestion {
  function: string;
  line: number;
  expectedType: string;
  validFits: string[];
  relevantBindings: string[];
}

/**
 * Find `= undefined` functions in a module, replace them with typed holes,
 * load in GHCi, and return hole-fit suggestions for each function.
 */
export async function handleSuggest(
  session: GhciSession,
  args: { module_path: string },
  projectDir: string
): Promise<string> {
  const absPath = path.resolve(projectDir, args.module_path);

  // Read the source file
  let source: string;
  try {
    source = await readFile(absPath, "utf-8");
  } catch (err: unknown) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") {
      return JSON.stringify({
        success: false,
        error: `File not found: ${absPath}`,
      });
    }
    throw err;
  }

  // Find `= undefined` patterns line by line
  const lines = source.split("\n");
  const undefinedFns: UndefinedFunction[] = [];
  const undefinedPattern = /=\s*undefined\s*$/;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    if (undefinedPattern.test(line)) {
      // Extract function name: first non-whitespace token on the line
      const nameMatch = line.match(/^\s*(\S+)/);
      if (nameMatch) {
        undefinedFns.push({ name: nameMatch[1]!, line: i + 1 });
      }
    }
  }

  if (undefinedFns.length === 0) {
    return JSON.stringify({
      success: true,
      suggestions: [],
      summary: "No undefined functions found",
    });
  }

  // Pre-check: load module first to verify it compiles
  const preCheck = await session.loadModule(args.module_path);
  const preErrors = parseGhcErrors(preCheck.output).filter(
    (e) => e.severity === "error"
  );
  if (preErrors.length > 0) {
    return JSON.stringify({
      success: false,
      error: "Module has compile errors — fix them before suggesting",
      errors: preErrors,
    });
  }

  // Create backup
  const backupPath = absPath + ".suggest-backup";
  await writeFile(backupPath, source, "utf-8");

  // Replace `= undefined` with `= _`
  const modifiedLines = lines.map((line) =>
    undefinedPattern.test(line)
      ? line.replace(/=\s*undefined\s*$/, "= _")
      : line
  );
  const modifiedSource = modifiedLines.join("\n");
  await writeFile(absPath, modifiedSource, "utf-8");

  try {
    // Set GHCi flags for typed holes
    await session.execute(":set -fdefer-type-errors");
    await session.execute(":set -fmax-valid-hole-fits=10");

    // Load modified module
    const loadResult = await session.loadModule(args.module_path);

    // Parse typed holes from output
    const holes = parseTypedHoles(loadResult.output);

    // Map holes to function names by line number
    const suggestions: Suggestion[] = undefinedFns.map((fn) => {
      const matchingHole = holes.find((h) => h.location.line === fn.line);
      return {
        function: fn.name,
        line: fn.line,
        expectedType: matchingHole?.expectedType ?? "unknown",
        validFits: matchingHole
          ? matchingHole.validFits.map((f) => `${f.name} :: ${f.type}`)
          : [],
        relevantBindings: matchingHole
          ? matchingHole.relevantBindings.map(
              (b) => `${b.name} :: ${b.type}`
            )
          : [],
      };
    });

    return JSON.stringify({
      success: true,
      suggestions,
      summary: `Found ${suggestions.length} undefined function(s) with suggestions`,
    });
  } finally {
    // ALWAYS restore original file
    await writeFile(absPath, source, "utf-8");
    try {
      await unlink(backupPath);
    } catch {
      /* ignore cleanup errors */
    }

    // Restore GHCi flags
    await session.execute(":set -fno-defer-type-errors");
    await session.execute(":set -fmax-valid-hole-fits=6");

    // Reload original module
    await session.loadModule(args.module_path);
  }
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_suggest",
    "Find `= undefined` functions in a Haskell module and suggest implementations. " +
      "Temporarily replaces each `= undefined` with a typed hole `= _`, loads the module " +
      "in GHCi to get hole fits, and returns suggestions for each function. " +
      "The original file is always restored after analysis.",
    {
      module_path: z
        .string()
        .describe(
          'Path to a module containing `= undefined` stubs. Examples: "src/Lib.hs"'
        ),
    },
    async ({ module_path }) => {
      const session = await ctx.getSession();
      const result = await handleSuggest(
        session,
        { module_path },
        ctx.getProjectDir()
      );
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
