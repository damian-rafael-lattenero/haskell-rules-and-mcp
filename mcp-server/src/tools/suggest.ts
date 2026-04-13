import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile, unlink } from "node:fs/promises";
import path from "node:path";
import { GhciSession } from "../ghci-session.js";
import { parseTypedHoles } from "../parsers/hole-parser.js";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { parseBrowseOutput, inferModuleName } from "../parsers/browse-parser.js";
import { suggestFunctionProperties, type Sibling } from "../laws/function-laws.js";
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
    // No undefined stubs — switch to analyze mode automatically
    return handleAnalyze(session, args.module_path);
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

/**
 * Analyze an already-implemented module: list functions with types and suggest properties.
 * This is the alternative to hole-fit analysis for modules that don't use = undefined.
 */
async function handleAnalyze(
  session: GhciSession,
  modulePath: string
): Promise<string> {
  const modName = inferModuleName(modulePath);
  const browseResult = await session.execute(`:browse *${modName}`);
  if (!browseResult.success) {
    return JSON.stringify({
      success: false,
      mode: "analyze",
      error: `Could not browse module ${modName}`,
    });
  }

  const defs = parseBrowseOutput(browseResult.output);
  const functions = defs.filter((d) => d.kind === "function");

  if (functions.length === 0) {
    return JSON.stringify({
      success: true,
      mode: "analyze",
      functions: [],
      summary: "No functions found in module",
    });
  }

  // Build sibling list for roundtrip detection
  const siblings: Sibling[] = functions.map((f) => ({
    name: f.name,
    type: `${f.name} :: ${f.type}`,
  }));

  // Get type and suggest properties for each function
  const analyzed = functions.map((fn) => {
    const typeStr = `${fn.name} :: ${fn.type}`;
    const properties = suggestFunctionProperties(fn.name, typeStr, siblings);
    return {
      name: fn.name,
      type: fn.type,
      suggestedProperties: properties.map((p) => ({
        law: p.law,
        property: p.property,
        confidence: p.confidence,
      })),
    };
  });

  const withProps = analyzed.filter((a) => a.suggestedProperties.length > 0);

  return JSON.stringify({
    success: true,
    mode: "analyze",
    functions: analyzed,
    summary: `Analyzed ${analyzed.length} function(s), ${withProps.length} have suggested properties`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_suggest",
    "Find `= undefined` functions in a Haskell module and suggest implementations. " +
      "Temporarily replaces each `= undefined` with a typed hole `= _`, loads the module " +
      "in GHCi to get hole fits, and returns suggestions for each function. " +
      "The original file is always restored after analysis. " +
      "If no `= undefined` stubs are found (or mode='analyze'), analyzes implemented functions " +
      "and suggests QuickCheck properties based on their types.",
    {
      module_path: z
        .string()
        .describe(
          'Path to a module to analyze. Examples: "src/Lib.hs"'
        ),
      mode: z
        .enum(["suggest", "analyze"])
        .optional()
        .describe(
          'suggest (default): find = undefined stubs and show hole fits. ' +
          'analyze: analyze implemented functions and suggest QuickCheck properties.'
        ),
    },
    async ({ module_path, mode }) => {
      const session = await ctx.getSession();

      // If mode is explicitly "analyze", skip the undefined-stub scan entirely
      if (mode === "analyze") {
        const result = await handleAnalyze(session, module_path);
        ctx.logToolExecution("ghci_suggest", true);
        return { content: [{ type: "text" as const, text: result }] };
      }

      const result = await handleSuggest(
        session,
        { module_path },
        ctx.getProjectDir()
      );

      // Track function counts in workflow state
      try {
        const parsed = JSON.parse(result);
        if (parsed.success && parsed.suggestions) {
          const undefinedCount = parsed.suggestions.length;
          const existing = ctx.getModuleProgress(module_path);
          const currentImplemented = existing?.functionsImplemented ?? 0;
          ctx.updateModuleProgress(module_path, {
            functionsTotal: undefinedCount + currentImplemented,
            phase: undefinedCount > 0 ? "implementing" : "complete",
          });
        }
      } catch { /* don't break suggest if tracking fails */ }
      ctx.logToolExecution("ghci_suggest", true);

      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
