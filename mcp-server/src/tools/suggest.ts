import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile, unlink } from "node:fs/promises";
import path from "node:path";
import { GhciSession } from "../ghci-session.js";
import { parseTypedHoles } from "../parsers/hole-parser.js";
import { parseGhcErrors } from "../parsers/error-parser.js";
import { parseBrowseOutput, inferModuleName } from "../parsers/browse-parser.js";
import {
  parseCabalModules,
  moduleToFilePath,
  getLibrarySrcDir,
} from "../parsers/cabal-parser.js";
import { suggestFunctionProperties, type Sibling } from "../laws/function-laws.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

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
    // No stubs found — return guidance instead of silently falling back to analyze
    return JSON.stringify({
      success: true,
      mode: "suggest",
      suggestions: [],
      summary: "No `= undefined` stubs found in this module.",
      _nextStep:
        "To get implementation suggestions:\n" +
        "1. Add type signatures with `= undefined` bodies:\n" +
        "   foo :: Int -> Int\n" +
        "   foo = undefined\n" +
        "2. Re-run ghci_suggest(module_path=\"...\")\n\n" +
        "Or use ghci_suggest(mode=\"analyze\") to get QuickCheck " +
        "property suggestions for already-implemented functions.",
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
    let loadResult = await session.loadModule(args.module_path);
    let holes = parseTypedHoles(loadResult.output);

    // Fase 4 fix: GHCi's `:l` replaces the loaded-module set. If an earlier
    // `ghci_load(other.hs)` evicted the target's dependencies, the holes GHC
    // reports may end up with `expectedType: "unknown"` because the types
    // they reference aren't in scope. Detect this (no holes OR every hole
    // has an unknown type) and retry once after loading ALL library modules
    // from the cabal file — deterministic, cheap, and non-destructive
    // (we still restore the source in `finally`).
    const allUnknown =
      holes.length > 0 &&
      holes.every((h) => !h.expectedType || h.expectedType === "unknown");
    if ((holes.length === 0 || allUnknown) && undefinedFns.length > 0) {
      try {
        const cabalModules = await parseCabalModules(projectDir);
        const srcDir = await getLibrarySrcDir(projectDir);
        const paths = cabalModules.library.map((mod) =>
          moduleToFilePath(mod, srcDir)
        );
        if (paths.length > 0) {
          await session.loadModules(paths, cabalModules.library);
          loadResult = await session.loadModule(args.module_path);
          holes = parseTypedHoles(loadResult.output);
        }
      } catch {
        // Non-fatal — fall through to whatever holes we already have.
      }
    }

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

    // For functions with empty validFits, supplement with scope info:
    // constructors of input types, useful functions from imports
    for (const s of suggestions) {
      if (s.validFits.length === 0 && s.expectedType !== "unknown") {
        try {
          // Extract type names from the expected type to find constructors
          const typeNames = s.expectedType.match(/\b[A-Z][\w']*/g) ?? [];
          const scopeInfo: string[] = [];
          for (const tn of typeNames.slice(0, 3)) { // limit to avoid latency
            const info = await session.execute(`:i ${tn}`);
            if (info.success && info.output.includes("data ") || info.output.includes("newtype ")) {
              scopeInfo.push(info.output.split("\n")[0]!.trim());
            }
          }
          if (scopeInfo.length > 0) {
            (s as any).scopeInfo = scopeInfo;
          }
        } catch { /* non-fatal */ }
      }
    }

    return JSON.stringify({
      success: true,
      suggestions,
      summary: `Found ${suggestions.length} undefined function(s) with suggestions`,
      _nextStep: `Implement the ${suggestions.length} function(s) using the hole fits above. After each, run ghci_load(diagnostics=true).`,
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
export async function handleAnalyze(
  session: GhciSession,
  modulePath: string,
  projectDir: string
): Promise<string> {
  const modName = inferModuleName(modulePath);

  // Load the whole project (not just the target module) so cross-module
  // engines like evaluator-preservation can see siblings. Before Phase 5
  // this used `:l` on a single module which replaced the loaded set,
  // leaving `:show modules` returning just the target and `:browse` on
  // other modules failing — so `evaluatorPreservationEngine` could never
  // match because it never got the interpreter sibling.
  //
  // We first try load_all (reads the .cabal for library modules). If
  // that succeeds, every module in the project is in scope. Fallback:
  // if load_all fails (non-cabal file, standalone script, etc.) load
  // only the target module.
  try {
    const cabalMods = await parseCabalModules(projectDir);
    const srcDir = await getLibrarySrcDir(projectDir);
    const paths = cabalMods.library.map((mod) =>
      moduleToFilePath(mod, srcDir)
    );
    if (paths.length > 0) {
      const loaded = await session.loadModules(paths, cabalMods.library);
      if (!loaded.success) {
        return JSON.stringify({
          success: false,
          mode: "analyze",
          error: `Could not load project modules for cross-module analysis`,
          loadOutput: loaded.output.slice(0, 2_000),
        });
      }
    } else {
      // Fallback to single-module load for non-standard layouts.
      const loadResult = await session.loadModule(modulePath);
      if (!loadResult.success) {
        return JSON.stringify({
          success: false,
          mode: "analyze",
          error: `Could not load ${modulePath} for analysis`,
          loadOutput: loadResult.output.slice(0, 2_000),
        });
      }
    }
  } catch {
    // No cabal, or cabal parse failed — single-module fallback.
    const loadResult = await session.loadModule(modulePath);
    if (!loadResult.success) {
      return JSON.stringify({
        success: false,
        mode: "analyze",
        error: `Could not load ${modulePath} for analysis`,
        loadOutput: loadResult.output.slice(0, 2_000),
      });
    }
  }

  const browseResult = await session.execute(`:browse ${modName}`);
  // Safety guard: truncate if output is unexpectedly large
  if (browseResult.output.length > 50_000) {
    browseResult.output = browseResult.output.slice(0, 50_000) +
      "\n... (truncated — output too large)";
  }
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

  // Build sibling list from ALL currently-loaded modules, not just the
  // target module. This is necessary for cross-module engines (e.g.
  // evaluator-preservation) to fire: when analyzing `simplify :: Expr -> Expr`
  // in Simplify.hs, we want `eval :: Env -> Expr -> r` from Eval.hs to appear
  // as a sibling. `:show modules` lists everything in the GHCi scope; we
  // `:browse` each and union the function definitions.
  const siblings: Sibling[] = [];
  const seen = new Set<string>();
  for (const f of functions) {
    siblings.push({ name: f.name, type: `${f.name} :: ${f.type}` });
    seen.add(f.name);
  }
  try {
    const showMods = await session.execute(":show modules");
    const otherModules = showMods.output
      .split("\n")
      .map((line) => line.match(/^(\S+)\s+\(/)?.[1])
      .filter((n): n is string => !!n && n !== modName);

    for (const m of otherModules) {
      const result = await session.execute(`:browse ${m}`);
      if (!result.success) continue;
      const otherDefs = parseBrowseOutput(
        result.output.length > 50_000
          ? result.output.slice(0, 50_000)
          : result.output
      );
      for (const d of otherDefs) {
        if (d.kind !== "function") continue;
        if (seen.has(d.name)) continue; // target-module functions win
        siblings.push({ name: d.name, type: `${d.name} :: ${d.type}` });
        seen.add(d.name);
      }
    }
  } catch {
    // Non-fatal: cross-module siblings are best-effort. If browse fails for
    // one module we still have the target-module siblings.
  }

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
        ...(p.rationale ? { rationale: p.rationale } : {}),
      })),
    };
  });

  const withProps = analyzed.filter((a) => a.suggestedProperties.length > 0);

  return JSON.stringify({
    success: true,
    mode: "analyze",
    functions: analyzed,
    summary: `Analyzed ${analyzed.length} function(s), ${withProps.length} have suggested properties`,
    _nextStep: withProps.length > 0
      ? `Run ghci_quickcheck for the ${withProps.length} function(s) with suggested properties.`
      : "All functions analyzed. Write custom QuickCheck properties based on the function contracts.",
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_suggest",
    "Find `= undefined` functions and show typed-hole implementation suggestions. " +
      "Temporarily replaces each `= undefined` with a typed hole `= _`, loads the module " +
      "in GHCi to get hole fits, and returns suggestions for each function. " +
      "The original file is always restored after analysis. " +
      "Returns empty suggestions with guidance if no `= undefined` stubs exist — add stubs first. " +
      "Use mode='analyze' to analyze implemented functions and suggest QuickCheck properties.",
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
        const result = await handleAnalyze(session, module_path, ctx.getProjectDir());
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
