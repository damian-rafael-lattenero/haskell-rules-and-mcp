import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { access, mkdir, writeFile, readFile } from "node:fs/promises";
import path from "node:path";
import {
  parseCabalModules,
  moduleToFilePath,
  getLibrarySrcDir,
} from "../parsers/cabal-parser.js";
import type { ToolContext } from "./registry.js";

export interface ScaffoldResult {
  created: string[];
  alreadyExist: string[];
  srcDir: string;
  totalModules: number;
}

/**
 * Read the .cabal file, find modules that don't have source files yet,
 * and create minimal stubs for them.
 *
 * When signatures are provided, generates typed stubs with = undefined
 * so that ghci_suggest can find them and offer hole-fit suggestions.
 *
 * Returns a report of what was created vs what already existed.
 */
export async function handleScaffold(
  projectDir: string,
  signatures?: Record<string, string[]>
): Promise<string> {
  const cabalModules = await parseCabalModules(projectDir);
  const srcDir = await getLibrarySrcDir(projectDir);

  const modules = cabalModules.library;
  const created: string[] = [];
  const alreadyExist: string[] = [];

  // Build type-owner map for cross-module import generation
  const typeOwner = signatures ? buildTypeOwnerMap(signatures) : new Map<string, string>();

  for (const mod of modules) {
    const relPath = moduleToFilePath(mod, srcDir);
    const absPath = path.join(projectDir, relPath);

    const exists = await fileExists(absPath);
    const modSigs = signatures?.[mod];

    // Compute cross-module imports for this module
    const importLines = modSigs ? computeImports(mod, modSigs, typeOwner) : [];

    // Allow overwriting minimal stubs (just "module X where\n") when signatures
    // are provided. This supports the flow: auto-scaffold creates minimal stubs
    // so GHCi can start, then explicit scaffold adds typed signatures.
    if (exists && modSigs && modSigs.length > 0) {
      const content = await readFile(absPath, "utf-8");
      const isMinimalStub = content.trim() === `module ${mod} where`;
      if (isMinimalStub) {
        const stub = generateStub(mod, modSigs, importLines);
        await writeFile(absPath, stub, "utf-8");
        created.push(relPath);
        continue;
      }
    }

    if (exists) {
      alreadyExist.push(relPath);
    } else {
      // Ensure parent directory exists
      const dir = path.dirname(absPath);
      await mkdir(dir, { recursive: true });

      // Write stub — with signatures if provided for this module
      const stub = generateStub(mod, modSigs, importLines);
      await writeFile(absPath, stub, "utf-8");
      created.push(relPath);
    }
  }

  const result: ScaffoldResult = {
    created,
    alreadyExist,
    srcDir,
    totalModules: modules.length,
  };

  return JSON.stringify({
    success: true,
    ...result,
    summary:
      created.length === 0
        ? `All ${modules.length} modules already have source files`
        : `Created ${created.length} stub(s): ${created.join(", ")}. ${alreadyExist.length} already existed.`,
    ...(created.length > 0
      ? {
          _nextStep: signatures
            ? "Stubs with `= undefined` created. Run ghci_session(restart) then ghci_suggest(module_path=\"...\") to see hole-fit suggestions."
            : "Minimal stubs created. Add type signatures with `= undefined` bodies, then run ghci_suggest for implementation hints.",
        }
      : {}),
  });
}

/**
 * Detect if a signature string is a Haskell declaration (data, newtype, type, class, etc.)
 * rather than a function type signature. Declarations should be emitted verbatim
 * without `= undefined`.
 */
export function isDeclaration(sig: string): boolean {
  const trimmed = sig.trimStart();
  return /^(data|newtype|type|class|instance|deriving)\s/.test(trimmed);
}

/**
 * Build a map of type names to the module that defines them,
 * based on data/newtype/type declarations in the signatures.
 */
export function buildTypeOwnerMap(
  allSignatures: Record<string, string[]>
): Map<string, string> {
  const typeOwner = new Map<string, string>();
  for (const [mod, sigs] of Object.entries(allSignatures)) {
    for (const sig of sigs) {
      // Match: data TypeName, newtype TypeName, type TypeName
      const match = /^(data|newtype|type)\s+([A-Z][\w']*)/.exec(sig.trim());
      if (match) {
        typeOwner.set(match[2]!, mod);
      }
    }
  }
  return typeOwner;
}

/**
 * Extract capitalized type names used in a list of signatures.
 * These are potential cross-module references.
 */
function extractUsedTypes(signatures: string[]): Set<string> {
  const types = new Set<string>();
  // Base types that are always in scope — never need imports
  const builtins = new Set([
    "Int", "Integer", "Float", "Double", "Char", "Bool", "String",
    "IO", "Maybe", "Either", "Ordering",
    "Show", "Eq", "Ord", "Read", "Enum", "Bounded", "Num", "Integral",
    "Functor", "Applicative", "Monad", "Monoid", "Semigroup",
    "Foldable", "Traversable",
  ]);
  for (const sig of signatures) {
    // Find all capitalized identifiers (type constructors / type classes)
    const matches = sig.matchAll(/\b([A-Z][\w']*)\b/g);
    for (const m of matches) {
      const name = m[1]!;
      if (!builtins.has(name)) {
        types.add(name);
      }
    }
  }
  return types;
}

/**
 * Compute import lines needed for a module based on cross-module type references.
 */
export function computeImports(
  moduleName: string,
  signatures: string[],
  typeOwner: Map<string, string>
): string[] {
  const usedTypes = extractUsedTypes(signatures);
  // Group types by source module
  const importsByModule = new Map<string, string[]>();
  for (const typeName of usedTypes) {
    const owner = typeOwner.get(typeName);
    if (owner && owner !== moduleName) {
      if (!importsByModule.has(owner)) importsByModule.set(owner, []);
      importsByModule.get(owner)!.push(typeName);
    }
  }
  // Generate import lines sorted by module name
  const imports: string[] = [];
  for (const [mod, typeNames] of [...importsByModule.entries()].sort()) {
    imports.push(`import ${mod} (${typeNames.sort().join(", ")})`);
  }
  return imports;
}

/**
 * Generate a Haskell module stub.
 * With signatures: includes typed stubs with = undefined for ghci_suggest.
 * Declarations (data, newtype, type, class, instance, deriving) are emitted verbatim.
 * Cross-module imports are generated automatically when allSignatures is provided.
 * Without: minimal module header only.
 */
function generateStub(
  moduleName: string,
  signatures?: string[],
  importLines?: string[]
): string {
  if (!signatures || signatures.length === 0) {
    return `module ${moduleName} where\n`;
  }
  const imports = importLines && importLines.length > 0
    ? "\n" + importLines.join("\n") + "\n"
    : "";
  const stubs = signatures
    .map((sig) => {
      if (isDeclaration(sig)) {
        return sig;
      }
      const match = /^(\(?[\w']+\)?)\s*::/.exec(sig);
      const name = match ? match[1] : sig.split("::")[0]!.trim();
      return `${sig}\n${name} = undefined`;
    })
    .join("\n\n");
  return `module ${moduleName} where\n${imports}\n${stubs}\n`;
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_scaffold",
    "Read the .cabal file, find library modules that don't have source files yet, and create minimal stubs. " +
      "Use after adding new modules to the .cabal file, before restarting GHCi. " +
      "This prevents the 'can't find source for Module' error on GHCi startup. " +
      "Pass `signatures` to generate typed stubs with `= undefined` bodies for ghci_suggest.",
    {
      signatures: z
        .record(z.string(), z.array(z.string()))
        .optional()
        .describe(
          'Optional: map of module name to type signatures. ' +
            'Generates `= undefined` stubs for ghci_suggest. ' +
            'Example: {"Parser.Core": ["satisfy :: String -> (Char -> Bool) -> Parser Char"]}'
        ),
    },
    async ({ signatures }) => {
      const result = await handleScaffold(ctx.getProjectDir(), signatures);
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}

async function fileExists(filePath: string): Promise<boolean> {
  try {
    await access(filePath);
    return true;
  } catch {
    return false;
  }
}
