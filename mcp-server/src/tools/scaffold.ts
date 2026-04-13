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

  for (const mod of modules) {
    const relPath = moduleToFilePath(mod, srcDir);
    const absPath = path.join(projectDir, relPath);

    const exists = await fileExists(absPath);
    const modSigs = signatures?.[mod];

    // Allow overwriting minimal stubs (just "module X where\n") when signatures
    // are provided. This supports the flow: auto-scaffold creates minimal stubs
    // so GHCi can start, then explicit scaffold adds typed signatures.
    if (exists && modSigs && modSigs.length > 0) {
      const content = await readFile(absPath, "utf-8");
      const isMinimalStub = content.trim() === `module ${mod} where`;
      if (isMinimalStub) {
        const stub = generateStub(mod, modSigs);
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
      const stub = generateStub(mod, modSigs);
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
 * Generate a Haskell module stub.
 * With signatures: includes typed stubs with = undefined for ghci_suggest.
 * Declarations (data, newtype, type, class, instance, deriving) are emitted verbatim.
 * Without: minimal module header only.
 */
function generateStub(moduleName: string, signatures?: string[]): string {
  if (!signatures || signatures.length === 0) {
    return `module ${moduleName} where\n`;
  }
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
  return `module ${moduleName} where\n\n${stubs}\n`;
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
