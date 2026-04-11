import { access, mkdir, writeFile } from "node:fs/promises";
import path from "node:path";
import {
  parseCabalModules,
  moduleToFilePath,
  getLibrarySrcDir,
} from "../parsers/cabal-parser.js";

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
 * Returns a report of what was created vs what already existed.
 */
export async function handleScaffold(
  projectDir: string
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
    if (exists) {
      alreadyExist.push(relPath);
    } else {
      // Ensure parent directory exists
      const dir = path.dirname(absPath);
      await mkdir(dir, { recursive: true });

      // Write minimal stub
      const stub = generateStub(mod);
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
  });
}

/**
 * Generate a minimal Haskell module stub.
 */
function generateStub(moduleName: string): string {
  return `module ${moduleName} where\n`;
}

async function fileExists(filePath: string): Promise<boolean> {
  try {
    await access(filePath);
    return true;
  } catch {
    return false;
  }
}
