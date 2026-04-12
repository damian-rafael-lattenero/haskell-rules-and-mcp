import path from "node:path";
import { readdir } from "node:fs/promises";
import { extractPackageName, findCabalFile } from "./parsers/cabal-parser.js";
import { readFile } from "node:fs/promises";

export interface ProjectInfo {
  name: string;
  path: string;
  cabalFile: string;
}

/**
 * Discover Haskell projects in the playground directory.
 * A project is any subdirectory containing a .cabal file.
 */
export async function discoverProjects(
  playgroundDir: string
): Promise<ProjectInfo[]> {
  const projects: ProjectInfo[] = [];

  let entries: string[];
  try {
    entries = await readdir(playgroundDir);
  } catch {
    return [];
  }

  for (const entry of entries) {
    const fullPath = path.join(playgroundDir, entry);
    try {
      const cabalFile = await findCabalFile(fullPath);
      const content = await readFile(cabalFile, "utf-8");
      const name = extractPackageName(content) ?? entry;
      projects.push({ name, path: fullPath, cabalFile: path.basename(cabalFile) });
    } catch {
      // Not a project directory — skip
    }
  }

  return projects;
}

/**
 * Resolve the playground directory from the MCP server's base directory.
 */
export function getPlaygroundDir(baseDir: string): string {
  return path.join(baseDir, "playground");
}
