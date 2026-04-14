import path from "node:path";
import { readdir } from "node:fs/promises";
import { extractPackageName, findCabalFile } from "./parsers/cabal-parser.js";
import { readFile } from "node:fs/promises";

export interface ProjectInfo {
  name: string;
  dirName: string;
  path: string;
  cabalFile: string;
}

/**
 * Discover Haskell projects in a directory.
 * A project is any subdirectory containing a .cabal file.
 */
export async function discoverProjects(
  searchDir: string
): Promise<ProjectInfo[]> {
  const projects: ProjectInfo[] = [];

  let entries: string[];
  try {
    entries = await readdir(searchDir);
  } catch {
    return [];
  }

  for (const entry of entries) {
    const fullPath = path.join(searchDir, entry);
    try {
      const cabalFile = await findCabalFile(fullPath);
      const content = await readFile(cabalFile, "utf-8");
      const name = extractPackageName(content);
      // Skip projects whose .cabal file is empty or missing the required name: field.
      // An empty cabal file causes GHCi startup to fail with a confusing error,
      // and pollutes the project list with unusable entries.
      if (!name) continue;
      projects.push({ name, dirName: entry, path: fullPath, cabalFile: path.basename(cabalFile) });
    } catch {
      // Not a project directory — skip
    }
  }

  return projects;
}
