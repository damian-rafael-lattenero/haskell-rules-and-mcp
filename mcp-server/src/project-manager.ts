import path from "node:path";
import { readdir, stat } from "node:fs/promises";
import { extractPackageName, findCabalFile } from "./parsers/cabal-parser.js";
import { readFile } from "node:fs/promises";

export interface ProjectInfo {
  name: string;
  dirName: string;
  path: string;
  cabalFile: string;
}

/**
 * Discover Haskell projects in a directory recursively.
 * A project is any subdirectory containing a .cabal file.
 * 
 * @param searchDir - Root directory to search from
 * @param maxDepth - Maximum recursion depth (default: 3)
 * @returns Array of discovered projects
 */
export async function discoverProjects(
  searchDir: string,
  maxDepth: number = 3
): Promise<ProjectInfo[]> {
  const projects: ProjectInfo[] = [];

  async function scan(dir: string, depth: number): Promise<void> {
    if (depth > maxDepth) return;

    let entries: string[];
    try {
      entries = await readdir(dir);
    } catch {
      return;
    }

    for (const entry of entries) {
      // Skip hidden directories and common non-project directories
      if (entry.startsWith(".") || entry === "node_modules" || entry === "dist-newstyle") {
        continue;
      }

      const fullPath = path.join(dir, entry);
      
      try {
        const stats = await stat(fullPath);
        
        // Try to find .cabal file in this directory
        try {
          const cabalFile = await findCabalFile(fullPath);
          const content = await readFile(cabalFile, "utf-8");
          const name = extractPackageName(content);
          // Skip projects whose .cabal file is empty or missing the required name: field.
          // An empty cabal file causes GHCi startup to fail with a confusing error,
          // and pollutes the project list with unusable entries.
          if (name) {
            projects.push({ 
              name, 
              dirName: entry, 
              path: fullPath, 
              cabalFile: path.basename(cabalFile) 
            });
          }
        } catch {
          // No .cabal here, continue searching
        }
        
        // If it's a directory, search recursively
        if (stats.isDirectory()) {
          await scan(fullPath, depth + 1);
        }
      } catch {
        // Skip entries we can't read
      }
    }
  }

  await scan(searchDir, 0);
  return projects;
}
