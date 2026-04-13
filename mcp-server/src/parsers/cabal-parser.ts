import { readFile } from "node:fs/promises";
import path from "node:path";

export interface CabalModules {
  library: string[];
  executables: Map<string, string[]>;
}

/**
 * Parse a .cabal file and extract module names from exposed-modules
 * and other-modules for all stanzas.
 *
 * Returns module names in Haskell dotted form (e.g. "HM.Syntax").
 */
export async function parseCabalModules(
  projectDir: string
): Promise<CabalModules> {
  const cabalFile = await findCabalFile(projectDir);
  const content = await readFile(cabalFile, "utf-8");
  return extractModules(content);
}

/**
 * Find the .cabal file in a project directory.
 */
export async function findCabalFile(projectDir: string): Promise<string> {
  const { readdir } = await import("node:fs/promises");
  const files = await readdir(projectDir);
  const cabalFile = files.find((f) => f.endsWith(".cabal"));
  if (!cabalFile) {
    throw new Error(`No .cabal file found in ${projectDir}`);
  }
  return path.join(projectDir, cabalFile);
}

/**
 * Extract the package name from .cabal content.
 */
export function extractPackageName(content: string): string | null {
  const match = content.match(/^name:\s*(.+)/im);
  return match ? match[1]!.trim() : null;
}

/**
 * Read the package name from the .cabal file in a project directory.
 */
export async function parseCabalPackageName(
  projectDir: string
): Promise<string> {
  const cabalFile = await findCabalFile(projectDir);
  const content = await readFile(cabalFile, "utf-8");
  const name = extractPackageName(content);
  if (!name) {
    throw new Error(`No 'name:' field found in ${cabalFile}`);
  }
  return name;
}

/**
 * Extract module lists from .cabal content.
 *
 * Handles the format:
 *   exposed-modules:
 *     Lib
 *     HM.Syntax
 *     HM.Subst
 */
export function extractModules(content: string): CabalModules {
  const result: CabalModules = {
    library: [],
    executables: new Map(),
  };

  const lines = content.split("\n");
  let currentStanza: "library" | "executable" | null = null;
  let currentExeName = "";
  let inModuleList = false;
  let moduleListIndent = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;
    const trimmed = line.trim();

    // Detect stanza headers (no leading whitespace)
    if (/^library\b/i.test(line)) {
      currentStanza = "library";
      inModuleList = false;
      continue;
    }
    if (/^executable\s+(\S+)/i.test(line)) {
      const match = line.match(/^executable\s+(\S+)/i);
      currentStanza = "executable";
      currentExeName = match![1]!;
      result.executables.set(currentExeName, []);
      inModuleList = false;
      continue;
    }
    // Other stanza headers (test-suite, benchmark, etc.) — reset to prevent
    // their modules from leaking into library or executable module lists
    if (/^(test-suite|benchmark|common|flag|source-repository)\b/i.test(line)) {
      currentStanza = null;
      inModuleList = false;
      continue;
    }

    // Detect module list fields
    const moduleFieldMatch = trimmed.match(
      /^(exposed-modules|other-modules)\s*:\s*(.*)/i
    );
    if (moduleFieldMatch) {
      inModuleList = true;
      // Calculate indent of continuation lines (indent of field + some more)
      const fieldIndent = line.length - line.trimStart().length;
      moduleListIndent = fieldIndent + 2; // continuation must be indented more

      // There might be modules on the same line as the field
      const sameLine = moduleFieldMatch[2]!.trim();
      if (sameLine) {
        for (const mod of parseModuleNames(sameLine)) {
          addModule(result, currentStanza, currentExeName, mod);
        }
      }
      continue;
    }

    // Continuation of module list
    if (inModuleList) {
      const lineIndent = line.length - line.trimStart().length;
      if (trimmed === "" || lineIndent >= moduleListIndent) {
        if (trimmed !== "") {
          for (const mod of parseModuleNames(trimmed)) {
            addModule(result, currentStanza, currentExeName, mod);
          }
        }
      } else {
        inModuleList = false;
      }
    }
  }

  return result;
}

function addModule(
  result: CabalModules,
  stanza: "library" | "executable" | null,
  exeName: string,
  mod: string
): void {
  if (stanza === "library") {
    result.library.push(mod);
  } else if (stanza === "executable") {
    result.executables.get(exeName)?.push(mod);
  }
}

/**
 * Parse module names from a line, handling comma-separated and
 * whitespace-separated formats.
 */
function parseModuleNames(text: string): string[] {
  return text
    .split(/[,\s]+/)
    .map((s) => s.trim())
    .filter((s) => s.length > 0 && /^[A-Z]/.test(s));
}

/**
 * Convert a Haskell module name to a file path.
 * E.g. "HM.Syntax" -> "src/HM/Syntax.hs"
 */
export function moduleToFilePath(
  moduleName: string,
  srcDir: string = "src"
): string {
  return path.join(srcDir, moduleName.replace(/\./g, "/") + ".hs");
}

/**
 * Get the hs-source-dirs for the library stanza.
 * Defaults to "src" if not found.
 */
export async function getLibrarySrcDir(
  projectDir: string
): Promise<string> {
  const cabalFile = await findCabalFile(projectDir);
  const content = await readFile(cabalFile, "utf-8");

  const lines = content.split("\n");
  let inLibrary = false;

  for (const line of lines) {
    if (/^library\b/i.test(line)) {
      inLibrary = true;
      continue;
    }
    // New stanza starts
    if (inLibrary && /^\S/.test(line) && line.trim() !== "") {
      break;
    }
    if (inLibrary) {
      const match = line.match(/^\s+hs-source-dirs\s*:\s*(.+)/i);
      if (match) {
        return match[1]!.trim().split(/[,\s]+/)[0]!;
      }
    }
  }

  return "src";
}

/**
 * Extract build-depends package names from the library stanza.
 * Returns just the package names (without version constraints).
 * E.g. "base >= 4.14 && < 5, containers >= 0.6" → ["base", "containers"]
 */
export async function extractBuildDepends(projectDir: string): Promise<string[]> {
  const cabalFile = await findCabalFile(projectDir);
  const content = await readFile(cabalFile, "utf-8");
  return extractBuildDependsFromContent(content);
}

export function extractBuildDependsFromContent(content: string): string[] {
  const deps: string[] = [];
  const lines = content.split("\n");
  let inLibrary = false;
  let inBuildDepends = false;
  let buildDependsIndent = 0;

  for (const line of lines) {
    if (/^library\b/i.test(line)) {
      inLibrary = true;
      inBuildDepends = false;
      continue;
    }
    if (inLibrary && /^\S/.test(line) && line.trim() !== "") {
      inLibrary = false;
      inBuildDepends = false;
      continue;
    }
    if (inLibrary) {
      const bdMatch = line.match(/^\s+build-depends\s*:\s*(.*)/i);
      if (bdMatch) {
        inBuildDepends = true;
        buildDependsIndent = line.length - line.trimStart().length + 2;
        if (bdMatch[1]!.trim()) {
          for (const dep of parseDeps(bdMatch[1]!)) deps.push(dep);
        }
        continue;
      }
      if (inBuildDepends) {
        const lineIndent = line.length - line.trimStart().length;
        const trimmed = line.trim();
        if (trimmed === "" || lineIndent >= buildDependsIndent) {
          if (trimmed) for (const dep of parseDeps(trimmed)) deps.push(dep);
        } else {
          inBuildDepends = false;
        }
      }
    }
  }
  return deps;
}

function parseDeps(text: string): string[] {
  return text
    .split(",")
    .map((d) => d.trim().split(/\s/)[0]!.trim())
    .filter((d) => d.length > 0 && /^[a-zA-Z]/.test(d));
}
