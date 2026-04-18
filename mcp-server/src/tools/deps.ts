/**
 * ghci_deps — Manage project dependencies without manual .cabal editing.
 * Actions: add, remove, list, graph
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, writeFile, readdir, stat } from "node:fs/promises";
import path from "node:path";
import { findCabalFile, getLibrarySrcDir } from "../parsers/cabal-parser.js";
import { type ToolContext, registerStrictTool } from "./registry.js";

/** A dependency entry with name and optional version constraint. */
export interface DepEntry {
  name: string;
  version: string;
  raw: string;
}

/** Parse raw dependency lines into DepEntry objects. */
function parseDependencyLines(lines: string[]): DepEntry[] {
  return lines
    .map((raw) => {
      const trimmed = raw.trim().replace(/,$/, "").trim();
      if (!trimmed || !/^[a-zA-Z]/.test(trimmed)) return null;
      const spaceIdx = trimmed.search(/\s/);
      const name = spaceIdx === -1 ? trimmed : trimmed.slice(0, spaceIdx);
      const version = spaceIdx === -1 ? "" : trimmed.slice(spaceIdx).trim();
      return { name, version, raw: trimmed };
    })
    .filter((e): e is DepEntry => e !== null);
}

/**
 * Extract the full raw dependency lines from the library stanza's build-depends.
 * Returns the lines with their original leading whitespace/commas.
 */
function extractLibraryBuildDependsLines(content: string): {
  startLine: number;
  endLine: number;
  lines: string[];
} | null {
  const lines = content.split("\n");
  let inLibrary = false;
  let inBuildDepends = false;
  let buildDependsIndent = 0;
  let startLine = -1;
  let endLine = -1;
  const depLines: string[] = [];

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;

    if (/^library\b/i.test(line)) {
      inLibrary = true;
      inBuildDepends = false;
      continue;
    }

    if (inLibrary && /^\S/.test(line) && line.trim() !== "") {
      if (inBuildDepends) endLine = i - 1;
      inLibrary = false;
      inBuildDepends = false;
      continue;
    }

    if (inLibrary) {
      const bdMatch = line.match(/^(\s+)build-depends\s*:\s*(.*)/i);
      if (bdMatch) {
        inBuildDepends = true;
        buildDependsIndent = bdMatch[1]!.length + 2;
        startLine = i;
        if (bdMatch[2]!.trim()) depLines.push(bdMatch[2]!.trim());
        continue;
      }

      if (inBuildDepends) {
        const lineIndent = line.length - line.trimStart().length;
        const trimmed = line.trim();
        if (trimmed === "" || lineIndent >= buildDependsIndent) {
          if (trimmed) depLines.push(trimmed);
        } else {
          endLine = i - 1;
          inBuildDepends = false;
        }
      }
    }
  }

  if (inBuildDepends && startLine !== -1) endLine = lines.length - 1;
  if (startLine === -1) return null;

  return { startLine, endLine, lines: depLines };
}

/**
 * Rewrite the build-depends section in library stanza with new dep entries.
 * Preserves original indentation style.
 */
function rewriteBuildDepends(content: string, entries: DepEntry[]): string {
  const lines = content.split("\n");
  let inLibrary = false;
  let inBuildDepends = false;
  let buildDependsIndent = 4;
  let startLine = -1;
  let endLine = -1;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]!;

    if (/^library\b/i.test(line)) {
      inLibrary = true;
      inBuildDepends = false;
      continue;
    }

    if (inLibrary && /^\S/.test(line) && line.trim() !== "") {
      if (inBuildDepends) endLine = i - 1;
      inLibrary = false;
      inBuildDepends = false;
      continue;
    }

    if (inLibrary) {
      const bdMatch = line.match(/^(\s+)build-depends\s*:/i);
      if (bdMatch) {
        inBuildDepends = true;
        buildDependsIndent = bdMatch[1]!.length + 2;
        startLine = i;
        continue;
      }

      if (inBuildDepends) {
        const lineIndent = line.length - line.trimStart().length;
        const trimmed = line.trim();
        if (trimmed === "" || lineIndent >= buildDependsIndent) {
          // still in build-depends
        } else {
          endLine = i - 1;
          inBuildDepends = false;
        }
      }
    }
  }

  if (inBuildDepends && startLine !== -1) endLine = lines.length - 1;
  if (startLine === -1) return content;

  const indent = " ".repeat(buildDependsIndent);

  // Build new build-depends block
  const newBlock: string[] = [];
  newBlock.push(`${" ".repeat(buildDependsIndent - 2)}build-depends:`);
  entries.forEach((e, idx) => {
    const comma = idx < entries.length - 1 ? "," : "";
    const val = e.version ? `${e.name} ${e.version}` : e.name;
    newBlock.push(`${indent}${val}${comma}`);
  });

  // Replace lines from startLine to endLine
  const before = lines.slice(0, startLine);
  const after = lines.slice(endLine + 1);
  return [...before, ...newBlock, ...after].join("\n");
}

// ─── Graph: Module Import Analysis ───────────────────────────────────────────

interface GraphResult {
  nodes: string[];
  edges: Array<{ from: string; to: string }>;
  cycles: string[][];
  orphans: string[];
}

/** Recursively collect all .hs files under a directory. */
async function collectHsFiles(dir: string): Promise<string[]> {
  const files: string[] = [];
  let entries: string[];
  try {
    entries = await readdir(dir);
  } catch {
    return files;
  }
  for (const entry of entries) {
    const full = path.join(dir, entry);
    let s;
    try { s = await stat(full); } catch { continue; }
    if (s.isDirectory()) {
      files.push(...(await collectHsFiles(full)));
    } else if (entry.endsWith(".hs")) {
      files.push(full);
    }
  }
  return files;
}

/** Extract the module name declared in a .hs file. */
function extractModuleName(content: string): string | null {
  const m = content.match(/^module\s+([\w.]+)/m);
  return m ? m[1]! : null;
}

/** Extract all import module names from .hs content. */
function extractImports(content: string): string[] {
  const imports: string[] = [];
  for (const m of content.matchAll(/^import\s+(?:qualified\s+)?(?:"[^"]*"\s+)?([\w.]+)/gm)) {
    imports.push(m[1]!);
  }
  return imports;
}

/** DFS-based cycle detection. Returns list of cycles (each as array of node names). */
function findCycles(adjacency: Map<string, Set<string>>): string[][] {
  const visited = new Set<string>();
  const inStack = new Set<string>();
  const cycles: string[][] = [];

  function dfs(node: string, stack: string[]): void {
    visited.add(node);
    inStack.add(node);
    stack.push(node);

    for (const neighbor of adjacency.get(node) ?? []) {
      if (!visited.has(neighbor)) {
        dfs(neighbor, stack);
      } else if (inStack.has(neighbor)) {
        // Found a cycle — extract the cycle portion
        const cycleStart = stack.indexOf(neighbor);
        if (cycleStart !== -1) {
          cycles.push(stack.slice(cycleStart));
        }
      }
    }

    stack.pop();
    inStack.delete(node);
  }

  for (const node of adjacency.keys()) {
    if (!visited.has(node)) {
      dfs(node, []);
    }
  }

  return cycles;
}

async function buildModuleGraph(projectDir: string): Promise<GraphResult> {
  // Determine src directory
  let srcDir = "src";
  try {
    srcDir = await getLibrarySrcDir(projectDir);
  } catch { /* default */ }

  const fullSrcDir = path.resolve(projectDir, srcDir);
  const hsFiles = await collectHsFiles(fullSrcDir);

  // Build module name → content map
  const moduleContents = new Map<string, string>();
  for (const file of hsFiles) {
    const content = await readFile(file, "utf-8");
    const name = extractModuleName(content);
    if (name) {
      moduleContents.set(name, content);
    }
  }

  const projectModules = new Set(moduleContents.keys());
  const edges: Array<{ from: string; to: string }> = [];
  const adjacency = new Map<string, Set<string>>();

  for (const [modName, content] of moduleContents) {
    adjacency.set(modName, new Set());
    const imports = extractImports(content);
    for (const imp of imports) {
      // Only include edges to project-internal modules
      if (projectModules.has(imp) && imp !== modName) {
        edges.push({ from: modName, to: imp });
        adjacency.get(modName)!.add(imp);
      }
    }
  }

  const cycles = findCycles(adjacency);

  // Orphans = modules that nobody imports
  const imported = new Set(edges.map((e) => e.to));
  const orphans = [...projectModules].filter((m) => !imported.has(m));

  return {
    nodes: [...projectModules].sort(),
    edges,
    cycles,
    orphans: orphans.sort(),
  };
}

// ─── Main handler ─────────────────────────────────────────────────────────────

export async function handleDeps(
  projectDir: string,
  args: { action: string; package?: string; version?: string }
): Promise<string> {
  const { action } = args;

  let cabalFile: string;
  try {
    cabalFile = await findCabalFile(projectDir);
  } catch {
    return JSON.stringify({ success: false, error: `No .cabal file found in ${projectDir}` });
  }

  const content = await readFile(cabalFile, "utf-8");
  const extracted = extractLibraryBuildDependsLines(content);

  if (action === "list") {
    if (!extracted) {
      return JSON.stringify({
        success: true,
        dependencies: [],
        note: "No library stanza with build-depends found",
      });
    }
    const deps = parseDependencyLines(extracted.lines);
    return JSON.stringify({ success: true, dependencies: deps });
  }

  if (action === "add") {
    const pkg = args.package;
    if (!pkg) {
      return JSON.stringify({ success: false, error: "package parameter is required for action 'add'" });
    }
    if (!extracted) {
      return JSON.stringify({ success: false, error: "No library stanza with build-depends found in .cabal" });
    }

    const deps = parseDependencyLines(extracted.lines);
    const existing = deps.find((d) => d.name === pkg);
    if (existing) {
      return JSON.stringify({
        success: true,
        status: "already_present",
        package: pkg,
        message: `Package '${pkg}' is already in build-depends`,
      });
    }

    const newEntry: DepEntry = {
      name: pkg,
      version: args.version ?? "",
      raw: args.version ? `${pkg} ${args.version}` : pkg,
    };
    deps.push(newEntry);
    const newContent = rewriteBuildDepends(content, deps);
    await writeFile(cabalFile, newContent, "utf-8");

    return JSON.stringify({
      success: true,
      status: "added",
      package: pkg,
      ...(args.version ? { version: args.version } : {}),
      message: `Added '${newEntry.raw}' to build-depends. Run ghci_session(restart) to pick up the new dependency.`,
    });
  }

  if (action === "remove") {
    const pkg = args.package;
    if (!pkg) {
      return JSON.stringify({ success: false, error: "package parameter is required for action 'remove'" });
    }

    // Protect base — removing it would break the project
    if (pkg === "base") {
      return JSON.stringify({
        success: false,
        error: "Cannot remove 'base': it is a protected core dependency required by all Haskell projects.",
      });
    }

    if (!extracted) {
      return JSON.stringify({ success: false, error: "No library stanza with build-depends found in .cabal" });
    }

    const deps = parseDependencyLines(extracted.lines);
    const idx = deps.findIndex((d) => d.name === pkg);
    if (idx === -1) {
      return JSON.stringify({
        success: false,
        error: `Package '${pkg}' not found in build-depends. Current packages: ${deps.map((d) => d.name).join(", ")}`,
      });
    }

    deps.splice(idx, 1);
    const newContent = rewriteBuildDepends(content, deps);
    await writeFile(cabalFile, newContent, "utf-8");

    return JSON.stringify({
      success: true,
      status: "removed",
      package: pkg,
      message: `Removed '${pkg}' from build-depends. Run ghci_session(restart) to apply.`,
    });
  }

  if (action === "graph") {
    try {
      const graph = await buildModuleGraph(projectDir);
      return JSON.stringify({
        success: true,
        ...graph,
        summary: `${graph.nodes.length} module(s), ${graph.edges.length} import edge(s)` +
          (graph.cycles.length > 0 ? `, ${graph.cycles.length} cycle(s) detected` : "") +
          (graph.orphans.length > 0 ? `, ${graph.orphans.length} orphan(s)` : ""),
      });
    } catch (err) {
      return JSON.stringify({
        success: false,
        error: `Failed to build module graph: ${err instanceof Error ? err.message : String(err)}`,
      });
    }
  }

  return JSON.stringify({ success: false, error: `Unknown action '${action}'. Valid: add, remove, list, graph` });
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(server, ctx, 
    "ghci_deps",
    "Manage project dependencies in the .cabal file without manual editing. " +
      "Actions: 'add' to add a package, 'remove' to remove one, 'list' to see current dependencies, " +
      "'graph' to visualize module import graph. " +
      "After add/remove, run ghci_session(action='restart') to reload with updated deps.",
    {
      action: z.enum(["add", "remove", "list", "graph"]).describe(
        '"add" to add a package, "remove" to remove, "list" to show current deps, "graph" for import graph'
      ),
      package: z.string().optional().describe(
        'Package name to add or remove. Examples: "containers", "text", "mtl". Required for add/remove.'
      ),
      version: z.string().optional().describe(
        'Version constraint for add. Examples: ">= 2.0", "^>= 1.4", ">= 0.6 && < 0.7". Omit for no constraint.'
      ),
    },
    async ({ action, package: pkg, version }) => {
      const result = await handleDeps(ctx.getProjectDir(), { action, package: pkg, version });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
