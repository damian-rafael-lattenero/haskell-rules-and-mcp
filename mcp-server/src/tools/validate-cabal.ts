import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { readFile, readdir } from "node:fs/promises";
import path from "node:path";
import type { ToolContext } from "./registry.js";

interface ParsedImport {
  module: string;
  qualified: boolean;
  as?: string;
}

const PACKAGE_BY_PREFIX: Array<{ prefix: string; pkg: string }> = [
  { prefix: "Data.Map", pkg: "containers" },
  { prefix: "Data.Set", pkg: "containers" },
  { prefix: "Data.Sequence", pkg: "containers" },
  { prefix: "Data.Tree", pkg: "containers" },
  { prefix: "Data.IntMap", pkg: "containers" },
  { prefix: "Data.IntSet", pkg: "containers" },
  { prefix: "Data.Text", pkg: "text" },
  { prefix: "Data.Vector", pkg: "vector" },
];

export function extractImports(source: string): ParsedImport[] {
  const imports: ParsedImport[] = [];
  const lines = source.split(/\r?\n/);

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed.startsWith("import ")) continue;

    const qualifiedMatch = /^import\s+qualified\s+([A-Z][A-Za-z0-9_.']*)(?:\s+as\s+([A-Z][A-Za-z0-9_']*))?/.exec(trimmed);
    if (qualifiedMatch) {
      imports.push({
        module: qualifiedMatch[1]!,
        qualified: true,
        ...(qualifiedMatch[2] ? { as: qualifiedMatch[2] } : {}),
      });
      continue;
    }

    const plainMatch = /^import\s+([A-Z][A-Za-z0-9_.']*)/.exec(trimmed);
    if (plainMatch) {
      imports.push({ module: plainMatch[1]!, qualified: false });
    }
  }

  return imports;
}

export function mapImportsToPackages(importModules: string[]): string[] {
  const required = new Set<string>();

  for (const moduleName of importModules) {
    for (const { prefix, pkg } of PACKAGE_BY_PREFIX) {
      if (moduleName.startsWith(prefix)) required.add(pkg);
    }
  }

  return [...required].sort();
}

export function extractTestSuiteDeps(cabalContent: string): string[] {
  const deps = new Set<string>();
  const lines = cabalContent.split(/\r?\n/);
  let inTestSuite = false;
  let readingDeps = false;

  for (const rawLine of lines) {
    const line = rawLine.replace(/\r$/, "");
    const trimmed = line.trim();

    if (/^(library|executable|benchmark|foreign-library)\b/.test(trimmed)) {
      inTestSuite = false;
      readingDeps = false;
      continue;
    }

    if (/^test-suite\b/.test(trimmed)) {
      inTestSuite = true;
      readingDeps = false;
      continue;
    }

    if (!inTestSuite) continue;

    if (/^build-depends:/.test(trimmed)) {
      readingDeps = true;
      const rest = trimmed.replace(/^build-depends:\s*/, "");
      for (const dep of rest.split(",")) {
        const name = dep.trim().split(/\s+/)[0];
        if (name) deps.add(name);
      }
      continue;
    }

    if (readingDeps) {
      if (!/^\s+/.test(line) || trimmed.includes(":")) {
        readingDeps = false;
        continue;
      }

      for (const dep of trimmed.split(",")) {
        const name = dep.trim().split(/\s+/)[0];
        if (name) deps.add(name);
      }
    }
  }

  return [...deps].sort();
}

async function findCabalFile(projectDir: string): Promise<string | null> {
  const files = await readdir(projectDir);
  const match = files.find((f) => f.endsWith(".cabal"));
  return match ? path.join(projectDir, match) : null;
}

export async function handleValidateCabal(projectDir: string): Promise<string> {
  const cabalFile = await findCabalFile(projectDir);
  if (!cabalFile) {
    return JSON.stringify({
      success: false,
      error: "No .cabal file found in project directory.",
    });
  }

  const cabalContent = await readFile(cabalFile, "utf-8");
  const testDeps = extractTestSuiteDeps(cabalContent);
  const specPath = path.join(projectDir, "test", "Spec.hs");

  let specContent = "";
  try {
    specContent = await readFile(specPath, "utf-8");
  } catch {
    return JSON.stringify({
      success: true,
      checkedFile: path.relative(projectDir, cabalFile),
      note: "No test/Spec.hs found. Nothing to validate for test-suite imports.",
    });
  }

  const parsedImports = extractImports(specContent);
  const requiredPackages = mapImportsToPackages(parsedImports.map((i) => i.module));
  const missing = requiredPackages.filter((pkg) => !testDeps.includes(pkg));

  if (missing.length > 0) {
    return JSON.stringify({
      success: false,
      error: "Missing test-suite dependencies required by test/Spec.hs imports.",
      missingDependencies: missing,
      errors: missing.map((pkg) => ({
        type: "missing-test-dependency",
        package: pkg,
        suggestion: `Add '${pkg}' to test-suite build-depends.`,
      })),
    });
  }

  return JSON.stringify({
    success: true,
    checkedFile: path.relative(projectDir, cabalFile),
    testSuiteDependencies: testDeps,
    requiredPackages,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_validate_cabal",
    "Validate Cabal test-suite dependencies against imports used by test/Spec.hs.",
    {},
    async () => {
      const result = await handleValidateCabal(ctx.getProjectDir());
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
