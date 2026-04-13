/**
 * ghci_init — Create a Haskell project from scratch.
 * Generates .cabal file, cabal.project, and src/ directory structure.
 * Eliminates the only manual step in the MCP workflow.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { mkdir, writeFile, access } from "node:fs/promises";
import path from "node:path";
import type { ToolContext } from "./registry.js";

export async function handleInit(
  projectDir: string,
  args: {
    name: string;
    modules: string[];
    deps?: string[];
    language?: string;
  }
): Promise<string> {
  const { name, modules, deps, language } = args;
  const allDeps = ["base >= 4.14 && < 5"];
  if (deps) {
    for (const d of deps) {
      if (!d.startsWith("base")) allDeps.push(d);
    }
  }
  // Always include QuickCheck if not explicitly listed
  if (!allDeps.some(d => d.includes("QuickCheck"))) {
    allDeps.push("QuickCheck >= 2.14");
  }

  const lang = language ?? "Haskell2010";

  // Check if .cabal already exists
  const cabalPath = path.join(projectDir, `${name}.cabal`);
  try {
    await access(cabalPath);
    return JSON.stringify({
      success: false,
      error: `${name}.cabal already exists in ${projectDir}. Use ghci_scaffold to add modules.`,
    });
  } catch {
    // Good — doesn't exist yet
  }

  // Generate .cabal content
  const modulesSection = modules.length > 0
    ? modules.map(m => `    ${m}`).join("\n")
    : "    Lib";

  const depsSection = allDeps.map(d => `    ${d}`).join(",\n");

  const cabalContent = `cabal-version:      2.4
name:               ${name}
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:
${modulesSection}
  build-depends:
${depsSection}
  hs-source-dirs:   src
  default-language:  ${lang}
  ghc-options:       -Wall
`;

  // Write files
  await writeFile(cabalPath, cabalContent, "utf-8");
  await writeFile(path.join(projectDir, "cabal.project"), "packages: .\n", "utf-8");

  // Create src directory
  const srcDir = path.join(projectDir, "src");
  await mkdir(srcDir, { recursive: true });

  // Create nested directories for dotted modules
  for (const mod of modules) {
    const parts = mod.split(".");
    if (parts.length > 1) {
      const dir = path.join(srcDir, ...parts.slice(0, -1));
      await mkdir(dir, { recursive: true });
    }
  }

  return JSON.stringify({
    success: true,
    cabalFile: `${name}.cabal`,
    modules: modules.length > 0 ? modules : ["Lib"],
    dependencies: allDeps,
    language: lang,
    _nextStep: `Project created. Run ghci_scaffold(signatures={...}) to generate typed stubs, then ghci_session(restart) to start GHCi.`,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_init",
    "Create a new Haskell project from scratch. Generates .cabal file, cabal.project, " +
      "and src/ directory structure. Use this instead of manually writing .cabal files. " +
      "After init, run ghci_scaffold(signatures=...) to add typed stubs.",
    {
      name: z.string().describe(
        'Project name (used for .cabal filename). Examples: "expr-eval", "parser-lib"'
      ),
      modules: z.array(z.string()).describe(
        'Module names for exposed-modules. Examples: ["Expr.Syntax", "Expr.Eval"]'
      ),
      deps: z.array(z.string()).optional().describe(
        'Additional dependencies beyond base and QuickCheck. Examples: ["containers", "mtl >= 2.2"]'
      ),
      language: z.string().optional().describe(
        'Default language. Default: "Haskell2010". Other: "GHC2024"'
      ),
    },
    async ({ name, modules, deps, language }) => {
      const result = await handleInit(ctx.getProjectDir(), { name, modules, deps, language });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
