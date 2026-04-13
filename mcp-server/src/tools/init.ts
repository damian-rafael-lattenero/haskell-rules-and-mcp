/**
 * ghci_init — Create a Haskell project from scratch.
 * Creates .cabal, cabal.project, and src/ directory structure in the specified
 * or current directory. Generic — works for any Haskell project, not tied to
 * any specific directory layout.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { mkdir, writeFile, access, readdir } from "node:fs/promises";
import path from "node:path";
import type { ToolContext } from "./registry.js";

export async function handleInit(
  targetDir: string,
  args: {
    name: string;
    modules: string[];
    deps?: string[];
    language?: string;
  }
): Promise<string> {
  const { name, modules, deps, language } = args;

  // Check if a .cabal already exists in the target directory
  try {
    const files = await readdir(targetDir);
    const existingCabal = files.find(f => f.endsWith(".cabal"));
    if (existingCabal) {
      return JSON.stringify({
        success: false,
        error: `A .cabal file already exists in ${targetDir} (${existingCabal}). Use ghci_scaffold to add modules.`,
      });
    }
  } catch {
    // Directory doesn't exist yet — create it
    await mkdir(targetDir, { recursive: true });
  }

  const allDeps = ["base >= 4.20 && < 5"];
  if (deps) {
    for (const d of deps) {
      if (!d.startsWith("base")) allDeps.push(d);
    }
  }
  if (!allDeps.some(d => d.includes("QuickCheck"))) {
    allDeps.push("QuickCheck >= 2.14");
  }

  const lang = language ?? "GHC2024";

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

  await writeFile(path.join(targetDir, `${name}.cabal`), cabalContent, "utf-8");
  await writeFile(path.join(targetDir, "cabal.project"), "packages: .\n", "utf-8");

  const srcDir = path.join(targetDir, "src");
  await mkdir(srcDir, { recursive: true });

  for (const mod of modules) {
    const parts = mod.split(".");
    if (parts.length > 1) {
      const dir = path.join(srcDir, ...parts.slice(0, -1));
      await mkdir(dir, { recursive: true });
    }
  }

  return JSON.stringify({
    success: true,
    projectDir: targetDir,
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
      "and src/ directory structure in the current project directory. " +
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
        'Default language. Default: "GHC2024". Other: "Haskell2010"'
      ),
    },
    async ({ name, modules, deps, language }) => {
      const result = await handleInit(ctx.getProjectDir(), { name, modules, deps, language });
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
