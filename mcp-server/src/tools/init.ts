/**
 * ghci_init — Create a Haskell project from scratch.
 * Creates .cabal, cabal.project, and src/ directory structure in the specified
 * or current directory. Generic — works for any Haskell project, not tied to
 * any specific directory layout.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";
import { mkdir, writeFile, access, readdir, stat } from "node:fs/promises";
import path from "node:path";
import type { ToolContext } from "./registry.js";

/**
 * Check if we're currently in an existing Haskell project directory.
 * Returns the .cabal file name if found, null otherwise.
 */
async function checkExistingProject(dir: string): Promise<string | null> {
  try {
    const files = await readdir(dir);
    for (const file of files) {
      if (file.endsWith(".cabal")) {
        const fullPath = path.join(dir, file);
        const stats = await stat(fullPath);
        if (stats.isFile()) {
          return file;
        }
      }
    }
    return null;
  } catch {
    return null;
  }
}

export async function handleInit(
  targetDir: string,
  currentProjectDir: string,
  workspaceRoot: string,
  args: {
    name: string;
    modules: string[];
    deps?: string[];
    language?: string;
    target_path?: string;
    force_in_current?: boolean;
    build_tool?: "cabal" | "stack";
    test_suite?: boolean;
  }
): Promise<string> {
  const { name, modules, deps, language, target_path, force_in_current, build_tool, test_suite } = args;

  // SMART PATH RESOLUTION:
  // 1. If target_path provided → use it (explicit user intent)
  // 2. If no target_path:
  //    a. If name matches current project → work in current project dir
  //    b. Otherwise → suggest creating in workspace root or subdirectory
  
  let resolvedTargetDir = targetDir;
  const currentProjectCabal = await checkExistingProject(currentProjectDir);
  const currentProjectName = currentProjectCabal?.replace(".cabal", "");
  
  // Case 1: User provided explicit target_path
  if (target_path) {
    resolvedTargetDir = path.resolve(workspaceRoot, target_path);
  }
  // Case 2: No target_path, but name matches current project
  else if (currentProjectName && currentProjectName === name) {
    // User wants to reinit/work with current project
    resolvedTargetDir = currentProjectDir;
  }
  // Case 3: No target_path, name differs from current project
  else if (currentProjectName && currentProjectName !== name) {
    // Ambiguous: user might want new project alongside or inside current
    return JSON.stringify({
      success: false,
      error: `Ambiguous intent: You're in project '${currentProjectName}' but trying to create '${name}'.`,
      context: {
        currentProject: currentProjectName,
        currentProjectDir,
        intendedProject: name,
        workspaceRoot,
      },
      question: "Where do you want to create the new project?",
      options: [
        {
          id: "current_project",
          description: `Reinit current project '${currentProjectName}' (dangerous - will overwrite)`,
          action: `Use force_in_current: true if you really want this`,
        },
        {
          id: "workspace_root",
          description: `Create '${name}' in workspace root: ${workspaceRoot}/${name}/`,
          action: `Use: target_path: "${name}"`,
        },
        {
          id: "playground",
          description: `Create '${name}' in playground: ${workspaceRoot}/playground/${name}/`,
          action: `Use: target_path: "playground/${name}"`,
        },
        {
          id: "custom",
          description: `Create '${name}' in custom location`,
          action: `Use: target_path: "your/custom/path/${name}"`,
        },
      ],
      hint: "Specify target_path parameter to indicate where to create the project.",
    });
  }
  // Case 4: No current project, no target_path → create in workspace root
  else {
    resolvedTargetDir = path.join(workspaceRoot, name);
  }
  
  // Check if a .cabal already exists in the resolved target directory
  const existingCabal = await checkExistingProject(resolvedTargetDir);
  
  if (existingCabal && !force_in_current) {
    const existingProjectName = existingCabal.replace(".cabal", "");
    // We're trying to init in a directory that already has a project
    return JSON.stringify({
      success: false,
      error: `A .cabal file already exists in ${resolvedTargetDir} (${existingCabal}).`,
      context: {
        targetDir: resolvedTargetDir,
        existingProject: existingProjectName,
        intendedProject: name,
      },
      suggestions: [
        existingProjectName === name
          ? "If you want to add modules to the existing project, use ghci_scaffold instead."
          : `Project '${existingProjectName}' exists but you requested '${name}'. Check your project name.`,
        `If you want to REPLACE the existing project (dangerous), use: force_in_current: true`,
        `If you want a NEW project, use a different target_path.`,
      ],
    });
  }
  
  // Proceed with project creation
  try {
    await mkdir(resolvedTargetDir, { recursive: true });
  } catch {
    // Directory might already exist but without .cabal — that's fine
  }

  const allDeps = ["base >= 4.20 && < 5", "containers"];
  if (deps) {
    for (const d of deps) {
      if (!d.startsWith("base") && !allDeps.includes(d)) allDeps.push(d);
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

  const testSuiteSection = test_suite ? `
test-suite ${name}-test
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  build-depends:
    base >= 4.20 && < 5,
    ${name},
    QuickCheck >= 2.14
  default-language: ${lang}
  ghc-options:       -Wall
` : "";

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
${testSuiteSection}`;

  await writeFile(path.join(resolvedTargetDir, `${name}.cabal`), cabalContent, "utf-8");
  await writeFile(path.join(resolvedTargetDir, "cabal.project"), "packages: .\n", "utf-8");

  // Stack support: generate minimal stack.yaml when build_tool is "stack"
  if (build_tool === "stack") {
    const stackYaml = `resolver: lts-23.0
packages:
  - .
`;
    await writeFile(path.join(resolvedTargetDir, "stack.yaml"), stackYaml, "utf-8");
  }

  const srcDir = path.join(resolvedTargetDir, "src");
  await mkdir(srcDir, { recursive: true });

  for (const mod of modules) {
    const parts = mod.split(".");
    if (parts.length > 1) {
      const dir = path.join(srcDir, ...parts.slice(0, -1));
      await mkdir(dir, { recursive: true });
    }
  }

  // Create test/ scaffold when test_suite is requested
  let testSuiteCreated = false;
  if (test_suite) {
    const testDir = path.join(resolvedTargetDir, "test");
    await mkdir(testDir, { recursive: true });
    const specContent = `module Main where

import Test.QuickCheck

-- Generated by ghci_init with test_suite=true.
-- Run ghci_quickcheck_export(output_path="test/Spec.hs") to populate this file
-- with your saved QuickCheck properties.
main :: IO ()
main = putStrLn "No properties exported yet. Run ghci_quickcheck_export first."
`;
    await writeFile(path.join(testDir, "Spec.hs"), specContent, "utf-8");
    testSuiteCreated = true;
  }

  // Smart next step based on where we created the project
  let nextStep: string;
  if (resolvedTargetDir === currentProjectDir) {
    nextStep =
      `Project created/updated in current directory. ` +
      `Run ghci_scaffold(signatures={...}) to generate typed stubs, then ghci_session(restart) to start GHCi.`;
  } else {
    const relativePath = path.relative(workspaceRoot, resolvedTargetDir);
    nextStep =
      `Project created at ${relativePath}. ` +
      `Run ghci_switch_project(project="${name}") — it switches AND auto-scaffolds source files on switch. ` +
      `Only call ghci_scaffold(signatures={...}) separately if you want typed stubs with = undefined bodies for ghci_suggest hole-fit mode.`;
  }

  return JSON.stringify({
    success: true,
    projectDir: resolvedTargetDir,
    relativePath: path.relative(workspaceRoot, resolvedTargetDir),
    cabalFile: `${name}.cabal`,
    modules: modules.length > 0 ? modules : ["Lib"],
    dependencies: allDeps,
    dependencyDefaultsApplied: allDeps.filter((dep) => dep === "containers" || dep.includes("QuickCheck")),
    language: lang,
    ...(testSuiteCreated ? { testSuite: { created: true, specFile: "test/Spec.hs" } } : {}),
    _nextStep: nextStep,
  });
}

export function register(server: McpServer, ctx: ToolContext): void {
  server.tool(
    "ghci_init",
    "Create a new Haskell project from scratch. Generates .cabal file, cabal.project, and src/ directory structure. " +
      "SMART PATH RESOLUTION: " +
      "1) If target_path provided → uses it explicitly. " +
      "2) If name matches current project → works in current project directory. " +
      "3) If name differs → asks for clarification (workspace root vs playground vs custom). " +
      "4) If no current project → creates in workspace root. " +
      "This prevents accidental overwrites and makes intent clear.",
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
      target_path: z.string().optional().describe(
        'Optional: relative path from workspace root where to create the project. ' +
        'Examples: "expr-eval" (creates in root), "playground/expr-eval" (in playground), "projects/my-lib". ' +
        'If not provided, uses smart resolution based on current project context.'
      ),
      force_in_current: z.boolean().optional().describe(
        'Advanced: Set to true to skip the smart detection and force project creation even if .cabal exists. Use only when you know what you are doing.'
      ),
      build_tool: z.enum(["cabal", "stack"]).optional().describe(
        'Build tool to use. "cabal" (default): generates .cabal + cabal.project. ' +
        '"stack": also generates stack.yaml with a recent LTS resolver.'
      ),
      test_suite: z.boolean().optional().describe(
        'If true, adds a test-suite stanza to the .cabal file and creates test/Spec.hs. ' +
        'Use before ghci_quickcheck_export to have a ready test target for CI/CD.'
      ),
    },
    async ({ name, modules, deps, language, target_path, force_in_current, build_tool, test_suite }) => {
      const workspaceRoot = ctx.getBaseDir();
      const currentProjectDir = ctx.getProjectDir();
      
      // Pass a dummy targetDir (will be resolved in handleInit)
      const result = await handleInit(
        workspaceRoot, // Used as base for resolution
        currentProjectDir, 
        workspaceRoot,
        { 
          name, 
          modules, 
          deps, 
          language,
          target_path,
          force_in_current,
          build_tool,
          test_suite,
        }
      );
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
