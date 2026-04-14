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

/**
 * Check if target directory or any of its immediate subdirectories contain Haskell projects.
 * Returns info about found projects.
 */
async function checkForConflictingProjects(
  targetDir: string,
  intendedName: string
): Promise<{ hasDirectConflict: boolean; hasSubdirProjects: boolean; subdirProjects: string[] }> {
  const result = {
    hasDirectConflict: false,
    hasSubdirProjects: false,
    subdirProjects: [] as string[],
  };

  // Check if target directory itself has a .cabal
  const directCabal = await checkExistingProject(targetDir);
  if (directCabal) {
    result.hasDirectConflict = true;
    return result;
  }

  // Check immediate subdirectories for .cabal files
  try {
    const entries = await readdir(targetDir);
    for (const entry of entries) {
      const subPath = path.join(targetDir, entry);
      try {
        const stats = await stat(subPath);
        if (stats.isDirectory()) {
          const subCabal = await checkExistingProject(subPath);
          if (subCabal) {
            result.hasSubdirProjects = true;
            result.subdirProjects.push(entry);
          }
        }
      } catch {
        // Skip entries we can't read
      }
    }
  } catch {
    // Target directory doesn't exist yet - that's fine
  }

  return result;
}

function buildTestSuiteDeps(allDeps: string[]): string[] {
  const testDeps = allDeps.filter((d) => !d.startsWith("base"));
  const quickCheckIndex = testDeps.findIndex((d) => d.includes("QuickCheck"));

  if (quickCheckIndex > 0) {
    const [quickCheck] = testDeps.splice(quickCheckIndex, 1);
    testDeps.push(quickCheck!);
  }

  return testDeps;
}

export function generateTestSuiteSection(
  packageName: string,
  allDeps: string[],
  lang: string
): string {
  const testDeps = buildTestSuiteDeps(allDeps);
  const renderedDeps = testDeps.map((dep) => `    ${dep}`).join(",\n");

  const copiedDeps = renderedDeps.length > 0 ? `${renderedDeps},\n` : "";
  return `
test-suite ${packageName}-test
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  build-depends:
    base >= 4.20 && < 5,
    ${packageName},
${copiedDeps}  default-language: ${lang}
  ghc-options:       -Wall
`;
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
          id: "subdirectory",
          description: `Create '${name}' in a subdirectory`,
          action: `Use: target_path: "subdirectory/${name}" (replace 'subdirectory' with your desired folder name)`,
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
  
  // CRITICAL VALIDATION: Check for conflicting projects
  const conflicts = await checkForConflictingProjects(resolvedTargetDir, name);
  
  // Case A: Direct conflict - target directory already has a .cabal file
  if (conflicts.hasDirectConflict && !force_in_current) {
    const existingCabal = await checkExistingProject(resolvedTargetDir);
    const existingProjectName = existingCabal!.replace(".cabal", "");
    
    return JSON.stringify({
      success: false,
      error: `Project already exists at target location`,
      conflict_type: "direct",
      details: {
        targetDir: resolvedTargetDir,
        existingProject: existingProjectName,
        existingCabalFile: existingCabal,
        intendedProject: name,
      },
      question: existingProjectName === name
        ? `Project '${name}' already exists. What do you want to do?`
        : `Project '${existingProjectName}' exists but you requested '${name}'. What do you want to do?`,
      options: [
        {
          id: "use_existing",
          description: `Use existing project '${existingProjectName}' (recommended if names match)`,
          action: existingProjectName === name
            ? `Switch to it with: ghci_switch_project(project="${name}")`
            : `Check if you meant to use '${existingProjectName}' instead of '${name}'`,
        },
        {
          id: "add_modules",
          description: `Add modules to existing project without overwriting`,
          action: `Use: ghci_scaffold(signatures={...})`,
        },
        {
          id: "replace",
          description: `DANGER: Replace existing project completely`,
          action: `Use: force_in_current: true (will delete existing .cabal and recreate)`,
        },
        {
          id: "new_location",
          description: `Create '${name}' in a different location`,
          action: `Use: target_path: "different/path/${name}"`,
        },
      ],
    });
  }
  
  // Case B: Subdirectory conflicts - target path contains other projects
  if (conflicts.hasSubdirProjects && !force_in_current) {
    return JSON.stringify({
      success: false,
      error: `Target directory contains existing Haskell projects`,
      conflict_type: "subdirectory",
      details: {
        targetDir: resolvedTargetDir,
        intendedProject: name,
        existingProjects: conflicts.subdirProjects,
      },
      question: `Directory '${path.basename(resolvedTargetDir)}' already contains ${conflicts.subdirProjects.length} project(s): ${conflicts.subdirProjects.join(", ")}. What do you want to do?`,
      options: [
        {
          id: "create_alongside",
          description: `Create '${name}' as a sibling project in the same directory`,
          action: `Confirm by re-running with: force_in_current: true`,
          warning: "This will create a multi-project directory structure",
        },
        {
          id: "use_existing",
          description: `Use one of the existing projects instead`,
          action: `Switch with: ghci_switch_project(project="<project-name>")`,
          available: conflicts.subdirProjects,
        },
        {
          id: "new_location",
          description: `Create '${name}' in a clean directory`,
          action: `Use: target_path: "clean/path/${name}"`,
        },
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

  const testSuiteSection = test_suite ? generateTestSuiteSection(name, allDeps, lang) : "";

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
      `Run ghci_switch_project(project="${name}") to switch to it — it auto-scaffolds source files on switch. ` +
      `The project cache has been refreshed automatically. ` +
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
      "3) If name differs → asks for clarification (workspace root vs subdirectory vs custom). " +
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
        'Examples: "my-project" (creates in root), "subfolder/my-lib" (in subdirectory). ' +
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
      
      // Invalidate projects cache so ghci_switch_project finds the new project
      const parsed = JSON.parse(result);
      if (parsed.success && ctx.invalidateProjectsCache) {
        ctx.invalidateProjectsCache();
      }
      
      return { content: [{ type: "text" as const, text: result }] };
    }
  );
}
