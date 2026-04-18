import { mkdir, writeFile, access } from "node:fs/promises";
import path from "node:path";

export interface CreateProjectOptions {
  name: string;
  rootDir: string;
  modules?: string[];
  deps?: string[];
  language?: "GHC2024" | "Haskell2010";
  withTestSuite?: boolean;
}

export interface CreateProjectResult {
  success: true;
  projectDir: string;
  created: string[];
  modules: string[];
}

export interface CreateProjectError {
  success: false;
  error: string;
  hint?: string;
  projectDir?: string;
}

function pascalCase(s: string): string {
  return s
    .split(/[^A-Za-z0-9]+/)
    .filter(Boolean)
    .map((part) => part[0]!.toUpperCase() + part.slice(1))
    .join("");
}

async function pathExists(p: string): Promise<boolean> {
  try {
    await access(p);
    return true;
  } catch {
    return false;
  }
}

export async function handleCreateProject(
  opts: CreateProjectOptions
): Promise<CreateProjectResult | CreateProjectError> {
  const { name, rootDir, deps = [], language = "GHC2024", withTestSuite = true } = opts;
  const modules = opts.modules && opts.modules.length > 0 ? opts.modules : [pascalCase(name)];
  const projectDir = path.resolve(rootDir, name);
  const cabalPath = path.join(projectDir, `${name}.cabal`);

  if (await pathExists(cabalPath)) {
    return {
      success: false,
      error: `A project already exists at ${projectDir} (${name}.cabal present).`,
      hint: "Pick a different name, a different root_dir, or use ghci_add_modules to extend the existing project.",
      projectDir,
    };
  }

  await mkdir(projectDir, { recursive: true });
  await mkdir(path.join(projectDir, "src"), { recursive: true });

  for (const mod of modules) {
    const parts = mod.split(".");
    if (parts.length > 1) {
      await mkdir(path.join(projectDir, "src", ...parts.slice(0, -1)), { recursive: true });
    }
  }

  const allDeps = ["base >= 4.20 && < 5", ...deps.filter((d) => !d.startsWith("base"))];
  if (withTestSuite && !allDeps.some((d) => d.includes("QuickCheck"))) {
    allDeps.push("QuickCheck >= 2.14");
  }

  const libraryDeps = allDeps.map((d) => `    ${d}`).join(",\n");
  const modulesSection = modules.map((m) => `    ${m}`).join("\n");

  const testSuiteDeps = withTestSuite
    ? [
        "base >= 4.20 && < 5",
        name,
        ...allDeps.filter((d) => !d.startsWith("base")),
      ]
    : [];
  const testSuiteSection = withTestSuite
    ? `
test-suite ${name}-test
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  build-depends:
${testSuiteDeps.map((d) => `    ${d}`).join(",\n")}
  default-language: ${language}
  ghc-options:      -Wall
`
    : "";

  const cabalContent = `cabal-version:      2.4
name:               ${name}
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:
${modulesSection}
  build-depends:
${libraryDeps}
  hs-source-dirs:   src
  default-language: ${language}
  ghc-options:      -Wall
${testSuiteSection}`;

  const created: string[] = [];

  await writeFile(cabalPath, cabalContent, "utf-8");
  created.push(`${name}.cabal`);

  await writeFile(path.join(projectDir, "cabal.project"), "packages: .\n", "utf-8");
  created.push("cabal.project");

  for (const mod of modules) {
    const parts = mod.split(".");
    const modPath = path.join(projectDir, "src", ...parts) + ".hs";
    if (!(await pathExists(modPath))) {
      await writeFile(modPath, `module ${mod} where\n`, "utf-8");
      created.push(path.relative(projectDir, modPath));
    }
  }

  if (withTestSuite) {
    const testDir = path.join(projectDir, "test");
    await mkdir(testDir, { recursive: true });
    const specPath = path.join(testDir, "Spec.hs");
    if (!(await pathExists(specPath))) {
      await writeFile(
        specPath,
        `module Main where

main :: IO ()
main = pure ()
`,
        "utf-8"
      );
      created.push("test/Spec.hs");
    }
  }

  return {
    success: true,
    projectDir,
    created,
    modules,
  };
}
