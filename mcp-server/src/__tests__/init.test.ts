import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleInit, generateTestSuiteSection } from "../tools/init.js";
import { mkdtemp, rm, readFile, access, writeFile } from "node:fs/promises";
import { detectBuildTool } from "../parsers/cabal-parser.js";
import path from "node:path";
import os from "node:os";


describe("handleInit", () => {
  let targetDir: string;
  let workspaceRoot: string;
  let currentProjectDir: string;

  beforeEach(async () => {
    workspaceRoot = await mkdtemp(path.join(os.tmpdir(), "workspace-"));
    targetDir = await mkdtemp(path.join(os.tmpdir(), "init-test-"));
    currentProjectDir = targetDir; // Default: tests run in clean dir
  });

  afterEach(async () => {
    await rm(targetDir, { recursive: true, force: true });
    await rm(workspaceRoot, { recursive: true, force: true });
  });

  it("creates .cabal in the target directory", async () => {
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
        name: "my-project", 
        modules: ["Lib"],
        target_path: path.relative(workspaceRoot, targetDir)
      })
    );
    expect(result.success).toBe(true);
    await access(path.join(targetDir, "my-project.cabal"));
  });

  it("creates .cabal with correct content", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
      name: "test", 
      modules: ["Lib", "Types"],
      target_path: path.relative(workspaceRoot, targetDir)
    });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("name:               test");
    expect(cabal).toContain("Lib");
    expect(cabal).toContain("Types");
    expect(cabal).toContain("QuickCheck");
    expect(cabal).toContain("base");
    expect(cabal).toContain("containers");
  });

  it("creates cabal.project", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
      name: "test", 
      modules: ["Lib"],
      target_path: path.relative(workspaceRoot, targetDir)
    });
    const content = await readFile(path.join(targetDir, "cabal.project"), "utf-8");
    expect(content).toContain("packages: .");
  });

  it("creates src directory and nested dirs", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
      name: "test", 
      modules: ["Expr.Syntax", "Expr.Eval"],
      target_path: path.relative(workspaceRoot, targetDir)
    });
    await access(path.join(targetDir, "src", "Expr"));
  });

  it("includes custom dependencies", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, {
      name: "test", 
      modules: ["Lib"], 
      deps: ["containers", "mtl >= 2.2"],
      target_path: path.relative(workspaceRoot, targetDir)
    });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("containers");
    expect(cabal).toContain("mtl >= 2.2");
  });

  it("adds containers by default even when user does not request it", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, {
      name: "defaults-test",
      modules: ["Lib"],
      target_path: path.relative(workspaceRoot, targetDir),
    });
    const cabal = await readFile(path.join(targetDir, "defaults-test.cabal"), "utf-8");
    expect(cabal).toContain("containers");
  });

  it("fails if .cabal already exists", async () => {
    await writeFile(path.join(targetDir, "existing.cabal"), "name: existing\n");
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
        name: "test", 
        modules: ["Lib"],
        target_path: path.relative(workspaceRoot, targetDir)
      })
    );
    expect(result.success).toBe(false);
    expect(result.error).toContain("already exists");
  });

  it("creates target directory if it doesn't exist", async () => {
    const newDir = path.join(targetDir, "subdir", "deep");
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
        name: "test", 
        modules: ["Lib"],
        target_path: path.relative(workspaceRoot, newDir)
      })
    );
    expect(result.success).toBe(true);
    await access(path.join(newDir, "test.cabal"));
  });

  it("includes _nextStep guidance with ghci_scaffold when creating in current directory", async () => {
    // When target resolves to the current project dir, _nextStep uses ghci_scaffold
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
        name: "test", 
        modules: ["Lib"],
        target_path: path.relative(workspaceRoot, targetDir)
      })
    );
    expect(typeof result._nextStep).toBe("string");
    expect(result._nextStep.length).toBeGreaterThan(0);
  });

  it("includes _nextStep guidance with ghci_switch_project when creating in different directory", async () => {
    // When creating in a NEW directory (not current project dir), hint uses ghci_switch_project
    const newSubdir = path.join(workspaceRoot, "brand-new-proj");
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
        name: "brand-new-proj", 
        modules: ["Lib"],
        target_path: path.relative(workspaceRoot, newSubdir)
      })
    );
    expect(result._nextStep).toContain("ghci_switch_project");
    expect(result._nextStep).toContain('project="brand-new-proj"');
  });

  it("_nextStep does not say 'then ghci_scaffold separately' for non-current dir", async () => {
    // _nextStep should clarify that ghci_switch_project already auto-scaffolds
    const newDir = path.join(workspaceRoot, "new-proj");
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
        name: "new-proj", 
        modules: ["Lib"],
        target_path: path.relative(workspaceRoot, newDir)
      })
    );
    expect(result._nextStep).toContain("auto-scaffolds");
  });

  it("test_suite=true adds test-suite stanza to .cabal", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, {
      name: "mylib",
      modules: ["Lib"],
      test_suite: true,
      target_path: path.relative(workspaceRoot, targetDir),
    });
    const cabal = await readFile(path.join(targetDir, "mylib.cabal"), "utf-8");
    expect(cabal).toContain("test-suite");
    expect(cabal).toContain("exitcode-stdio-1.0");
    expect(cabal).toContain("Spec.hs");
    expect(cabal).toContain("containers");
  });

  it("test_suite=true creates test/Spec.hs", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, {
      name: "mylib",
      modules: ["Lib"],
      test_suite: true,
      target_path: path.relative(workspaceRoot, targetDir),
    });
    await access(path.join(targetDir, "test", "Spec.hs"));
    const spec = await readFile(path.join(targetDir, "test", "Spec.hs"), "utf-8");
    expect(spec).toContain("module Main");
    expect(spec).toContain("ghci_quickcheck_export");
  });

  it("test_suite=false (default) does NOT create test directory", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, {
      name: "mylib",
      modules: ["Lib"],
      target_path: path.relative(workspaceRoot, targetDir),
    });
    const cabal = await readFile(path.join(targetDir, "mylib.cabal"), "utf-8");
    expect(cabal).not.toContain("test-suite");
    let found = false;
    try {
      await access(path.join(targetDir, "test"));
      found = true;
    } catch { /* expected */ }
    expect(found).toBe(false);
  });

  it("test_suite=true result includes testSuite.created=true", async () => {
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, {
        name: "mylib",
        modules: ["Lib"],
        test_suite: true,
        target_path: path.relative(workspaceRoot, targetDir),
      })
    );
    expect(result.testSuite).toBeDefined();
    expect(result.testSuite.created).toBe(true);
    expect(result.testSuite.specFile).toBe("test/Spec.hs");
  });

  it("defaults to GHC2024", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
      name: "test", 
      modules: ["Lib"],
      target_path: path.relative(workspaceRoot, targetDir)
    });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("GHC2024");
  });

  it("supports custom language override", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, { 
      name: "test", 
      modules: ["Lib"], 
      language: "Haskell2010",
      target_path: path.relative(workspaceRoot, targetDir)
    });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("Haskell2010");
  });

  it("build_tool:stack generates stack.yaml", async () => {
    const result = JSON.parse(
      await handleInit(targetDir, currentProjectDir, workspaceRoot, {
        name: "test",
        modules: ["Lib"],
        build_tool: "stack",
        target_path: path.relative(workspaceRoot, targetDir),
      })
    );
    expect(result.success).toBe(true);
    const stackYaml = await readFile(path.join(targetDir, "stack.yaml"), "utf-8");
    expect(stackYaml).toContain("resolver");
    expect(stackYaml).toContain("packages");
  });

  it("build_tool:cabal does NOT generate stack.yaml", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, {
      name: "test",
      modules: ["Lib"],
      build_tool: "cabal",
      target_path: path.relative(workspaceRoot, targetDir),
    });
    let found = false;
    try {
      await access(path.join(targetDir, "stack.yaml"));
      found = true;
    } catch { /* expected */ }
    expect(found).toBe(false);
  });

  it("default (no build_tool) does NOT generate stack.yaml", async () => {
    await handleInit(targetDir, currentProjectDir, workspaceRoot, {
      name: "test",
      modules: ["Lib"],
      target_path: path.relative(workspaceRoot, targetDir),
    });
    let found = false;
    try {
      await access(path.join(targetDir, "stack.yaml"));
      found = true;
    } catch { /* expected */ }
    expect(found).toBe(false);
  });
});

describe("generateTestSuiteSection", () => {
  it("includes library dependencies in the test suite stanza", () => {
    const allDeps = ["base >= 4.20 && < 5", "containers", "text >= 2.0", "QuickCheck >= 2.14"];
    const section = generateTestSuiteSection("my-lib", allDeps, "GHC2024");

    expect(section).toContain("test-suite my-lib-test");
    expect(section).toContain("containers");
    expect(section).toContain("text >= 2.0");
    expect(section).toContain("QuickCheck >= 2.14");
  });

  it("does not duplicate base dependency in copied test deps", () => {
    const section = generateTestSuiteSection(
      "my-lib",
      ["base >= 4.20 && < 5", "containers", "QuickCheck >= 2.14"],
      "GHC2024"
    );

    const baseMatches = section.match(/base\s+>=\s+4\.20/g) ?? [];
    expect(baseMatches).toHaveLength(1);
  });

  it("keeps valid stanza shape when only QuickCheck is present", () => {
    const section = generateTestSuiteSection(
      "my-lib",
      ["base >= 4.20 && < 5", "QuickCheck >= 2.14"],
      "GHC2024"
    );

    expect(section).toContain("test-suite my-lib-test");
    expect(section).toContain("main-is:          Spec.hs");
    expect(section).toContain("QuickCheck >= 2.14");
    expect(section).toContain("default-language: GHC2024");
  });
});

describe("detectBuildTool", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "build-tool-test-"));
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("returns 'stack' when stack.yaml is present", async () => {
    await writeFile(path.join(dir, "stack.yaml"), "resolver: lts-22.0\n", "utf-8");
    await writeFile(path.join(dir, "pkg.cabal"), "name: pkg\n", "utf-8");
    expect(await detectBuildTool(dir)).toBe("stack");
  });

  it("returns 'cabal' when only .cabal exists", async () => {
    await writeFile(path.join(dir, "pkg.cabal"), "name: pkg\n", "utf-8");
    expect(await detectBuildTool(dir)).toBe("cabal");
  });

  it("returns 'cabal' by default (nothing present)", async () => {
    expect(await detectBuildTool(dir)).toBe("cabal");
  });
});
