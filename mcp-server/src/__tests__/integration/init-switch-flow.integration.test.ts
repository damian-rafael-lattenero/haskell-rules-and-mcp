import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleInit } from "../../tools/init.js";
import { discoverProjects } from "../../project-manager.js";
import { mkdir, writeFile, rm } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const testRoot = path.join(__dirname, "..", "fixtures", "init-switch-test");

describe("ghci_init → ghci_switch_project flow", () => {
  beforeEach(async () => {
    await rm(testRoot, { recursive: true, force: true });
    await mkdir(testRoot, { recursive: true });
  });

  afterEach(async () => {
    await rm(testRoot, { recursive: true, force: true });
  });

  it("should create project in subdirectory and be discoverable", async () => {
    // Simulate ghci_init with target_path="playground/expr-eval"
    const result = await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "expr-eval",
        modules: ["Expr.Syntax", "Expr.Eval", "Expr.Simplify", "Expr.Pretty"],
        deps: ["containers", "QuickCheck"],
        target_path: "playground/expr-eval",
      }
    );

    const parsed = JSON.parse(result);
    expect(parsed.success).toBe(true);
    expect(parsed.projectDir).toContain("playground/expr-eval");
    expect(parsed.modules).toEqual(["Expr.Syntax", "Expr.Eval", "Expr.Simplify", "Expr.Pretty"]);

    // Verify project is discoverable
    const projects = await discoverProjects(testRoot, 3);
    expect(projects).toHaveLength(1);
    expect(projects[0]!.name).toBe("expr-eval");
    expect(projects[0]!.path).toContain("playground");
  });

  it("should discover multiple projects in different subdirectories", async () => {
    // Create project 1 in playground/
    await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "project-a",
        modules: ["Lib"],
        target_path: "playground/project-a",
      }
    );

    // Create project 2 in experiments/
    await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "project-b",
        modules: ["Lib"],
        target_path: "experiments/project-b",
      }
    );

    // Create project 3 in root
    await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "project-c",
        modules: ["Lib"],
        target_path: "project-c",
      }
    );

    // All should be discoverable
    const projects = await discoverProjects(testRoot, 3);
    expect(projects).toHaveLength(3);
    expect(projects.map(p => p.name).sort()).toEqual(["project-a", "project-b", "project-c"]);
  });

  it("should handle deeply nested project paths", async () => {
    // Create project at deep path
    const result = await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "deep-project",
        modules: ["Lib"],
        target_path: "level1/level2/deep-project",
      }
    );

    const parsed = JSON.parse(result);
    expect(parsed.success).toBe(true);

    // Should be discoverable with maxDepth=3
    const projects = await discoverProjects(testRoot, 3);
    expect(projects).toHaveLength(1);
    expect(projects[0]!.name).toBe("deep-project");
  });

  it("should not discover project beyond maxDepth", async () => {
    // Create project at very deep path
    await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "too-deep",
        modules: ["Lib"],
        target_path: "a/b/c/d/too-deep",
      }
    );

    // Should NOT be discoverable with maxDepth=3
    const projects = await discoverProjects(testRoot, 3);
    expect(projects).toHaveLength(0);

    // But should be discoverable with maxDepth=5
    const projectsDeep = await discoverProjects(testRoot, 5);
    expect(projectsDeep).toHaveLength(1);
  });

  it("should create test suite when requested", async () => {
    const result = await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "test-project",
        modules: ["Lib"],
        target_path: "test-project",
        test_suite: true,
      }
    );

    const parsed = JSON.parse(result);
    expect(parsed.success).toBe(true);
    expect(parsed.testSuite).toBeDefined();
    expect(parsed.testSuite.created).toBe(true);
    expect(parsed.testSuite.specFile).toBe("test/Spec.hs");

    // Verify project structure
    const projects = await discoverProjects(testRoot, 3);
    expect(projects).toHaveLength(1);
  });

  it("should prevent ambiguous project creation", async () => {
    // Create initial project
    await handleInit(
      testRoot,
      testRoot,
      testRoot,
      {
        name: "existing-project",
        modules: ["Lib"],
        target_path: "existing-project",
      }
    );

    // Try to create different project without target_path while "in" existing project
    const currentProjectDir = path.join(testRoot, "existing-project");
    const result = await handleInit(
      testRoot,
      currentProjectDir,
      testRoot,
      {
        name: "new-project",
        modules: ["Lib"],
        // No target_path - should ask for clarification
      }
    );

    const parsed = JSON.parse(result);
    expect(parsed.success).toBe(false);
    expect(parsed.error).toContain("Ambiguous intent");
    expect(parsed.options).toBeDefined();
    expect(parsed.options.length).toBeGreaterThan(0);
  });
});
