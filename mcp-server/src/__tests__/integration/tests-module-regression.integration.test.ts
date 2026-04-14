/**
 * Integration tests for the tests_module / regression filter feature.
 * These tests do NOT require GHC — they only test the property-store and
 * quickcheck handler behaviour with mocked GHCi sessions.
 */
import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { mkdtemp, rm } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import { handleQuickCheck, handleQuickCheckBatch, resetQuickCheckState } from "../../tools/quickcheck.js";
import { saveProperty, getModuleProperties, getAllProperties } from "../../property-store.js";

function createMockSession(qcOutput: string) {
  return {
    execute: vi.fn(async (cmd: string) => {
      if (cmd.includes("import Test.QuickCheck")) {
        return { output: "", success: true };
      }
      if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
        return { output: qcOutput, success: true };
      }
      if (cmd.startsWith("let ")) {
        return { output: "", success: true };
      }
      return { output: "", success: true };
    }),
    typeOf: vi.fn(async () => ({ output: "", success: false })),
    loadModules: vi.fn(async () => ({ output: "", success: true })),
    isAlive: () => true,
  } as any;
}

describe("tests_module: quickcheck_batch → regression filter", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "tests-module-int-"));
    resetQuickCheckState();
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("properties tagged with tests_module are filterable by semantic module", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    // Run batch with tests_module pointing to Eval
    await handleQuickCheckBatch(
      session,
      {
        properties: [
          "\\x -> x == x",
          "\\y -> y /= y || y == y",
        ],
        module_path: "src/Syntax.hs",
        tests_module: "src/Eval.hs",
      },
      undefined,
      tmpDir
    );

    // Also save a property without tests_module (legacy)
    await saveProperty(tmpDir, { property: "legacy-prop", module: "src/Other.hs" });

    // getModuleProperties for Eval should return the two tagged properties
    const evalProps = await getModuleProperties(tmpDir, "src/Eval.hs");
    expect(evalProps).toHaveLength(2);

    // getModuleProperties for Syntax should return nothing (load context, not semantic target)
    const syntaxProps = await getModuleProperties(tmpDir, "src/Syntax.hs");
    expect(syntaxProps).toHaveLength(0);

    // getAllProperties returns all 3
    const all = await getAllProperties(tmpDir);
    expect(all).toHaveLength(3);
  });

  it("regression run without module filter re-runs all properties", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: ["\\x -> x == x"],
        module_path: "src/Syntax.hs",
        tests_module: "src/Eval.hs",
      },
      undefined,
      tmpDir
    );

    const all = await getAllProperties(tmpDir);
    expect(all).toHaveLength(1);
    expect(all[0]!.tests_module).toBe("src/Eval.hs");
    expect(all[0]!.module).toBe("src/Syntax.hs");
  });
});

describe("init: test_suite flag creates correct cabal content", () => {
  let tmpDir: string;
  let workspaceRoot: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "init-int-"));
    workspaceRoot = tmpDir;
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("test_suite=true: cabal has test-suite stanza and test/Spec.hs exists", async () => {
    const { handleInit } = await import("../../tools/init.js");
    const { readFile, access } = await import("node:fs/promises");

    const result = JSON.parse(
      await handleInit(tmpDir, tmpDir, workspaceRoot, {
        name: "myproj",
        modules: ["Lib"],
        test_suite: true,
        target_path: "myproj",
      })
    );

    expect(result.success).toBe(true);

    const projectDir = result.projectDir;
    const cabal = await readFile(path.join(projectDir, "myproj.cabal"), "utf-8");
    expect(cabal).toContain("test-suite");
    expect(cabal).toContain("exitcode-stdio-1.0");

    await access(path.join(projectDir, "test", "Spec.hs"));
    const spec = await readFile(path.join(projectDir, "test", "Spec.hs"), "utf-8");
    expect(spec).toContain("module Main");
  });

  it("test_suite=false: no test-suite stanza and no test/Spec.hs", async () => {
    const { handleInit } = await import("../../tools/init.js");
    const { readFile } = await import("node:fs/promises");

    const result = JSON.parse(
      await handleInit(tmpDir, tmpDir, workspaceRoot, {
        name: "cleanproj",
        modules: ["Lib"],
        test_suite: false,
        target_path: "cleanproj",
      })
    );

    const projectDir = result.projectDir;
    const cabal = await readFile(path.join(projectDir, "cleanproj.cabal"), "utf-8");
    expect(cabal).not.toContain("test-suite");

    let found = false;
    try {
      const { access } = await import("node:fs/promises");
      await access(path.join(projectDir, "test", "Spec.hs"));
      found = true;
    } catch { /* expected */ }
    expect(found).toBe(false);
  });

  it("_nextStep clarifies auto-scaffold on switch (no double scaffold)", async () => {
    const { handleInit } = await import("../../tools/init.js");

    const result = JSON.parse(
      await handleInit(tmpDir, tmpDir, workspaceRoot, {
        name: "switchproj",
        modules: ["Lib"],
        target_path: "switchproj",
      })
    );

    expect(result._nextStep).toContain("ghci_switch_project");
    expect(result._nextStep).toContain("auto-scaffolds");
    // Should NOT tell users they need to call ghci_scaffold separately
    expect(result._nextStep).not.toMatch(/then ghci_scaffold\s*\(/);
  });
});
