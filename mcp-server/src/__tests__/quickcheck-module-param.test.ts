import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import { handleQuickCheck, resetQuickCheckState } from "../tools/quickcheck.js";
import { loadStore, saveProperty } from "../property-store.js";

/**
 * Tests that the `module` parameter on ghci_quickcheck correctly assigns
 * properties to the right module in properties.json.
 */

function createMockSession(qcOutput: string) {
  return {
    execute: vi.fn(async (cmd: string) => {
      if (cmd.includes("import Test.QuickCheck")) {
        return { output: "", success: true };
      }
      if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
        return { output: qcOutput, success: true };
      }
      // let binding for property
      if (cmd.startsWith("let ")) {
        return { output: "", success: true };
      }
      return { output: "", success: true };
    }),
    typeOf: vi.fn(async () => ({ output: "", success: false })),
    loadModules: vi.fn(async () => {}),
    isAlive: () => true,
  } as any;
}

describe("ghci_quickcheck module parameter", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "qc-module-test-"));
    resetQuickCheckState();
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("saves property to explicit module when provided", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheck(
      session,
      {
        property: "\\x -> reverse (reverse x) == (x :: [Int])",
        module: "src/Parser/Run.hs",
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.module).toBe("src/Parser/Run.hs");
  });

  it("falls back to activeModule when module not provided", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");
    const ctx = {
      getWorkflowState: () => ({
        activeModule: "src/Parser/Combinators.hs",
        modules: new Map(),
      }),
      getModuleProgress: () => undefined,
      updateModuleProgress: vi.fn(),
    };

    await handleQuickCheck(
      session,
      { property: "\\x -> x == x" },
      ctx,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.module).toBe("src/Parser/Combinators.hs");
  });

  it("explicit module overrides activeModule", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");
    const ctx = {
      getWorkflowState: () => ({
        activeModule: "src/Parser/Combinators.hs",
        modules: new Map(),
      }),
      getModuleProgress: () => undefined,
      updateModuleProgress: vi.fn(),
    };

    await handleQuickCheck(
      session,
      {
        property: "\\x -> x == x",
        module: "src/Parser/Run.hs",
      },
      ctx,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties[0]!.module).toBe("src/Parser/Run.hs");
  });

  it("does not persist failing properties", async () => {
    const session = createMockSession("*** Failed! Falsifiable (after 3 tests):\n0");

    await handleQuickCheck(
      session,
      {
        property: "\\x -> x > 0",
        module: "src/Foo.hs",
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(0);
  });

  // --- module_path alias (Change 3) ---

  it("module_path works as alias for module", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheck(
      session,
      {
        property: "\\x -> reverse (reverse x) == (x :: [Int])",
        module_path: "src/Parser/Run.hs",
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.module).toBe("src/Parser/Run.hs");
  });

  it("module_path takes precedence over module when both provided", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheck(
      session,
      {
        property: "\\x -> x == x",
        module: "src/Old.hs",
        module_path: "src/New.hs",
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties[0]!.module).toBe("src/New.hs");
  });

  it("module_path alone overrides activeModule", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");
    const ctx = {
      getWorkflowState: () => ({
        activeModule: "src/Active.hs",
        modules: new Map(),
      }),
      getModuleProgress: () => undefined,
      updateModuleProgress: vi.fn(),
    };

    await handleQuickCheck(
      session,
      {
        property: "\\x -> x == x",
        module_path: "src/Explicit.hs",
      },
      ctx,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties[0]!.module).toBe("src/Explicit.hs");
  });
});
