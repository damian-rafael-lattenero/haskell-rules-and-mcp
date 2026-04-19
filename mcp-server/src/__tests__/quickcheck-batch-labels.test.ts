/**
 * Unit tests: ghci_quickcheck_batch now supports shared `function_name` and
 * per-item `labels` so the exporter can emit semantic names instead of
 * positional fallbacks like `property_2`.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp, rm } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import { handleQuickCheckBatch, resetQuickCheckState } from "../tools/quickcheck.js";
import { loadStore } from "../property-store.js";

function createMockSession(qcOutput: string) {
  return {
    execute: async (cmd: string) => {
      if (cmd.includes("import Test.QuickCheck")) return { output: "", success: true };
      if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
        return { output: qcOutput, success: true };
      }
      return { output: "", success: true };
    },
    typeOf: async () => ({ output: "", success: false }),
    loadModules: async () => {},
    isAlive: () => true,
  } as any;
}

describe("ghci_quickcheck_batch — function_name + labels", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "qc-batch-labels-"));
    resetQuickCheckState();
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("shared function_name is propagated to every saved property", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: [
          "\\a b -> (a + b :: Int) == b + a",
          "\\a b c -> ((a + b) + c :: Int) == a + (b + c)",
        ],
        module_path: "src/Mod.hs",
        function_name: "myAdd",
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(2);
    expect(store.properties.every((p) => p.functionName === "myAdd")).toBe(true);
  });

  it("per-item labels override function_name on the property record", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: [
          "\\a b -> (a + b :: Int) == b + a",
          "\\a b c -> ((a + b) + c :: Int) == a + (b + c)",
        ],
        module_path: "src/Mod.hs",
        function_name: "myAdd",
        labels: ["commutativity", "associativity"],
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties.map((p) => p.label)).toEqual([
      "commutativity",
      "associativity",
    ]);
    // function_name is still attached — labels and function_name coexist.
    expect(store.properties.every((p) => p.functionName === "myAdd")).toBe(true);
  });

  it("short labels array falls back to function_name for missing slots", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: [
          "\\a -> (a :: Int) == a",
          "\\a b -> (a + b :: Int) == b + a",
          "\\a -> (negate (negate a :: Int)) == a",
        ],
        module_path: "src/Mod.hs",
        function_name: "myFunc",
        labels: ["identity"], // only one label
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties[0]!.label).toBe("identity");
    // Items 1 and 2 have no explicit label — they just carry function_name.
    expect(store.properties[1]!.label).toBeUndefined();
    expect(store.properties[2]!.label).toBeUndefined();
    expect(store.properties.every((p) => p.functionName === "myFunc")).toBe(true);
  });

  it("empty-string labels are treated as 'not provided'", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: [
          "\\a -> (a :: Int) == a",
          "\\a b -> (a + b :: Int) == b + a",
        ],
        module_path: "src/Mod.hs",
        function_name: "myFunc",
        labels: ["", "commut"],
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties[0]!.label).toBeUndefined();
    expect(store.properties[1]!.label).toBe("commut");
  });

  it("neither function_name nor labels → still works (backward compat)", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: ["\\a -> (a :: Int) == a"],
        module_path: "src/Mod.hs",
      },
      undefined,
      tmpDir
    );

    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.functionName).toBeUndefined();
    expect(store.properties[0]!.label).toBeUndefined();
  });
});
