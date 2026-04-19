/**
 * Integration-style unit test covering the full chain:
 *
 *   ghci_quickcheck_batch({ function_name: "myFn" })
 *     ⇒ saveProperty with functionName
 *     ⇒ handleExportTests picks it up as label fallback
 *
 * This is what closes the "property_2/property_3 everywhere" UX gap
 * surfaced during the Arithmetic Expression Evaluator session: batch
 * runs now produce semantic Spec.hs labels.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import { handleQuickCheckBatch, resetQuickCheckState } from "../tools/quickcheck.js";
import { handleExportTests } from "../tools/export-tests.js";

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

describe("batch → save → export : label fallback chain", () => {
  let tmp: string;

  beforeEach(async () => {
    tmp = await mkdtemp(path.join(os.tmpdir(), "qc-chain-"));
    resetQuickCheckState();
  });

  afterEach(async () => {
    await rm(tmp, { recursive: true, force: true });
  });

  it("batch with function_name produces non-positional labels in Spec.hs", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: [
          "\\a b -> (a + b :: Int) == b + a",
          "\\a b c -> ((a + b) + c :: Int) == a + (b + c)",
          "\\a -> (a + 0 :: Int) == a",
        ],
        module_path: "src/Math.hs",
        function_name: "myAdd",
      },
      undefined,
      tmp
    );

    const outputPath = path.join(tmp, "test/Spec.hs");
    await handleExportTests(tmp, { output_path: "test/Spec.hs" });
    const generated = await readFile(outputPath, "utf-8");

    // Should contain myAdd-based labels (disambiguated with _2, _3), not property_N.
    expect(generated).toMatch(/putStr "myAdd(?:|_2|_3):/);
    // NOT the ugly positional fallback
    expect(generated).not.toMatch(/putStr "property_[123]:/);
  });

  it("labels array beats function_name when both provided", async () => {
    const session = createMockSession("+++ OK, passed 100 tests.");

    await handleQuickCheckBatch(
      session,
      {
        properties: [
          "\\a b -> (a + b :: Int) == b + a",
          "\\a b c -> ((a + b) + c :: Int) == a + (b + c)",
        ],
        module_path: "src/Math.hs",
        function_name: "myAdd",
        labels: ["commut", "assoc"],
      },
      undefined,
      tmp
    );

    const outputPath = path.join(tmp, "test/Spec.hs");
    await handleExportTests(tmp, { output_path: "test/Spec.hs" });
    const generated = await readFile(outputPath, "utf-8");

    expect(generated).toMatch(/putStr "commut:/);
    expect(generated).toMatch(/putStr "assoc:/);
    expect(generated).not.toMatch(/putStr "myAdd/);
  });
});
