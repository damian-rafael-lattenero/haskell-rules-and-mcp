/**
 * Integration tests for property lifecycle with export workflow.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdir, rm, writeFile, readFile } from "node:fs/promises";
import path from "node:path";
import { handlePropertyLifecycle } from "../../tools/property-lifecycle.js";
import { handleExportTests } from "../../tools/export-tests.js";
import { saveProperty } from "../../property-store.js";

const TEST_PROJECT_DIR = path.resolve(
  import.meta.dirname,
  "../../../test-fixtures/property-lifecycle-integration"
);

beforeEach(async () => {
  await mkdir(TEST_PROJECT_DIR, { recursive: true });
  await mkdir(path.join(TEST_PROJECT_DIR, ".haskell-flows"), { recursive: true });
  await mkdir(path.join(TEST_PROJECT_DIR, "test"), { recursive: true });
  await mkdir(path.join(TEST_PROJECT_DIR, "src"), { recursive: true });

  // Create minimal .cabal file (generic test project)
  const cabalContent = `name: test-project
version: 0.1.0.0
cabal-version: >= 1.10
build-type: Simple

library
  exposed-modules: TestModule
  build-depends: base >= 4.20 && < 5, QuickCheck >= 2.14
  hs-source-dirs: src
  default-language: GHC2024

test-suite test-suite
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  hs-source-dirs: test
  build-depends: base, test-project, QuickCheck
  default-language: GHC2024
`;
  await writeFile(path.join(TEST_PROJECT_DIR, "test-project.cabal"), cabalContent);
  await writeFile(
    path.join(TEST_PROJECT_DIR, "src", "TestModule.hs"),
    "module TestModule where\n\nidentity :: a -> a\nidentity x = x\n"
  );
});

afterEach(async () => {
  await rm(TEST_PROJECT_DIR, { recursive: true, force: true });
});

describe("Property Lifecycle Integration", () => {
  it("should complete full workflow: save -> deprecate -> export (filtered)", async () => {
    // Step 1: Save multiple properties (generic, language-agnostic test data)
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\x -> identity x == x",
      module: "src/TestModule.hs",
      functionName: "identity",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\n -> show (read n) == n",
      module: "src/TestModule.hs",
      functionName: "roundtrip",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\n -> length n >= 0 ==> show (read n) == n",
      module: "src/TestModule.hs",
      functionName: "roundtrip",
    });

    // Step 2: List properties
    const listResult = JSON.parse(
      await handlePropertyLifecycle(TEST_PROJECT_DIR, { action: "list" })
    );
    expect(listResult.count).toBe(3);
    expect(listResult.active).toBe(3);

    // Step 3: Deprecate the old property
    const deprecateResult = JSON.parse(
      await handlePropertyLifecycle(TEST_PROJECT_DIR, {
        action: "deprecate",
        property: "\\n -> show (read n) == n",
        reason: "Replaced with version that handles edge cases",
      })
    );
    expect(deprecateResult.success).toBe(true);

    // Step 4: List again - should show 1 deprecated
    const listResult2 = JSON.parse(
      await handlePropertyLifecycle(TEST_PROJECT_DIR, { action: "list" })
    );
    expect(listResult2.count).toBe(3);
    expect(listResult2.active).toBe(2);
    expect(listResult2.deprecated).toBe(1);

    // Step 5: Export - should only export active properties
    const exportResult = JSON.parse(
      await handleExportTests(TEST_PROJECT_DIR, {
        output_path: "test/Spec.hs",
        validate_test_suite: false,
      })
    );
    expect(exportResult.success).toBe(true);
    expect(exportResult.propertyCount).toBe(2);

    // Step 6: Verify exported file doesn't contain deprecated property
    const exported = await readFile(
      path.join(TEST_PROJECT_DIR, "test", "Spec.hs"),
      "utf-8"
    );
    expect(exported).toContain("identity x == x");
    expect(exported).toContain("length n >= 0 ==>");
    // Check that the old property (without precondition) is not present as a standalone quickCheck call
    const lines = exported.split("\n");
    const hasOldProperty = lines.some(
      (line) =>
        line.includes("quickCheck") &&
        line.includes("show (read n) == n") &&
        !line.includes("length n >= 0 ==>")
    );
    expect(hasOldProperty).toBe(false);
  });

  it("should support replace workflow with linking", async () => {
    // Save old property (generic serialization roundtrip)
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\x -> deserialize (serialize x) == Just x",
      module: "src/TestModule.hs",
    });

    // Replace with normalized version
    const replaceResult = JSON.parse(
      await handlePropertyLifecycle(TEST_PROJECT_DIR, {
        action: "replace",
        property: "\\x -> deserialize (serialize x) == Just x",
        replaced_by: "\\x -> fmap normalize (deserialize (serialize x)) == Just (normalize x)",
        reason: "Added normalization to handle semantic equivalence",
      })
    );
    expect(replaceResult.success).toBe(true);
    expect(replaceResult.old_property).toBe("\\x -> deserialize (serialize x) == Just x");
    expect(replaceResult.new_property).toContain("normalize");

    // Save the new property
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\x -> fmap normalize (deserialize (serialize x)) == Just (normalize x)",
      module: "src/TestModule.hs",
    });

    // Export should only include new property
    const exportResult = JSON.parse(
      await handleExportTests(TEST_PROJECT_DIR, {
        output_path: "test/Spec.hs",
        validate_test_suite: false,
      })
    );
    expect(exportResult.success).toBe(true);
    expect(exportResult.propertyCount).toBe(1);

    const exported = await readFile(
      path.join(TEST_PROJECT_DIR, "test", "Spec.hs"),
      "utf-8"
    );
    expect(exported).toContain("normalize");
    expect(exported).not.toContain("deserialize (serialize x) == Just x");
  });

  it("should handle remove action permanently", async () => {
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\_ -> True",
      module: "src/TestModule.hs",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\x -> x == x",
      module: "src/TestModule.hs",
    });

    // Remove trivial property
    const removeResult = JSON.parse(
      await handlePropertyLifecycle(TEST_PROJECT_DIR, {
        action: "remove",
        property: "\\_ -> True",
      })
    );
    expect(removeResult.success).toBe(true);

    // List should show only 1 property
    const listResult = JSON.parse(
      await handlePropertyLifecycle(TEST_PROJECT_DIR, { action: "list" })
    );
    expect(listResult.count).toBe(1);
    expect(listResult.active).toBe(1);
  });
});
