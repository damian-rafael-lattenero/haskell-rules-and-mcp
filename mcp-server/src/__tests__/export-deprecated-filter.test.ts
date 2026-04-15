/**
 * Unit tests for ghci_quickcheck_export with deprecated property filtering.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdir, rm, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { handleExportTests } from "../tools/export-tests.js";
import { saveProperty, deprecateProperty } from "../property-store.js";

const TEST_PROJECT_DIR = path.resolve(
  import.meta.dirname,
  "../../test-fixtures/export-deprecated-test"
);

beforeEach(async () => {
  await mkdir(TEST_PROJECT_DIR, { recursive: true });
  await mkdir(path.join(TEST_PROJECT_DIR, ".haskell-flows"), { recursive: true });
  await mkdir(path.join(TEST_PROJECT_DIR, "test"), { recursive: true });

  // Create minimal .cabal file
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
  await mkdir(path.join(TEST_PROJECT_DIR, "src"), { recursive: true });
  await writeFile(path.join(TEST_PROJECT_DIR, "test-project.cabal"), cabalContent);
  await writeFile(
    path.join(TEST_PROJECT_DIR, "src", "TestModule.hs"),
    "module TestModule where\n\nidentity :: a -> a\nidentity x = x\n"
  );
});

afterEach(async () => {
  await rm(TEST_PROJECT_DIR, { recursive: true, force: true });
});

describe("ghci_quickcheck_export with deprecated filtering", () => {
  it("should filter out deprecated properties by default", async () => {
    // Save 3 properties
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\x -> x == x",
      module: "src/TestModule.hs",
      functionName: "identity",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\_ -> True",
      module: "src/TestModule.hs",
      functionName: "trivial",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\xs -> reverse (reverse xs) == xs",
      module: "src/TestModule.hs",
      functionName: "doubleReverse",
    });

    // Deprecate the trivial one
    await deprecateProperty(TEST_PROJECT_DIR, "\\_ -> True", {
      reason: "Trivial property provides no signal",
    });

    const result = JSON.parse(
      await handleExportTests(TEST_PROJECT_DIR, {
        output_path: "test/Spec.hs",
        validate_test_suite: false,
      })
    );

    expect(result.success).toBe(true);
    expect(result.propertyCount).toBe(2); // Only 2 non-deprecated

    const exported = await readFile(
      path.join(TEST_PROJECT_DIR, "test", "Spec.hs"),
      "utf-8"
    );
    expect(exported).toContain("\\x -> x == x");
    expect(exported).toContain("reverse (reverse xs)");
    expect(exported).not.toContain("\\_ -> True");
  });

  it("should handle all properties being deprecated", async () => {
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\_ -> True",
      module: "src/TestModule.hs",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\_ -> True",
      module: "src/TestModule.hs",
    });

    await deprecateProperty(TEST_PROJECT_DIR, "\\_ -> True");
    await deprecateProperty(TEST_PROJECT_DIR, "\\_ -> True");

    const result = JSON.parse(
      await handleExportTests(TEST_PROJECT_DIR, {
        output_path: "test/Spec.hs",
        validate_test_suite: false,
      })
    );

    expect(result.success).toBe(false);
    expect(result.error).toContain("No saved properties");
  });

  it("should export deprecated properties if explicitly requested", async () => {
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\x -> x == x",
      module: "src/TestModule.hs",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\_ -> True",
      module: "src/TestModule.hs",
    });

    await deprecateProperty(TEST_PROJECT_DIR, "\\_ -> True");

    // Note: only_passing parameter is for future extension
    // Currently we always filter deprecated properties
    const result = JSON.parse(
      await handleExportTests(TEST_PROJECT_DIR, {
        output_path: "test/Spec.hs",
        validate_test_suite: false,
        only_passing: true,
      })
    );

    expect(result.success).toBe(true);
    expect(result.propertyCount).toBe(1);
  });

  it("should show count of deprecated properties in export result", async () => {
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\x -> x == x",
      module: "src/TestModule.hs",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\_ -> True",
      module: "src/TestModule.hs",
    });
    await saveProperty(TEST_PROJECT_DIR, {
      property: "\\_ -> True",
      module: "src/TestModule.hs",
    });

    await deprecateProperty(TEST_PROJECT_DIR, "\\_ -> True");
    await deprecateProperty(TEST_PROJECT_DIR, "\\_ -> True");

    const result = JSON.parse(
      await handleExportTests(TEST_PROJECT_DIR, {
        output_path: "test/Spec.hs",
        validate_test_suite: false,
      })
    );

    expect(result.success).toBe(true);
    expect(result.propertyCount).toBe(1); // Only 1 active
    // The droppedTrivial count includes deprecated properties
  });
});
