import { afterEach, describe, expect, it } from "vitest";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { saveProperty } from "../../property-store.js";
import { handleExportTests } from "../../tools/export-tests.js";

describe("export wildcards integration", () => {
  const dirs: string[] = [];

  afterEach(async () => {
    await Promise.all(dirs.map((dir) => rm(dir, { recursive: true, force: true })));
    dirs.length = 0;
  });

  it("exports wildcard properties with concrete type annotation", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "export-wildcard-"));
    dirs.push(dir);
    await writeFile(
      path.join(dir, "demo.cabal"),
      `cabal-version:      2.4
name:               demo
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:  TestLib
  build-depends:    base >= 4.20 && < 5
  hs-source-dirs:   src
  default-language: GHC2024

test-suite demo-test
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  build-depends:
    base >= 4.20 && < 5,
    demo,
    QuickCheck >= 2.14
  default-language: GHC2024
`,
      "utf-8"
    );

    await saveProperty(dir, {
      property: "\\_ -> (1 :: Int) == 1",
      module: "src/TestLib.hs",
      functionName: "wildcardConstant",
    });

    const result = JSON.parse(await handleExportTests(dir, { validate_test_suite: false }));
    expect(result.success).toBe(true);

    const spec = await readFile(path.join(dir, "test", "Spec.hs"), "utf-8");
    expect(spec).toContain("quickCheck ((\\_ -> (1 :: Int) == 1 :: () -> Bool))");
  });
});
