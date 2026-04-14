import { afterEach, describe, expect, it } from "vitest";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { handleValidateCabal } from "../../tools/validate-cabal.js";

describe("validate-cabal integration", () => {
  const dirs: string[] = [];

  afterEach(async () => {
    await Promise.all(dirs.map((d) => rm(d, { recursive: true, force: true })));
    dirs.length = 0;
  });

  it("reports missing test-suite dependencies required by Spec imports", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "validate-cabal-missing-"));
    dirs.push(dir);

    await writeFile(
      path.join(dir, "demo.cabal"),
      `cabal-version:      2.4
name:               demo
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:  Lib
  build-depends:    base >= 4.20 && < 5, containers
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
    await mkdir(path.join(dir, "test"), { recursive: true });
    await writeFile(path.join(dir, "test", "Spec.hs"), "import qualified Data.Map.Strict as Map\nmain = print Map.empty");

    const result = JSON.parse(await handleValidateCabal(dir));
    expect(result.success).toBe(false);
    expect(result.missingDependencies).toContain("containers");
  });

  it("passes when required dependencies are present in test-suite", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "validate-cabal-ok-"));
    dirs.push(dir);

    await writeFile(
      path.join(dir, "demo.cabal"),
      `cabal-version:      2.4
name:               demo
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:  Lib
  build-depends:    base >= 4.20 && < 5, containers
  hs-source-dirs:   src
  default-language: GHC2024

test-suite demo-test
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  build-depends:
    base >= 4.20 && < 5,
    demo,
    containers,
    QuickCheck >= 2.14
  default-language: GHC2024
`,
      "utf-8"
    );
    await mkdir(path.join(dir, "test"), { recursive: true });
    await writeFile(path.join(dir, "test", "Spec.hs"), "import qualified Data.Map.Strict as Map\nmain = print Map.empty");

    const result = JSON.parse(await handleValidateCabal(dir));
    expect(result.success).toBe(true);
    expect(result.requiredPackages).toContain("containers");
  });

  it("keeps generated export specs compilable with wildcard properties", async () => {
    const dir = await mkdtemp(path.join(os.tmpdir(), "validate-cabal-export-"));
    dirs.push(dir);
    await writeFile(
      path.join(dir, "demo.cabal"),
      `cabal-version:      2.4
name:               demo
version:            0.1.0.0
build-type:         Simple

library
  exposed-modules:  Lib
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
    await mkdir(path.join(dir, "test"), { recursive: true });
    await writeFile(path.join(dir, "test", "Spec.hs"), "import Test.QuickCheck\nmain = quickCheck ((\\_ -> True :: () -> Bool))");

    const result = JSON.parse(await handleValidateCabal(dir));
    expect(result.success).toBe(true);

    const saved = await readFile(path.join(dir, "test", "Spec.hs"), "utf-8");
    expect(saved).toContain(":: () -> Bool");
  });
});
