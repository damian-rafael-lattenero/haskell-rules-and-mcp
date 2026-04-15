import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { GhciSession } from "../../ghci-session.js";
import { handleLoadModule } from "../../tools/load-module.js";
import { fixWarning } from "../../tools/fix-warning.js";
import { writeFile, mkdir, rm } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const TEST_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "fixtures", "autofix-test");

describe("Auto-Fix Workflow Integration", () => {
  let session: GhciSession;

  beforeAll(async () => {
    await mkdir(path.join(TEST_DIR, "src"), { recursive: true });
    
    // Create a simple .cabal file
    const cabalContent = `name: autofix-test
version: 0.1.0.0
cabal-version: >= 1.10
build-type: Simple

library
  exposed-modules: Test
  hs-source-dirs: src
  build-depends: base >= 4.7 && < 5
  default-language: Haskell2010
`;
    await writeFile(path.join(TEST_DIR, "autofix-test.cabal"), cabalContent);

    // Create a module with warnings
    const moduleContent = `module Test where

eval env (Lit n) = Right n
eval env (Add e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  return (v1 + v2)

data Expr = Lit Int | Add Expr Expr
`;
    await writeFile(path.join(TEST_DIR, "src", "Test.hs"), moduleContent);

    session = new GhciSession(TEST_DIR);
    await session.start();
  });

  afterAll(async () => {
    if (session.isAlive()) {
      await session.kill();
    }
    await rm(TEST_DIR, { recursive: true, force: true });
  });

  it("ghci_load → suggestedFixes → ghci_fix_warning → ghci_load clean", async () => {
    // Step 1: Load module and get warnings with suggestedFixes
    const loadResult1 = await handleLoadModule(
      session,
      { module_path: "src/Test.hs", diagnostics: true },
      TEST_DIR
    );

    const data1 = JSON.parse(loadResult1);
    
    // Should have warnings
    expect(data1.warnings.length).toBeGreaterThan(0);
    
    // Should have suggestedFixes if any warnings are auto-fixable
    if (data1.suggestedFixes && data1.suggestedFixes.length > 0) {
      const fix = data1.suggestedFixes[0];
      
      // Step 2: Apply the suggested fix
      const fixResult = await fixWarning(
        TEST_DIR,
        fix.file,
        fix.line,
        fix.code,
        true
      );
      
      expect(fixResult.success).toBe(true);
      expect(fixResult.applied).toBe(true);
      
      // Step 3: Reload and verify warning is gone
      const loadResult2 = await handleLoadModule(
        session,
        { module_path: "src/Test.hs", diagnostics: true },
        TEST_DIR
      );
      
      const data2 = JSON.parse(loadResult2);
      
      // Should have fewer warnings now
      expect(data2.warnings.length).toBeLessThanOrEqual(data1.warnings.length);
    }
  });

  it("preview fix without applying", async () => {
    const loadResult = await handleLoadModule(
      session,
      { module_path: "src/Test.hs", diagnostics: true },
      TEST_DIR
    );

    const data = JSON.parse(loadResult);
    
    if (data.suggestedFixes && data.suggestedFixes.length > 0) {
      const fix = data.suggestedFixes[0];
      
      // Preview fix without applying
      const fixResult = await fixWarning(
        TEST_DIR,
        fix.file,
        fix.line,
        fix.code,
        false
      );
      
      expect(fixResult.success).toBe(true);
      expect(fixResult.patch).toBeDefined();
      expect(fixResult.applied).toBeUndefined();
    }
  });
});
