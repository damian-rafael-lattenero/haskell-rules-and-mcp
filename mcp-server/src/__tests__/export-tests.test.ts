import { describe, it, expect, beforeEach, afterEach } from "vitest";
import {
  handleExportTests,
  isTrivialProperty,
  detectQualifiedImports,
  normalizePropertyForExport,
} from "../tools/export-tests.js";
import { saveProperty } from "../property-store.js";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("handleExportTests", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "export-test-"));
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("returns error when no properties saved", async () => {
    const result = JSON.parse(await handleExportTests(tmpDir, {}));
    expect(result.success).toBe(false);
    expect(result.error).toContain("No saved properties");
  });

  it("generates test file from saved properties", async () => {
    await saveProperty(tmpDir, { property: "\\x -> x == x", module: "src/Lib.hs" });
    await saveProperty(tmpDir, { property: "\\x -> reverse (reverse x) == x", module: "src/Lib.hs" });

    const result = JSON.parse(await handleExportTests(tmpDir, { validate_test_suite: false }));
    expect(result.success).toBe(true);
    expect(result.propertyCount).toBe(2);

    const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
    expect(content).toContain("import Test.QuickCheck");
    expect(content).toContain("quickCheck");
    expect(content).toContain("x == x");
    expect(content).toContain("reverse");
  });

  it("uses custom output path", async () => {
    await saveProperty(tmpDir, { property: "True", module: "src/Lib.hs" });
    const result = JSON.parse(
      await handleExportTests(tmpDir, { output_path: "test/Props.hs", validate_test_suite: false })
    );
    expect(result.success).toBe(true);
    expect(result.outputPath).toBe("test/Props.hs");
    await readFile(path.join(tmpDir, "test/Props.hs"), "utf-8"); // should not throw
  });

  it("filters by module", async () => {
    await saveProperty(tmpDir, { property: "p1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "p2", module: "src/B.hs" });

    const result = JSON.parse(
      await handleExportTests(tmpDir, { module: "src/A.hs", validate_test_suite: false })
    );
    expect(result.propertyCount).toBe(1);
  });

  it("includes module imports from properties", async () => {
    await saveProperty(tmpDir, { property: "True", module: "src/Expr/Eval.hs" });
    const result = JSON.parse(await handleExportTests(tmpDir, { validate_test_suite: false }));
    const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
    expect(content).toContain("import Expr.Eval");
  });

  it("includes law/function labels in output", async () => {
    await saveProperty(tmpDir, {
      property: "\\x -> x == x",
      module: "src/Lib.hs",
      law: "reflexivity",
    });
    const result = JSON.parse(await handleExportTests(tmpDir, { validate_test_suite: false }));
    const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
    expect(content).toContain("reflexivity");
  });

  it("includes _nextStep guidance", async () => {
    await saveProperty(tmpDir, { property: "\\x -> x == x", module: "src/Lib.hs" });
    const result = JSON.parse(await handleExportTests(tmpDir, { validate_test_suite: false }));
    expect(result._nextStep).toContain("cabal_test");
  });

  describe("normalizePropertyForExport", () => {
    it("adds annotation for single wildcard lambda", () => {
      expect(normalizePropertyForExport("\\_ -> True")).toBe("(\\_ -> True :: () -> Bool)");
    });

    it("adds annotation for multi-argument lambda with wildcard", () => {
      expect(normalizePropertyForExport("\\x _ -> x == x")).toBe(
        "(\\x _ -> x == x :: () -> Bool)"
      );
      expect(normalizePropertyForExport("\\_ y -> y > 0")).toBe(
        "(\\_ y -> y > 0 :: () -> Bool)"
      );
      expect(normalizePropertyForExport("\\_ _ -> True")).toBe(
        "(\\_ _ -> True :: () -> Bool)"
      );
    });

    it("does not mutate normal property lambdas", () => {
      expect(normalizePropertyForExport("\\x -> x == x")).toBe("\\x -> x == x");
      expect(normalizePropertyForExport("\\x y -> x + y == y + x")).toBe(
        "\\x y -> x + y == y + x"
      );
    });

    it("does not mutate point-free properties", () => {
      expect(normalizePropertyForExport("const True")).toBe("const True");
    });
  });

  // ─── Bug Fix 8a: trivial property filter ────────────────────────────────────

  describe("isTrivialProperty", () => {
    it("flags '\\x -> True' as trivial", () => {
      expect(isTrivialProperty("\\x -> True")).toBe(true);
    });
    it("flags '\\_ -> True' as trivial", () => {
      expect(isTrivialProperty("\\_ -> True")).toBe(true);
    });
    it("flags '\\x y -> True' as trivial (multiple binders)", () => {
      expect(isTrivialProperty("\\x y -> True")).toBe(true);
    });
    it("flags 'const True' as trivial", () => {
      expect(isTrivialProperty("const True")).toBe(true);
    });
    it("does NOT flag '\\x -> x == x' as trivial", () => {
      expect(isTrivialProperty("\\x -> x == x")).toBe(false);
    });
    it("does NOT flag '\\xs -> reverse (reverse xs) == xs' as trivial", () => {
      expect(isTrivialProperty("\\xs -> reverse (reverse xs) == xs")).toBe(false);
    });
    it("does NOT flag '\\n -> n >= 0' as trivial", () => {
      expect(isTrivialProperty("\\n -> n >= 0")).toBe(false);
    });
  });

  describe("handleExportTests — trivial filtering", () => {
    it("drops trivial properties and reports count", async () => {
      await saveProperty(tmpDir, { property: "\\x -> True", module: "src/Lib.hs" });
      await saveProperty(tmpDir, { property: "\\x -> x == x", module: "src/Lib.hs" });

      const result = JSON.parse(await handleExportTests(tmpDir, { validate_test_suite: false }));
      expect(result.success).toBe(true);
      expect(result.propertyCount).toBe(1); // only the non-trivial one
      expect(result.droppedTrivial).toBe(1);
    });

    it("returns error when ALL properties are trivial", async () => {
      await saveProperty(tmpDir, { property: "\\x -> True", module: "src/Lib.hs" });
      await saveProperty(tmpDir, { property: "const True", module: "src/Lib.hs" });

      const result = JSON.parse(await handleExportTests(tmpDir, { validate_test_suite: false }));
      expect(result.success).toBe(false);
      expect(result.error).toContain("trivially true");
      expect(result.droppedTrivial).toBe(2);
    });

    it("comment in generated file notes dropped trivial properties", async () => {
      await saveProperty(tmpDir, { property: "\\_ -> True", module: "src/Lib.hs" });
      await saveProperty(tmpDir, { property: "\\n -> n + 1 > n", module: "src/Lib.hs" });

      await handleExportTests(tmpDir, { validate_test_suite: false });
      const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
      expect(content).toContain("trivial propert");
    });
  });

  // ─── Bug Fix 8b: qualified imports in generated Spec.hs ─────────────────────

  describe("detectQualifiedImports", () => {
    it("adds Map import when properties use Map.*", () => {
      const imports = detectQualifiedImports(["\\n -> eval Map.empty (Lit n) == Right n"]);
      expect(imports).toContain("import qualified Data.Map.Strict as Map");
    });

    it("adds Set import when properties use Set.*", () => {
      const imports = detectQualifiedImports(["\\xs -> Set.fromList xs == Set.fromList xs"]);
      expect(imports).toContain("import qualified Data.Set as Set");
    });

    it("adds multiple imports when needed", () => {
      const imports = detectQualifiedImports([
        "\\n -> eval Map.empty (Lit n) == Right n",
        "\\xs -> Set.size xs >= 0",
      ]);
      expect(imports).toContain("import qualified Data.Map.Strict as Map");
      expect(imports).toContain("import qualified Data.Set as Set");
    });

    it("returns empty array when no qualified modules are referenced", () => {
      const imports = detectQualifiedImports(["\\x -> x == x", "\\n -> n + 1 > n"]);
      expect(imports).toHaveLength(0);
    });

    it("does not duplicate imports for the same module", () => {
      const imports = detectQualifiedImports([
        "\\n -> eval Map.empty (Lit n) == Right n",
        "\\m -> eval Map.empty (Var m) == Left (UnboundVar m)",
      ]);
      const mapImports = imports.filter((i) => i.includes("Data.Map"));
      expect(mapImports).toHaveLength(1);
    });
  });

  describe("handleExportTests — Map import in generated file", () => {
    it("adds 'import qualified Data.Map.Strict as Map' when properties use Map.*", async () => {
      await saveProperty(tmpDir, {
        property: "\\n -> eval Map.empty (Lit n) == Right n",
        module: "src/Expr/Eval.hs",
      });

      await handleExportTests(tmpDir, { validate_test_suite: false });
      const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
      expect(content).toContain("import qualified Data.Map.Strict as Map");
    });

    it("does NOT add Map import when properties don't use Map.*", async () => {
      await saveProperty(tmpDir, {
        property: "\\x -> x == x",
        module: "src/Lib.hs",
      });

      await handleExportTests(tmpDir, { validate_test_suite: false });
      const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
      expect(content).not.toContain("import qualified Data.Map");
    });

    it("normalizes wildcard properties so generated test file compiles", async () => {
      await saveProperty(tmpDir, {
        property: "\\_ -> (1 :: Int) == 1",
        module: "src/Lib.hs",
      });

      await handleExportTests(tmpDir, { validate_test_suite: false });
      const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
      expect(content).toContain("quickCheck ((\\_ -> (1 :: Int) == 1 :: () -> Bool))");
    });
  });
});
