import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleExportTests } from "../tools/export-tests.js";
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

    const result = JSON.parse(await handleExportTests(tmpDir, {}));
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
      await handleExportTests(tmpDir, { output_path: "test/Props.hs" })
    );
    expect(result.success).toBe(true);
    expect(result.outputPath).toBe("test/Props.hs");
    await readFile(path.join(tmpDir, "test/Props.hs"), "utf-8"); // should not throw
  });

  it("filters by module", async () => {
    await saveProperty(tmpDir, { property: "p1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "p2", module: "src/B.hs" });

    const result = JSON.parse(
      await handleExportTests(tmpDir, { module: "src/A.hs" })
    );
    expect(result.propertyCount).toBe(1);
  });

  it("includes module imports from properties", async () => {
    await saveProperty(tmpDir, { property: "True", module: "src/Expr/Eval.hs" });
    const result = JSON.parse(await handleExportTests(tmpDir, {}));
    const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
    expect(content).toContain("import Expr.Eval");
  });

  it("includes law/function labels in output", async () => {
    await saveProperty(tmpDir, {
      property: "\\x -> x == x",
      module: "src/Lib.hs",
      law: "reflexivity",
    });
    const result = JSON.parse(await handleExportTests(tmpDir, {}));
    const content = await readFile(path.join(tmpDir, "test/Spec.hs"), "utf-8");
    expect(content).toContain("reflexivity");
  });

  it("includes _nextStep guidance", async () => {
    await saveProperty(tmpDir, { property: "True", module: "src/Lib.hs" });
    const result = JSON.parse(await handleExportTests(tmpDir, {}));
    expect(result._nextStep).toContain("cabal test");
  });
});
