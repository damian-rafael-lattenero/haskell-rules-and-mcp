import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleInit } from "../tools/init.js";
import { mkdtemp, rm, readFile, access } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("handleInit", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "init-test-"));
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("creates .cabal file with correct structure", async () => {
    const result = JSON.parse(
      await handleInit(tmpDir, {
        name: "my-project",
        modules: ["Lib", "Types"],
      })
    );
    expect(result.success).toBe(true);
    const cabal = await readFile(path.join(tmpDir, "my-project.cabal"), "utf-8");
    expect(cabal).toContain("name:               my-project");
    expect(cabal).toContain("Lib");
    expect(cabal).toContain("Types");
    expect(cabal).toContain("QuickCheck");
    expect(cabal).toContain("base");
  });

  it("creates cabal.project", async () => {
    await handleInit(tmpDir, { name: "test", modules: ["Lib"] });
    const content = await readFile(path.join(tmpDir, "cabal.project"), "utf-8");
    expect(content).toContain("packages: .");
  });

  it("creates src directory", async () => {
    await handleInit(tmpDir, { name: "test", modules: ["Lib"] });
    await access(path.join(tmpDir, "src")); // should not throw
  });

  it("creates nested directories for dotted modules", async () => {
    await handleInit(tmpDir, { name: "test", modules: ["Expr.Syntax", "Expr.Eval"] });
    await access(path.join(tmpDir, "src", "Expr")); // should not throw
  });

  it("includes custom dependencies", async () => {
    const result = JSON.parse(
      await handleInit(tmpDir, {
        name: "test",
        modules: ["Lib"],
        deps: ["containers", "mtl >= 2.2"],
      })
    );
    const cabal = await readFile(path.join(tmpDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("containers");
    expect(cabal).toContain("mtl >= 2.2");
  });

  it("defaults to Haskell2010 language", async () => {
    await handleInit(tmpDir, { name: "test", modules: ["Lib"] });
    const cabal = await readFile(path.join(tmpDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("Haskell2010");
  });

  it("supports custom language", async () => {
    await handleInit(tmpDir, { name: "test", modules: ["Lib"], language: "GHC2024" });
    const cabal = await readFile(path.join(tmpDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("GHC2024");
  });

  it("fails if .cabal already exists", async () => {
    await handleInit(tmpDir, { name: "test", modules: ["Lib"] });
    const result = JSON.parse(
      await handleInit(tmpDir, { name: "test", modules: ["Lib"] })
    );
    expect(result.success).toBe(false);
    expect(result.error).toContain("already exists");
  });

  it("includes _nextStep guidance", async () => {
    const result = JSON.parse(
      await handleInit(tmpDir, { name: "test", modules: ["Lib"] })
    );
    expect(result._nextStep).toContain("ghci_scaffold");
  });
});
