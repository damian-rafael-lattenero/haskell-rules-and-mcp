import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleInit } from "../tools/init.js";
import { mkdtemp, rm, readFile, access, writeFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("handleInit", () => {
  let targetDir: string;

  beforeEach(async () => {
    targetDir = await mkdtemp(path.join(os.tmpdir(), "init-test-"));
  });

  afterEach(async () => {
    await rm(targetDir, { recursive: true, force: true });
  });

  it("creates .cabal in the target directory", async () => {
    const result = JSON.parse(
      await handleInit(targetDir, { name: "my-project", modules: ["Lib"] })
    );
    expect(result.success).toBe(true);
    expect(result.projectDir).toBe(targetDir);
    await access(path.join(targetDir, "my-project.cabal"));
  });

  it("creates .cabal with correct content", async () => {
    await handleInit(targetDir, { name: "test", modules: ["Lib", "Types"] });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("name:               test");
    expect(cabal).toContain("Lib");
    expect(cabal).toContain("Types");
    expect(cabal).toContain("QuickCheck");
    expect(cabal).toContain("base");
  });

  it("creates cabal.project", async () => {
    await handleInit(targetDir, { name: "test", modules: ["Lib"] });
    const content = await readFile(path.join(targetDir, "cabal.project"), "utf-8");
    expect(content).toContain("packages: .");
  });

  it("creates src directory and nested dirs", async () => {
    await handleInit(targetDir, { name: "test", modules: ["Expr.Syntax", "Expr.Eval"] });
    await access(path.join(targetDir, "src", "Expr"));
  });

  it("includes custom dependencies", async () => {
    await handleInit(targetDir, {
      name: "test", modules: ["Lib"], deps: ["containers", "mtl >= 2.2"],
    });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("containers");
    expect(cabal).toContain("mtl >= 2.2");
  });

  it("fails if .cabal already exists", async () => {
    await writeFile(path.join(targetDir, "existing.cabal"), "name: existing\n");
    const result = JSON.parse(
      await handleInit(targetDir, { name: "test", modules: ["Lib"] })
    );
    expect(result.success).toBe(false);
    expect(result.error).toContain("already exists");
  });

  it("creates target directory if it doesn't exist", async () => {
    const newDir = path.join(targetDir, "subdir", "deep");
    const result = JSON.parse(
      await handleInit(newDir, { name: "test", modules: ["Lib"] })
    );
    expect(result.success).toBe(true);
    await access(path.join(newDir, "test.cabal"));
  });

  it("includes _nextStep guidance", async () => {
    const result = JSON.parse(
      await handleInit(targetDir, { name: "test", modules: ["Lib"] })
    );
    expect(result._nextStep).toContain("ghci_scaffold");
  });

  it("defaults to GHC2024", async () => {
    await handleInit(targetDir, { name: "test", modules: ["Lib"] });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("GHC2024");
  });

  it("supports custom language override", async () => {
    await handleInit(targetDir, { name: "test", modules: ["Lib"], language: "Haskell2010" });
    const cabal = await readFile(path.join(targetDir, "test.cabal"), "utf-8");
    expect(cabal).toContain("Haskell2010");
  });
});
