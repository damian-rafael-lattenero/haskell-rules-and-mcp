import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleScaffold } from "../tools/scaffold.js";
import { mkdtemp, rm, mkdir, writeFile, readFile, access } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("handleScaffold", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "scaffold-test-"));
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  async function setupProject(cabalContent: string, existingModules: string[] = []) {
    await writeFile(path.join(tmpDir, "test.cabal"), cabalContent);
    const srcDir = path.join(tmpDir, "src");
    await mkdir(srcDir, { recursive: true });
    for (const mod of existingModules) {
      const filePath = path.join(srcDir, mod.replace(/\./g, "/") + ".hs");
      await mkdir(path.dirname(filePath), { recursive: true });
      await writeFile(filePath, `module ${mod} where\n`);
    }
  }

  it("creates stub for missing module", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result.success).toBe(true);
    expect(result.created).toContain("src/Foo.hs");
    expect(result.alreadyExist).toEqual([]);

    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toBe("module Foo where\n");
  });

  it("does not overwrite existing modules", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`,
      ["Foo"]
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result.success).toBe(true);
    expect(result.created).toEqual([]);
    expect(result.alreadyExist).toContain("src/Foo.hs");
  });

  it("handles mixed existing and missing modules", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules:\n    Foo\n    Bar\n    Baz\n  hs-source-dirs: src\n`,
      ["Foo", "Baz"]
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result.success).toBe(true);
    expect(result.created).toEqual(["src/Bar.hs"]);
    expect(result.alreadyExist).toContain("src/Foo.hs");
    expect(result.alreadyExist).toContain("src/Baz.hs");
    expect(result.totalModules).toBe(3);
  });

  it("creates nested directory structure for dotted modules", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: HM.Syntax.Core\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result.created).toContain("src/HM/Syntax/Core.hs");

    const content = await readFile(path.join(tmpDir, "src/HM/Syntax/Core.hs"), "utf-8");
    expect(content).toBe("module HM.Syntax.Core where\n");
  });

  it("returns all-exist summary when nothing to create", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`,
      ["Foo"]
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result.summary).toContain("already have source files");
  });

  it("returns creation summary", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules:\n    A\n    B\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result.summary).toContain("Created 2 stub(s)");
  });

  it("handles empty module list", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  build-depends: base\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result.success).toBe(true);
    expect(result.created).toEqual([]);
    expect(result.totalModules).toBe(0);
  });
});
