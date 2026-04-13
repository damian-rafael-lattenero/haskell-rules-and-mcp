import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleScaffold, isDeclaration } from "../tools/scaffold.js";
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

  it("generates stubs with signatures and = undefined", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(
      await handleScaffold(tmpDir, {
        Foo: ["bar :: Int -> Int", "baz :: String -> Bool"],
      })
    );
    expect(result.success).toBe(true);
    expect(result.created).toContain("src/Foo.hs");

    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toContain("module Foo where");
    expect(content).toContain("bar :: Int -> Int");
    expect(content).toContain("bar = undefined");
    expect(content).toContain("baz :: String -> Bool");
    expect(content).toContain("baz = undefined");
  });

  it("generates minimal stub when no signatures provided", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    await handleScaffold(tmpDir);
    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toBe("module Foo where\n");
  });

  it("includes _nextStep when stubs created with signatures", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(
      await handleScaffold(tmpDir, { Foo: ["bar :: Int -> Int"] })
    );
    expect(result._nextStep).toBeDefined();
    expect(result._nextStep).toContain("ghci_suggest");
  });

  it("includes _nextStep when stubs created without signatures", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(await handleScaffold(tmpDir));
    expect(result._nextStep).toBeDefined();
    expect(result._nextStep).toContain("= undefined");
  });

  it("overwrites minimal stubs when signatures provided", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`,
      ["Foo"]
    );
    // Minimal stub "module Foo where\n" gets overwritten with typed signatures
    const result = JSON.parse(
      await handleScaffold(tmpDir, { Foo: ["bar :: Int -> Int"] })
    );
    expect(result.created).toContain("src/Foo.hs");
    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toContain("bar :: Int -> Int");
    expect(content).toContain("bar = undefined");
  });

  it("does not overwrite files with real content even with signatures", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    // Write a file with actual implementation
    const filePath = path.join(tmpDir, "src/Foo.hs");
    await writeFile(filePath, "module Foo where\n\nbar :: Int -> Int\nbar x = x + 1\n");
    const result = JSON.parse(
      await handleScaffold(tmpDir, { Foo: ["baz :: String -> Bool"] })
    );
    expect(result.created).toEqual([]);
    expect(result.alreadyExist).toContain("src/Foo.hs");
    // Original content preserved
    const content = await readFile(filePath, "utf-8");
    expect(content).toContain("bar x = x + 1");
    expect(content).not.toContain("baz");
  });

  it("emits data declarations verbatim without = undefined", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    const result = JSON.parse(
      await handleScaffold(tmpDir, {
        Foo: ["data Pos = Pos { posLine :: Int, posCol :: Int } deriving (Show, Eq)"],
      })
    );
    expect(result.success).toBe(true);
    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toContain("data Pos = Pos { posLine :: Int, posCol :: Int } deriving (Show, Eq)");
    expect(content).not.toContain("= undefined");
    // Should appear exactly once (not duplicated)
    const matches = content.match(/data Pos/g);
    expect(matches).toHaveLength(1);
  });

  it("emits newtype declarations verbatim", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    await handleScaffold(tmpDir, {
      Foo: ["newtype Parser a = Parser (String -> [(a, String)])"],
    });
    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toContain("newtype Parser a = Parser (String -> [(a, String)])");
    expect(content).not.toContain("= undefined");
  });

  it("emits type aliases verbatim", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    await handleScaffold(tmpDir, {
      Foo: ["type Name = String"],
    });
    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toContain("type Name = String");
    expect(content).not.toContain("= undefined");
  });

  it("handles mixed declarations and function signatures", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    await handleScaffold(tmpDir, {
      Foo: [
        "data Pos = Pos Int Int",
        "advance :: Pos -> Char -> Pos",
      ],
    });
    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    // Data declaration verbatim, no = undefined
    expect(content).toContain("data Pos = Pos Int Int");
    // Function signature with = undefined
    expect(content).toContain("advance :: Pos -> Char -> Pos");
    expect(content).toContain("advance = undefined");
    // Only advance has = undefined, not the data type
    const undefinedMatches = content.match(/= undefined/g);
    expect(undefinedMatches).toHaveLength(1);
  });

  it("handles deriving/instance declarations verbatim", async () => {
    await setupProject(
      `cabal-version: 3.12\nname: test\n\nlibrary\n  exposed-modules: Foo\n  hs-source-dirs: src\n`
    );
    await handleScaffold(tmpDir, {
      Foo: ["deriving instance Show Pos"],
    });
    const content = await readFile(path.join(tmpDir, "src/Foo.hs"), "utf-8");
    expect(content).toContain("deriving instance Show Pos");
    expect(content).not.toContain("= undefined");
  });
});

describe("isDeclaration", () => {
  it("detects data declarations", () => {
    expect(isDeclaration("data Pos = Pos Int Int")).toBe(true);
    expect(isDeclaration("data ParseError = ParseError { errPos :: Pos }")).toBe(true);
  });

  it("detects newtype declarations", () => {
    expect(isDeclaration("newtype Parser a = Parser (String -> [(a, String)])")).toBe(true);
  });

  it("detects type aliases", () => {
    expect(isDeclaration("type Name = String")).toBe(true);
  });

  it("detects class declarations", () => {
    expect(isDeclaration("class Monad m => MonadError e m where")).toBe(true);
  });

  it("detects instance declarations", () => {
    expect(isDeclaration("instance Show Pos where")).toBe(true);
    expect(isDeclaration("deriving instance Show Pos")).toBe(true);
  });

  it("returns false for function signatures", () => {
    expect(isDeclaration("foo :: Int -> Int")).toBe(false);
    expect(isDeclaration("satisfy :: String -> (Char -> Bool) -> Parser Char")).toBe(false);
  });
});
