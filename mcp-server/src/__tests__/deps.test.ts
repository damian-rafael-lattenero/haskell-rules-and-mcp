import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { handleDeps } from "../tools/deps.js";
import { mkdtemp, rm, readFile, writeFile, mkdir } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

const MINIMAL_CABAL = `cabal-version: 2.4
name:          my-pkg
version:       0.1.0.0

library
  exposed-modules: Lib
  build-depends:
    base >= 4.20 && < 5,
    QuickCheck >= 2.14
  hs-source-dirs: src
  default-language: GHC2024
`;

const NO_LIBRARY_CABAL = `cabal-version: 2.4
name:          my-pkg
version:       0.1.0.0

executable my-exe
  main-is: Main.hs
  build-depends: base
`;

describe("handleDeps — list", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "deps-test-"));
    await writeFile(path.join(dir, "my-pkg.cabal"), MINIMAL_CABAL, "utf-8");
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("list returns base and QuickCheck", async () => {
    const result = JSON.parse(await handleDeps(dir, { action: "list" }));
    expect(result.success).toBe(true);
    expect(result.dependencies).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ name: "base" }),
        expect.objectContaining({ name: "QuickCheck" }),
      ])
    );
  });

  it("list includes version strings", async () => {
    const result = JSON.parse(await handleDeps(dir, { action: "list" }));
    const base = result.dependencies.find((d: { name: string }) => d.name === "base");
    expect(base.version).toContain("4.20");
  });

  it("list on no-library cabal returns empty or error gracefully", async () => {
    await writeFile(path.join(dir, "my-pkg.cabal"), NO_LIBRARY_CABAL, "utf-8");
    const result = JSON.parse(await handleDeps(dir, { action: "list" }));
    // No library stanza: empty deps or error, but never crashes
    expect(result).toHaveProperty("success");
  });
});

describe("handleDeps — add", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "deps-test-"));
    await writeFile(path.join(dir, "my-pkg.cabal"), MINIMAL_CABAL, "utf-8");
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("add inserts new package into build-depends", async () => {
    const result = JSON.parse(
      await handleDeps(dir, { action: "add", package: "containers" })
    );
    expect(result.success).toBe(true);
    const cabal = await readFile(path.join(dir, "my-pkg.cabal"), "utf-8");
    expect(cabal).toContain("containers");
  });

  it("add with version inserts pkg >= X.Y constraint", async () => {
    const result = JSON.parse(
      await handleDeps(dir, { action: "add", package: "text", version: ">= 2.0" })
    );
    expect(result.success).toBe(true);
    const cabal = await readFile(path.join(dir, "my-pkg.cabal"), "utf-8");
    expect(cabal).toContain("text >= 2.0");
  });

  it("add of already-present package returns already_present status", async () => {
    const result = JSON.parse(
      await handleDeps(dir, { action: "add", package: "base" })
    );
    expect(result.success).toBe(true);
    expect(result.status).toBe("already_present");
  });

  it("add makes package visible via subsequent list", async () => {
    await handleDeps(dir, { action: "add", package: "containers" });
    const listResult = JSON.parse(await handleDeps(dir, { action: "list" }));
    const names = listResult.dependencies.map((d: { name: string }) => d.name);
    expect(names).toContain("containers");
  });

  it("add without package param returns error", async () => {
    const result = JSON.parse(await handleDeps(dir, { action: "add" }));
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });
});

describe("handleDeps — remove", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "deps-test-"));
    await writeFile(path.join(dir, "my-pkg.cabal"), MINIMAL_CABAL, "utf-8");
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("remove deletes existing package from build-depends", async () => {
    const result = JSON.parse(
      await handleDeps(dir, { action: "remove", package: "QuickCheck" })
    );
    expect(result.success).toBe(true);
    const cabal = await readFile(path.join(dir, "my-pkg.cabal"), "utf-8");
    expect(cabal).not.toContain("QuickCheck");
  });

  it("remove makes package absent via subsequent list", async () => {
    await handleDeps(dir, { action: "remove", package: "QuickCheck" });
    const listResult = JSON.parse(await handleDeps(dir, { action: "list" }));
    const names = listResult.dependencies.map((d: { name: string }) => d.name);
    expect(names).not.toContain("QuickCheck");
  });

  it("remove of non-existent package returns informative error", async () => {
    const result = JSON.parse(
      await handleDeps(dir, { action: "remove", package: "nonexistent-pkg" })
    );
    expect(result.success).toBe(false);
    expect(result.error).toMatch(/nonexistent-pkg|not found/i);
  });

  it("remove without package param returns error", async () => {
    const result = JSON.parse(await handleDeps(dir, { action: "remove" }));
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it("cannot remove base (protected)", async () => {
    const result = JSON.parse(
      await handleDeps(dir, { action: "remove", package: "base" })
    );
    expect(result.success).toBe(false);
    expect(result.error).toMatch(/base|protected/i);
  });
});

describe("handleDeps — graph", () => {
  let dir: string;

  beforeEach(async () => {
    dir = await mkdtemp(path.join(os.tmpdir(), "deps-graph-test-"));
    await writeFile(path.join(dir, "my-pkg.cabal"), MINIMAL_CABAL, "utf-8");
    // Create src directory with some .hs files
    const srcDir = path.join(dir, "src");
    await mkdir(srcDir, { recursive: true });
  });

  afterEach(async () => {
    await rm(dir, { recursive: true, force: true });
  });

  it("empty src directory returns empty graph", async () => {
    const result = JSON.parse(await handleDeps(dir, { action: "graph" }));
    expect(result.success).toBe(true);
    expect(result.nodes).toEqual([]);
    expect(result.edges).toEqual([]);
    expect(result.cycles).toEqual([]);
    expect(result.orphans).toEqual([]);
  });

  it("single module with no imports has no edges and is an orphan", async () => {
    await writeFile(
      path.join(dir, "src", "Foo.hs"),
      "module Foo where\n\nfoo = 42\n",
      "utf-8"
    );
    const result = JSON.parse(await handleDeps(dir, { action: "graph" }));
    expect(result.success).toBe(true);
    expect(result.nodes).toContain("Foo");
    expect(result.edges).toHaveLength(0);
    expect(result.orphans).toContain("Foo");
  });

  it("two modules A imports B: edge A->B, A is orphan (nobody imports A)", async () => {
    await writeFile(
      path.join(dir, "src", "B.hs"),
      "module B where\n\nb = 1\n",
      "utf-8"
    );
    await writeFile(
      path.join(dir, "src", "A.hs"),
      "module A where\nimport B\n\na = b\n",
      "utf-8"
    );
    const result = JSON.parse(await handleDeps(dir, { action: "graph" }));
    expect(result.success).toBe(true);
    expect(result.nodes).toContain("A");
    expect(result.nodes).toContain("B");
    expect(result.edges).toContainEqual({ from: "A", to: "B" });
    expect(result.cycles).toEqual([]);
    // A has no importers (nothing imports A) → A is orphan root
    expect(result.orphans).toContain("A");
    // B is imported by A → not an orphan
    expect(result.orphans).not.toContain("B");
  });

  it("detects cycle A->B->A", async () => {
    await writeFile(
      path.join(dir, "src", "A.hs"),
      "module A where\nimport B\n\na = 1\n",
      "utf-8"
    );
    await writeFile(
      path.join(dir, "src", "B.hs"),
      "module B where\nimport A\n\nb = 1\n",
      "utf-8"
    );
    const result = JSON.parse(await handleDeps(dir, { action: "graph" }));
    expect(result.success).toBe(true);
    expect(result.cycles.length).toBeGreaterThan(0);
    // Cycle should contain both A and B
    const cycleFlat = result.cycles.flat();
    expect(cycleFlat).toContain("A");
    expect(cycleFlat).toContain("B");
  });

  it("ignores external package imports (only project modules in graph)", async () => {
    await writeFile(
      path.join(dir, "src", "Foo.hs"),
      "module Foo where\nimport Data.List (sort)\nimport Data.Map (Map)\n\nfoo = sort [3,1,2]\n",
      "utf-8"
    );
    const result = JSON.parse(await handleDeps(dir, { action: "graph" }));
    expect(result.success).toBe(true);
    // Data.List and Data.Map are not project modules
    expect(result.nodes).not.toContain("Data.List");
    expect(result.nodes).not.toContain("Data.Map");
  });
});
