/**
 * Integration test for the new scaffolding pipeline:
 *   handleCreateProject → handleAddModules → verify on-disk state.
 *
 * Does NOT invoke GHCi (that is exercised at the e2e layer). Covers the
 * file-system contract: cabal generation, module stubs, dotted names,
 * cabal updates, and the "fail cleanly on a populated directory" rule.
 */
import { describe, it, expect, afterEach } from "vitest";
import { mkdtempSync, rmSync, readFileSync, existsSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { handleCreateProject } from "../../tools/create-project.js";
import { handleAddModules } from "../../tools/add-modules.js";

describe("integration: create-project → add-modules pipeline", () => {
  const tmpDirs: string[] = [];

  function mkTmp(): string {
    const d = mkdtempSync(path.join(os.tmpdir(), "create-add-integration-"));
    tmpDirs.push(d);
    return d;
  }

  afterEach(() => {
    for (const d of tmpDirs.splice(0)) {
      try { rmSync(d, { recursive: true, force: true }); } catch { /* ignore */ }
    }
  });

  it("creates a clean project then extends it with more modules", async () => {
    const root = mkTmp();
    const created = await handleCreateProject({
      name: "pipeline-demo",
      rootDir: root,
      modules: ["Foo"],
    });
    expect(created.success).toBe(true);
    if (!created.success) return;

    // 1. Sanity on the fresh layout.
    const projectDir = created.projectDir;
    expect(existsSync(path.join(projectDir, "src", "Foo.hs"))).toBe(true);
    expect(existsSync(path.join(projectDir, "test", "Spec.hs"))).toBe(true);

    // 2. Add two new modules — one flat, one nested, with a typed signature.
    const added = JSON.parse(
      await handleAddModules(projectDir, {
        modules: ["Bar", "Nested.Deep"],
        signatures: { Bar: ["bar :: Int -> Int"] },
      })
    );
    expect(added.success).toBe(true);
    expect(added.cabalUpdated).toEqual(expect.arrayContaining(["Bar", "Nested.Deep"]));

    // 3. Verify the cabal file reflects all three modules.
    const cabal = readFileSync(path.join(projectDir, "pipeline-demo.cabal"), "utf-8");
    expect(cabal).toContain("    Foo");
    expect(cabal).toContain("    Bar");
    expect(cabal).toContain("    Nested.Deep");

    // 4. Verify Bar has the typed stub and Nested.Deep has a plain one.
    const barHs = readFileSync(path.join(projectDir, "src", "Bar.hs"), "utf-8");
    expect(barHs).toContain("bar :: Int -> Int");
    expect(barHs).toContain("bar = undefined");
    expect(existsSync(path.join(projectDir, "src", "Nested", "Deep.hs"))).toBe(true);
  });

  it("ghci_add_modules fails cleanly if the target directory has no .cabal", async () => {
    const root = mkTmp();
    const response = JSON.parse(
      await handleAddModules(root, { modules: ["Foo"] })
    );
    expect(response.success).toBe(false);
    expect(response.hint).toMatch(/ghci_create_project/);
  });

  it("ghci_create_project refuses to overwrite an existing project", async () => {
    const root = mkTmp();
    const first = await handleCreateProject({ name: "dup", rootDir: root });
    expect(first.success).toBe(true);
    const second = await handleCreateProject({ name: "dup", rootDir: root });
    expect(second.success).toBe(false);
    if (second.success) return;
    expect(second.error).toMatch(/already exists/i);
  });
});
