import { describe, it, expect, afterEach } from "vitest";
import { mkdtempSync, mkdirSync, rmSync, writeFileSync, readFileSync, existsSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { addModulesToCabal, handleAddModules } from "../tools/add-modules.js";

function cabalFixture(modules: string[]): string {
  const lines = [
    "cabal-version:      2.4",
    "name:               fx",
    "version:            0.1.0.0",
    "build-type:         Simple",
    "",
    "library",
    "  exposed-modules:",
    ...modules.map((m) => `    ${m}`),
    "  build-depends:    base",
    "  hs-source-dirs:   src",
    "  default-language: GHC2024",
    "",
  ];
  return lines.join("\n");
}

describe("addModulesToCabal", () => {
  const tmpDirs: string[] = [];
  function mkTmp(modules: string[]): string {
    const d = mkdtempSync(path.join(os.tmpdir(), "add-modules-"));
    tmpDirs.push(d);
    writeFileSync(path.join(d, "fx.cabal"), cabalFixture(modules), "utf-8");
    mkdirSync(path.join(d, "src"), { recursive: true });
    return d;
  }
  afterEach(() => {
    for (const d of tmpDirs.splice(0)) {
      try { rmSync(d, { recursive: true, force: true }); } catch { /* ignore */ }
    }
  });

  it("appends new modules preserving existing indentation", async () => {
    const dir = mkTmp(["Foo"]);
    const added = await addModulesToCabal(dir, ["Bar", "Baz.Qux"]);
    expect(added).toEqual(["Bar", "Baz.Qux"]);
    const cabal = readFileSync(path.join(dir, "fx.cabal"), "utf-8");
    expect(cabal).toContain("    Foo");
    expect(cabal).toContain("    Bar");
    expect(cabal).toContain("    Baz.Qux");
  });

  it("is idempotent for already-listed modules", async () => {
    const dir = mkTmp(["Foo", "Bar"]);
    const added = await addModulesToCabal(dir, ["Foo", "Bar"]);
    expect(added).toEqual([]);
    const cabal = readFileSync(path.join(dir, "fx.cabal"), "utf-8");
    const fooCount = (cabal.match(/^\s+Foo$/gm) ?? []).length;
    expect(fooCount).toBe(1);
  });
});

describe("handleAddModules", () => {
  const tmpDirs: string[] = [];
  function mkTmp(modules: string[]): string {
    const d = mkdtempSync(path.join(os.tmpdir(), "add-modules-"));
    tmpDirs.push(d);
    writeFileSync(path.join(d, "fx.cabal"), cabalFixture(modules), "utf-8");
    mkdirSync(path.join(d, "src"), { recursive: true });
    return d;
  }
  afterEach(() => {
    for (const d of tmpDirs.splice(0)) {
      try { rmSync(d, { recursive: true, force: true }); } catch { /* ignore */ }
    }
  });

  it("updates cabal and scaffolds stubs for new modules", async () => {
    const dir = mkTmp(["Foo"]);
    const response = JSON.parse(
      await handleAddModules(dir, { modules: ["Bar", "Baz.Qux"] })
    );
    expect(response.success).toBe(true);
    expect(response.cabalUpdated).toEqual(["Bar", "Baz.Qux"]);
    expect(existsSync(path.join(dir, "src", "Bar.hs"))).toBe(true);
    expect(existsSync(path.join(dir, "src", "Baz", "Qux.hs"))).toBe(true);
  });

  it("fails when the project has no .cabal file", async () => {
    const dir = mkdtempSync(path.join(os.tmpdir(), "add-modules-no-cabal-"));
    tmpDirs.push(dir);
    const response = JSON.parse(
      await handleAddModules(dir, { modules: ["Foo"] })
    );
    expect(response.success).toBe(false);
    expect(response.error).toMatch(/No \.cabal file/i);
    expect(response.hint).toMatch(/ghci_create_project/);
  });

  it("fails when modules array is empty (no silent no-op)", async () => {
    const dir = mkTmp(["Foo"]);
    const response = JSON.parse(
      await handleAddModules(dir, { modules: [] })
    );
    expect(response.success).toBe(false);
    expect(response.error).toMatch(/No modules specified/i);
  });

  it("honors update_cabal=false and rejects modules missing from cabal", async () => {
    const dir = mkTmp(["Foo"]);
    const response = JSON.parse(
      await handleAddModules(dir, { modules: ["NotInCabal"], update_cabal: false })
    );
    expect(response.success).toBe(false);
    expect(response.error).toMatch(/Modules not listed in cabal/i);
  });

  it("generates typed stubs when signatures are provided", async () => {
    const dir = mkTmp(["Foo"]);
    const response = JSON.parse(
      await handleAddModules(dir, {
        modules: ["Bar"],
        signatures: { Bar: ["bar :: Int -> Int"] },
      })
    );
    expect(response.success).toBe(true);
    const bar = readFileSync(path.join(dir, "src", "Bar.hs"), "utf-8");
    expect(bar).toContain("bar :: Int -> Int");
    expect(bar).toContain("bar = undefined");
  });
});
