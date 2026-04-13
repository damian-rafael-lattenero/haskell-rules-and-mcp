import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { loadStore, saveProperty, getModuleProperties, getAllProperties, saveStore } from "../property-store.js";
import { mkdtemp, rm } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

describe("property-store", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "propstore-test-"));
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("loadStore returns empty store for new project", async () => {
    const store = await loadStore(tmpDir);
    expect(store.version).toBe(1);
    expect(store.properties).toHaveLength(0);
  });

  it("saveProperty creates store file and saves property", async () => {
    await saveProperty(tmpDir, {
      property: "\\x -> reverse (reverse x) == (x :: [Int])",
      module: "src/Lib.hs",
      functionName: "reverse",
    });
    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.property).toContain("reverse");
    expect(store.properties[0]!.module).toBe("src/Lib.hs");
    expect(store.properties[0]!.passCount).toBe(1);
    expect(store.properties[0]!.lastPassed).toBeDefined();
  });

  it("deduplicates on same property+module", async () => {
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });
    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.passCount).toBe(3);
  });

  it("stores different properties separately", async () => {
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "prop2", module: "src/A.hs" });
    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(2);
  });

  it("deduplicates same property across different modules", async () => {
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "prop1", module: "src/B.hs" });
    const store = await loadStore(tmpDir);
    // Same property string → deduplicated, keeps first module
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.passCount).toBe(2);
    expect(store.properties[0]!.module).toBe("src/A.hs");
  });

  it("getModuleProperties filters by module", async () => {
    await saveProperty(tmpDir, { property: "p1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "p2", module: "src/B.hs" });
    await saveProperty(tmpDir, { property: "p3", module: "src/A.hs" });
    const aProps = await getModuleProperties(tmpDir, "src/A.hs");
    expect(aProps).toHaveLength(2);
    const bProps = await getModuleProperties(tmpDir, "src/B.hs");
    expect(bProps).toHaveLength(1);
  });

  it("getAllProperties returns everything", async () => {
    await saveProperty(tmpDir, { property: "p1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "p2", module: "src/B.hs" });
    const all = await getAllProperties(tmpDir);
    expect(all).toHaveLength(2);
  });

  it("saves optional law and functionName", async () => {
    await saveProperty(tmpDir, {
      property: "\\x -> fmap id x == x",
      module: "src/Core.hs",
      functionName: "fmap",
      law: "Functor identity",
    });
    const store = await loadStore(tmpDir);
    expect(store.properties[0]!.law).toBe("Functor identity");
    expect(store.properties[0]!.functionName).toBe("fmap");
  });
});
