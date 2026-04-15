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

  // --- tests_module field ---

  it("saveProperty stores tests_module when provided", async () => {
    await saveProperty(tmpDir, {
      property: "\\n -> eval [] (Lit n) == Right n",
      module: "src/Expr/Syntax.hs",
      tests_module: "src/Expr/Eval.hs",
    });
    const store = await loadStore(tmpDir);
    expect(store.properties[0]!.tests_module).toBe("src/Expr/Eval.hs");
    expect(store.properties[0]!.module).toBe("src/Expr/Syntax.hs");
  });

  it("getModuleProperties filters by tests_module when present", async () => {
    await saveProperty(tmpDir, {
      property: "prop-eval",
      module: "src/Syntax.hs",
      tests_module: "src/Eval.hs",
    });
    await saveProperty(tmpDir, {
      property: "prop-syntax",
      module: "src/Syntax.hs",
    });

    const evalProps = await getModuleProperties(tmpDir, "src/Eval.hs");
    expect(evalProps).toHaveLength(1);
    expect(evalProps[0]!.property).toBe("prop-eval");

    // src/Syntax.hs via legacy module field
    const syntaxProps = await getModuleProperties(tmpDir, "src/Syntax.hs");
    expect(syntaxProps).toHaveLength(1);
    expect(syntaxProps[0]!.property).toBe("prop-syntax");
  });

  it("getModuleProperties falls back to module field for old records without tests_module", async () => {
    // Simulate a record saved before tests_module existed
    const { saveStore } = await import("../property-store.js");
    const legacyStore = {
      version: 1 as const,
      properties: [
        {
          property: "\\x -> x == x",
          module: "src/Legacy.hs",
          lastPassed: new Date().toISOString(),
          passCount: 1,
          // No tests_module field
        },
      ],
    };
    await saveStore(tmpDir, legacyStore);

    const props = await getModuleProperties(tmpDir, "src/Legacy.hs");
    expect(props).toHaveLength(1);
    expect(props[0]!.property).toBe("\\x -> x == x");
  });

  it("dedup upgrade: sets tests_module on existing record when not previously stored", async () => {
    // First save without tests_module
    await saveProperty(tmpDir, { property: "prop1", module: "src/Syntax.hs" });
    // Second save with tests_module — should upgrade
    await saveProperty(tmpDir, {
      property: "prop1",
      module: "src/Syntax.hs",
      tests_module: "src/Eval.hs",
    });
    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.tests_module).toBe("src/Eval.hs");
    expect(store.properties[0]!.passCount).toBe(2);
  });
});
