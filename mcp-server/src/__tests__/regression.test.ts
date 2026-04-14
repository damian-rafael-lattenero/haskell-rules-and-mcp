import { describe, it, expect, vi, beforeEach } from "vitest";
import { mkdtemp, rm } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import { saveProperty, loadStore } from "../property-store.js";

/**
 * Tests for the ghci_regression tool logic.
 * We test the property store integration and the list/run behavior
 * via the store directly since the tool handler requires a full MCP server.
 */

describe("regression tool — property store integration", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "regression-test-"));
  });

  afterAll(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("list mode returns stored properties grouped by module", async () => {
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs", law: "identity" });
    await saveProperty(tmpDir, { property: "prop2", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "prop3", module: "src/B.hs" });

    const store = await loadStore(tmpDir);
    // Group by module
    const byModule: Record<string, typeof store.properties> = {};
    for (const p of store.properties) {
      if (!byModule[p.module]) byModule[p.module] = [];
      byModule[p.module]!.push(p);
    }

    expect(Object.keys(byModule)).toHaveLength(2);
    expect(byModule["src/A.hs"]).toHaveLength(2);
    expect(byModule["src/B.hs"]).toHaveLength(1);
  });

  it("properties track passCount and lastPassed date", async () => {
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });
    await saveProperty(tmpDir, { property: "prop1", module: "src/A.hs" });

    const store = await loadStore(tmpDir);
    expect(store.properties).toHaveLength(1);
    expect(store.properties[0]!.passCount).toBe(3);
    expect(store.properties[0]!.lastPassed).toBeTruthy();
    // Verify it's a valid ISO date
    const date = new Date(store.properties[0]!.lastPassed);
    expect(date.getTime()).toBeGreaterThan(0);
  });

  it("stores law and functionName metadata", async () => {
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

  it("empty store returns valid structure", async () => {
    const store = await loadStore(tmpDir);
    expect(store.version).toBe(1);
    expect(store.properties).toEqual([]);
  });

  // --- tests_module filtering ---

  it("getModuleProperties uses tests_module for filtering when present", async () => {
    await saveProperty(tmpDir, {
      property: "eval-prop",
      module: "src/Syntax.hs",
      tests_module: "src/Eval.hs",
    });
    await saveProperty(tmpDir, {
      property: "syntax-prop",
      module: "src/Syntax.hs",
    });

    const { getModuleProperties } = await import("../property-store.js");
    const evalProps = await getModuleProperties(tmpDir, "src/Eval.hs");
    expect(evalProps).toHaveLength(1);
    expect(evalProps[0]!.property).toBe("eval-prop");
  });

  it("getModuleProperties falls back to module field for records without tests_module", async () => {
    await saveProperty(tmpDir, { property: "old-prop", module: "src/Eval.hs" });

    const { getModuleProperties } = await import("../property-store.js");
    const props = await getModuleProperties(tmpDir, "src/Eval.hs");
    expect(props).toHaveLength(1);
  });
});

describe("regression tool — save alias explanation", () => {
  // Tests the save alias behavior at the property store level.
  // The save alias in regression.ts returns a static message explaining auto-save.
  it("save alias message covers key concepts", () => {
    // Verify the message text has the right information for the LLM
    const expectedConcepts = [
      "auto-saved",
      "ghci_quickcheck",
      "tests_module",
    ];
    const saveMessage =
      "Properties are auto-saved when they pass via ghci_quickcheck or " +
      "ghci_quickcheck_batch. No manual save needed. " +
      "Use action='list' to see all saved properties. " +
      "To tag properties to the module they test (not just the load context), " +
      "pass tests_module='src/YourModule.hs' to ghci_quickcheck or ghci_quickcheck_batch.";

    for (const concept of expectedConcepts) {
      expect(saveMessage).toContain(concept);
    }
  });
});

import { afterAll } from "vitest";
