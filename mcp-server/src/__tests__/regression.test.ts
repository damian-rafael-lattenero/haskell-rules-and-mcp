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
});

import { afterAll } from "vitest";
