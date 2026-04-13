import { describe, it, expect, vi, afterEach } from "vitest";
import { handleAddImport } from "../tools/add-import.js";

describe("handleAddImport", () => {
  const originalFetch = globalThis.fetch;

  afterEach(() => {
    globalThis.fetch = originalFetch;
  });

  function mockHoogle(results: Array<{ item: string; module: { name: string }; package: { name: string }; docs: string; url: string }>) {
    globalThis.fetch = vi.fn().mockResolvedValue({
      ok: true,
      status: 200,
      json: async () => results,
    });
  }

  it("suggests import for a known function", async () => {
    mockHoogle([
      { item: "sort :: Ord a => [a] -> [a]", module: { name: "Data.List" }, package: { name: "base" }, docs: "Sort a list", url: "" },
      { item: "sort :: Ord a => [a] -> [a]", module: { name: "GHC.OldList" }, package: { name: "base" }, docs: "", url: "" },
    ]);

    const result = JSON.parse(await handleAddImport({ name: "sort" }));
    expect(result.success).toBe(true);
    expect(result.suggestedImport).toBe("import Data.List (sort)");
    expect(result.module).toBe("Data.List");
    expect(result.package).toBe("base");
  });

  it("suggests qualified import when requested", async () => {
    mockHoogle([
      { item: "fromList :: [(k, v)] -> Map k v", module: { name: "Data.Map" }, package: { name: "containers" }, docs: "", url: "" },
    ]);

    const result = JSON.parse(await handleAddImport({ name: "fromList", qualified: true }));
    expect(result.success).toBe(true);
    expect(result.suggestedImport).toBe("import Data.Map qualified");
  });

  it("returns alternatives from different modules", async () => {
    mockHoogle([
      { item: "sort", module: { name: "Data.List" }, package: { name: "base" }, docs: "", url: "" },
      { item: "sort", module: { name: "Data.Vector.Algorithms.Intro" }, package: { name: "vector-algorithms" }, docs: "", url: "" },
    ]);

    const result = JSON.parse(await handleAddImport({ name: "sort" }));
    expect(result.success).toBe(true);
    expect(result.alternatives.length).toBeGreaterThan(0);
  });

  it("prefers base package over others", async () => {
    mockHoogle([
      { item: "sort", module: { name: "Weird.Sort" }, package: { name: "obscure-pkg" }, docs: "", url: "" },
      { item: "sort", module: { name: "Data.List" }, package: { name: "base" }, docs: "", url: "" },
    ]);

    const result = JSON.parse(await handleAddImport({ name: "sort" }));
    expect(result.success).toBe(true);
    expect(result.module).toBe("Data.List"); // base should be preferred
  });

  it("returns error when hoogle finds nothing", async () => {
    mockHoogle([]);

    const result = JSON.parse(await handleAddImport({ name: "totallyFakeName" }));
    expect(result.success).toBe(false);
    expect(result.error).toContain("No Hoogle results");
  });

  it("handles network error gracefully", async () => {
    globalThis.fetch = vi.fn().mockRejectedValue(new Error("Network error"));

    const result = JSON.parse(await handleAddImport({ name: "sort" }));
    expect(result.success).toBe(false);
  });
});
