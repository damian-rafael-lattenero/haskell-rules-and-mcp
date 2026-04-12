import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { handleHoogleSearch } from "../tools/hoogle.js";

describe("handleHoogleSearch", () => {
  const originalFetch = globalThis.fetch;

  afterEach(() => {
    globalThis.fetch = originalFetch;
  });

  function mockFetch(results: any[], status = 200) {
    globalThis.fetch = vi.fn().mockResolvedValue({
      ok: status >= 200 && status < 300,
      status,
      json: async () => results,
    });
  }

  it("returns formatted results from Hoogle", async () => {
    mockFetch([
      {
        url: "https://hackage.haskell.org/package/base/docs/Prelude.html#v:map",
        module: { name: "Prelude", url: "" },
        package: { name: "base", url: "" },
        item: "<b>map</b> :: (a -> b) -> [a] -> [b]",
        type: "",
        docs: "Apply a function to each element.",
      },
    ]);
    const result = JSON.parse(await handleHoogleSearch({ query: "map" }));
    expect(result.success).toBe(true);
    expect(result.count).toBe(1);
    expect(result.results[0].name).toBe("map :: (a -> b) -> [a] -> [b]");
    expect(result.results[0].module).toBe("Prelude");
    expect(result.results[0].package).toBe("base");
  });

  it("respects count parameter (max 30)", async () => {
    mockFetch([]);
    await handleHoogleSearch({ query: "map", count: 50 });
    const fetchCall = (globalThis.fetch as any).mock.calls[0][0];
    expect(fetchCall).toContain("count=30");
  });

  it("uses default count of 10", async () => {
    mockFetch([]);
    await handleHoogleSearch({ query: "map" });
    const fetchCall = (globalThis.fetch as any).mock.calls[0][0];
    expect(fetchCall).toContain("count=10");
  });

  it("handles HTTP error", async () => {
    mockFetch([], 500);
    const result = JSON.parse(await handleHoogleSearch({ query: "map" }));
    expect(result.success).toBe(false);
    expect(result.error).toContain("HTTP 500");
  });

  it("handles network error", async () => {
    globalThis.fetch = vi.fn().mockRejectedValue(new Error("Network timeout"));
    const result = JSON.parse(await handleHoogleSearch({ query: "map" }));
    expect(result.success).toBe(false);
    expect(result.error).toContain("Network timeout");
  });

  it("strips HTML from results", async () => {
    mockFetch([
      {
        url: "",
        module: { name: "Prelude", url: "" },
        package: { name: "base", url: "" },
        item: "<span class=name>length</span> :: <a>Foldable</a> t =&gt; t a -&gt; <a>Int</a>",
        type: "",
        docs: "Returns the &amp; length of a list &lt;etc&gt;",
      },
    ]);
    const result = JSON.parse(await handleHoogleSearch({ query: "length" }));
    expect(result.results[0].name).not.toContain("<");
    expect(result.results[0].name).toContain("Foldable");
    expect(result.results[0].docs).toContain("& length");
  });

  it("encodes query properly", async () => {
    mockFetch([]);
    await handleHoogleSearch({ query: "(a -> b) -> [a] -> [b]" });
    const fetchCall = (globalThis.fetch as any).mock.calls[0][0];
    expect(fetchCall).toContain(encodeURIComponent("(a -> b) -> [a] -> [b]"));
  });

  it("truncates docs to 200 chars", async () => {
    mockFetch([
      {
        url: "",
        module: { name: "M", url: "" },
        package: { name: "p", url: "" },
        item: "foo :: Int",
        type: "",
        docs: "A".repeat(500),
      },
    ]);
    const result = JSON.parse(await handleHoogleSearch({ query: "foo" }));
    expect(result.results[0].docs.length).toBeLessThanOrEqual(200);
  });
});
