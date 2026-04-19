import { describe, it, expect, beforeEach } from "vitest";
import { mkdtemp, writeFile, rm, utimes } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import {
  computeStaleness,
  stalenessMessage,
  defaultBundlePath,
  _resetStalenessCacheForTests,
  STALENESS_THRESHOLD_MS,
} from "../staleness.js";
import { injectStalenessWarning } from "../tools/registry.js";

describe("staleness detector — pure", () => {
  let tmp: string;
  let bundlePath: string;

  beforeEach(async () => {
    tmp = await mkdtemp(path.join(os.tmpdir(), "staleness-test-"));
    bundlePath = path.join(tmp, "index.js");
    _resetStalenessCacheForTests();
  });

  async function writeBundle(mtimeMs: number): Promise<void> {
    await writeFile(bundlePath, "// bundle\n");
    const t = mtimeMs / 1000;
    await utimes(bundlePath, t, t);
  }

  it("reports not-stale when bundle does not exist", async () => {
    const now = 1_000_000;
    const r = await computeStaleness({
      bundlePath,
      nowMs: () => now,
      bootTimeMs: () => now - 60_000,
    });
    expect(r.stale).toBe(false);
    expect(r.error).toBeDefined();
  });

  it("reports not-stale when bundle is OLDER than boot time", async () => {
    const now = Date.now();
    // Bundle built 5 min before boot
    await writeBundle(now - 10 * 60_000);
    const r = await computeStaleness({
      bundlePath,
      nowMs: () => now,
      bootTimeMs: () => now - 5 * 60_000,
    });
    expect(r.stale).toBe(false);
    expect(r.bundleAheadByMs).toBeLessThan(0);
  });

  it("reports not-stale when bundle is just barely newer (below threshold)", async () => {
    const now = Date.now();
    const bootTime = now - 10 * 60_000;
    // Bundle 1 min newer than boot — below 2 min threshold
    await writeBundle(bootTime + 60_000);
    const r = await computeStaleness({
      bundlePath,
      nowMs: () => now,
      bootTimeMs: () => bootTime,
    });
    expect(r.stale).toBe(false);
    expect(r.bundleAheadByMs).toBe(60_000);
  });

  it("reports stale when bundle mtime exceeds threshold past boot", async () => {
    const now = Date.now();
    const bootTime = now - 30 * 60_000;
    // Bundle 10 min newer than boot — well over threshold
    await writeBundle(bootTime + 10 * 60_000);
    const r = await computeStaleness({
      bundlePath,
      nowMs: () => now,
      bootTimeMs: () => bootTime,
    });
    expect(r.stale).toBe(true);
    expect(r.bundleAheadByMs).toBe(10 * 60_000);
    expect(stalenessMessage(r)).toMatch(/10 minute\(s\) newer/);
  });

  it("caches within TTL", async () => {
    const now = Date.now();
    const bootTime = now - 30 * 60_000;
    await writeBundle(bootTime + 5 * 60_000);

    const first = await computeStaleness({
      bundlePath,
      nowMs: () => now,
      bootTimeMs: () => bootTime,
      cacheTtlMs: 60_000,
    });
    expect(first.cached).toBe(false);

    // Change the bundle mtime to "newer still" — cached check should NOT
    // pick up the change because the TTL has not elapsed.
    await writeBundle(bootTime + 100 * 60_000);
    const second = await computeStaleness({
      bundlePath,
      nowMs: () => now + 10_000, // 10s later, still within TTL
      bootTimeMs: () => bootTime,
      cacheTtlMs: 60_000,
    });
    expect(second.cached).toBe(true);
    expect(second.bundleAheadByMs).toBe(first.bundleAheadByMs);
  });

  it("refreshes after TTL elapses", async () => {
    const bootTime = Date.now() - 60 * 60_000;
    await writeBundle(bootTime + 5 * 60_000);

    const first = await computeStaleness({
      bundlePath,
      nowMs: () => bootTime + 30 * 60_000,
      bootTimeMs: () => bootTime,
      cacheTtlMs: 60_000,
    });
    expect(first.cached).toBe(false);

    await writeBundle(bootTime + 30 * 60_000);
    const second = await computeStaleness({
      bundlePath,
      nowMs: () => bootTime + 31 * 60_000 + 1, // TTL (60s) elapsed
      bootTimeMs: () => bootTime,
      cacheTtlMs: 60_000,
    });
    expect(second.cached).toBe(false);
    expect(second.bundleAheadByMs).toBe(30 * 60_000);
  });

  it("stalenessMessage returns null for non-stale result", () => {
    expect(
      stalenessMessage({
        stale: false,
        bootTimeMs: 0,
        cached: false,
      })
    ).toBeNull();
  });

  it("STALENESS_THRESHOLD_MS is 2 minutes as documented", () => {
    expect(STALENESS_THRESHOLD_MS).toBe(120_000);
  });

  afterEach_cleanup: {
    // no-op — tmp dirs are deliberately leaked to /tmp; the OS reaps them.
  }
});

describe("defaultBundlePath — path walkup", () => {
  it("resolves to <pkg>/dist/index.js from dist/tools/*", () => {
    const fake = "file:///home/u/proj/mcp-server/dist/tools/mcp-reload-code.js";
    expect(defaultBundlePath(fake)).toBe("/home/u/proj/mcp-server/dist/index.js");
  });

  it("returns the input when `dist/` is not in the path", () => {
    const fake = "file:///tmp/standalone.js";
    expect(defaultBundlePath(fake)).toBe("/tmp/standalone.js");
  });
});

describe("injectStalenessWarning — middleware", () => {
  const staleResult = {
    stale: true,
    bundleAheadByMs: 180_000, // 3 min
    bundleMtimeMs: 0,
    bootTimeMs: 0,
    cached: false,
  };

  it("adds _warning to a plain JSON object response", () => {
    const r = injectStalenessWarning(
      { content: [{ type: "text", text: JSON.stringify({ success: true }) }] },
      staleResult
    );
    const body = JSON.parse(r.content[0]!.text);
    expect(body._warning).toMatch(/MCP bundle on disk/);
    expect(body.success).toBe(true);
  });

  it("passes through untouched when not stale", () => {
    const r = injectStalenessWarning(
      { content: [{ type: "text", text: JSON.stringify({ success: true }) }] },
      { ...staleResult, stale: false, bundleAheadByMs: 0 }
    );
    const body = JSON.parse(r.content[0]!.text);
    expect(body._warning).toBeUndefined();
  });

  it("leaves non-JSON text responses untouched", () => {
    const original = { content: [{ type: "text" as const, text: "plain text output" }] };
    const r = injectStalenessWarning(original, staleResult);
    expect(r).toBe(original);
  });

  it("leaves JSON array responses untouched", () => {
    const original = { content: [{ type: "text" as const, text: JSON.stringify([1, 2, 3]) }] };
    const r = injectStalenessWarning(original, staleResult);
    expect(r).toBe(original);
  });

  it("preserves empty content", () => {
    const original = { content: [] };
    const r = injectStalenessWarning(original, staleResult);
    expect(r).toBe(original);
  });

  it("appends to an existing string _warning without duplicating", () => {
    const r1 = injectStalenessWarning(
      {
        content: [
          {
            type: "text",
            text: JSON.stringify({ success: true, _warning: "pre-existing" }),
          },
        ],
      },
      staleResult
    );
    const body1 = JSON.parse(r1.content[0]!.text);
    expect(body1._warning).toMatch(/pre-existing/);
    expect(body1._warning).toMatch(/MCP bundle on disk/);

    // Second pass should NOT duplicate
    const r2 = injectStalenessWarning(r1, staleResult);
    const body2 = JSON.parse(r2.content[0]!.text);
    const occurrences = (body2._warning as string).match(/MCP bundle on disk/g)?.length ?? 0;
    expect(occurrences).toBe(1);
  });

  it("pushes to an existing array _warning without duplicating", () => {
    const r1 = injectStalenessWarning(
      {
        content: [
          {
            type: "text",
            text: JSON.stringify({ success: true, _warning: ["item-a"] }),
          },
        ],
      },
      staleResult
    );
    const body1 = JSON.parse(r1.content[0]!.text);
    expect(body1._warning).toEqual(
      expect.arrayContaining([
        "item-a",
        expect.stringMatching(/MCP bundle on disk/),
      ])
    );

    const r2 = injectStalenessWarning(r1, staleResult);
    const body2 = JSON.parse(r2.content[0]!.text);
    const matches = (body2._warning as string[]).filter((s) =>
      s.includes("MCP bundle on disk")
    );
    expect(matches).toHaveLength(1);
  });

  it("leaves _warning of unexpected type untouched", () => {
    // E.g. a tool that uses `_warning: { code: 1 }` — middleware declines to rewrite.
    const r = injectStalenessWarning(
      {
        content: [
          {
            type: "text",
            text: JSON.stringify({ success: true, _warning: { code: 1 } }),
          },
        ],
      },
      staleResult
    );
    const body = JSON.parse(r.content[0]!.text);
    expect(body._warning).toEqual({ code: 1 });
  });
});

// Helper import kept separate so the unit-test block above stays focused.
import { afterEach } from "vitest";
afterEach(async () => {
  // Best-effort cleanup — TEMP dir contents never leave the test process.
});
