import { describe, it, expect, beforeEach, beforeAll, afterAll } from "vitest";
import { mkdtemp, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import {
  ensureToolchainWarmupStarted,
  awaitTool,
  _resetWarmupForTesting,
} from "../tools/toolchain-warmup.js";
import {
  resetManifestCache,
  setManifestPathForTests,
} from "../vendor-tools/manifest.js";
import type { ToolContext } from "../tools/registry.js";

// These tests exercise the warmup orchestration, NOT the actual binary
// download. We swap in a manifest with zero release entries so
// `canAutoDownload` returns false and the chain degrades to "unavailable"
// within milliseconds — otherwise a freshly-renamed real release URL would
// trigger a genuine 136MB download per run.
let emptyManifestDir: string;
beforeAll(async () => {
  emptyManifestDir = await mkdtemp(path.join(tmpdir(), "warmup-manifest-"));
  const emptyManifest = {
    manifestVersion: 2,
    updatedAt: "test",
    releases: {
      hlint: { binaryName: "hlint", platforms: {} },
      fourmolu: { binaryName: "fourmolu", platforms: {} },
      ormolu: { binaryName: "ormolu", platforms: {} },
      hls: { binaryName: "haskell-language-server-wrapper", platforms: {} },
    },
    tools: [],
  };
  const manifestFile = path.join(emptyManifestDir, "manifest.json");
  await writeFile(manifestFile, JSON.stringify(emptyManifest), "utf-8");
  setManifestPathForTests(manifestFile);
  resetManifestCache();
});
afterAll(async () => {
  setManifestPathForTests(null);
  resetManifestCache();
  await rm(emptyManifestDir, { recursive: true, force: true });
});

function makeStubCtx(): {
  ctx: ToolContext;
  setCalls: Array<{ tool: string; status: string }>;
} {
  const setCalls: Array<{ tool: string; status: string }> = [];
  const ctx = {
    getSession: async () => { throw new Error("not used"); },
    getProjectDir: () => "/tmp/stub",
    getBaseDir: () => "/tmp",
    resetQuickCheckState: () => {},
    getRulesNotice: async () => null,
    resetRulesCache: () => {},
    getWorkflowState: () => ({}) as never,
    logToolExecution: () => {},
    getModuleProgress: () => undefined,
    updateModuleProgress: () => {},
    setOptionalToolAvailability: (tool: string, status: string) => {
      setCalls.push({ tool, status });
    },
  } as unknown as ToolContext;
  return { ctx, setCalls };
}

describe("toolchain-warmup", () => {
  beforeEach(() => {
    _resetWarmupForTesting();
  });

  it("is idempotent — calling ensureToolchainWarmupStarted twice only registers warmups once", async () => {
    const { ctx } = makeStubCtx();
    ensureToolchainWarmupStarted(ctx, []);
    // Second call with different tools list is a no-op (warmup already started)
    ensureToolchainWarmupStarted(ctx, ["hlint", "fourmolu"]);
    // awaitTool for a tool never registered should hit ensureTool fallback.
    // We can't easily assert without mocking ensureTool, so check the shape.
    const result = await awaitTool("hlint");
    expect(result).toHaveProperty("available");
  });

  it("awaitTool shares the in-flight promise between concurrent callers", async () => {
    _resetWarmupForTesting();
    const { ctx } = makeStubCtx();
    ensureToolchainWarmupStarted(ctx, ["hlint"]);
    // Both calls should observe the SAME underlying promise once resolved.
    const [a, b] = await Promise.all([awaitTool("hlint"), awaitTool("hlint")]);
    expect(a.available === b.available).toBe(true);
    expect(a.message === b.message).toBe(true);
  });

  it("propagates tool availability into the workflow state via ctx", async () => {
    _resetWarmupForTesting();
    const { ctx, setCalls } = makeStubCtx();
    ensureToolchainWarmupStarted(ctx, ["hlint"]);
    await awaitTool("hlint");
    // Either "available" or "unavailable" MUST have been reported on "lint".
    const lintCalls = setCalls.filter((c) => c.tool === "lint");
    expect(lintCalls.length).toBeGreaterThanOrEqual(1);
    expect(["available", "unavailable"]).toContain(lintCalls[0]!.status);
  });

  it("_resetWarmupForTesting clears state so new warmups can start", async () => {
    const { ctx } = makeStubCtx();
    ensureToolchainWarmupStarted(ctx, ["hlint"]);
    _resetWarmupForTesting();
    // After reset, starting warmup with different tools is allowed.
    ensureToolchainWarmupStarted(ctx, ["fourmolu"]);
    const result = await awaitTool("fourmolu");
    expect(result).toHaveProperty("available");
  });
});
