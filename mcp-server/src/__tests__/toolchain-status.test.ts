import { describe, expect, it, beforeAll, afterAll } from "vitest";
import { mkdtemp, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { handleToolchainStatus } from "../tools/toolchain-status.js";
import type { ToolContext } from "../tools/registry.js";
import {
  resetManifestCache,
  setManifestPathForTests,
} from "../vendor-tools/manifest.js";

// Avoid actual 100MB+ binary downloads when runtime probes kick in. With an
// empty releases map, every ensureTool falls through to "unavailable" in
// milliseconds — which is exactly what this test expects to validate.
let emptyManifestDir: string;
beforeAll(async () => {
  emptyManifestDir = await mkdtemp(path.join(tmpdir(), "toolchain-manifest-"));
  const empty = {
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
  await writeFile(manifestFile, JSON.stringify(empty), "utf-8");
  setManifestPathForTests(manifestFile);
  resetManifestCache();
});
afterAll(async () => {
  setManifestPathForTests(null);
  resetManifestCache();
  await rm(emptyManifestDir, { recursive: true, force: true });
});

describe("handleToolchainStatus", () => {
  it("returns release matrix diagnostics without runtime probes", async () => {
    const parsed = JSON.parse(
      await handleToolchainStatus({ include_matrix: true, include_runtime: false })
    );
    expect(parsed.success).toBe(true);
    expect(Array.isArray(parsed.releaseMatrix)).toBe(true);
    expect(parsed.releaseMatrixSummary.total).toBeGreaterThan(0);
  });

  it("propagates runtime tool availability to ctx.setOptionalToolAvailability (Bug 5 fix)", async () => {
    const setCalls: Array<{ tool: string; status: string }> = [];
    const stubCtx = {
      getSession: async () => { throw new Error("not used"); },
      getProjectDir: () => "/tmp",
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

    await handleToolchainStatus({ include_matrix: false, include_runtime: true }, stubCtx);
    // Each runtime tool category (lint/format/hls) should receive exactly one update.
    const categories = new Set(setCalls.map((c) => c.tool));
    expect(categories.has("lint")).toBe(true);
    expect(categories.has("format")).toBe(true);
    expect(categories.has("hls")).toBe(true);
    // Every update must be either "available" or "unavailable" — no "unknown".
    for (const c of setCalls) {
      expect(["available", "unavailable"]).toContain(c.status);
    }
  });
});
