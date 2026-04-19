/**
 * Integration test: the staleness middleware fires end-to-end through
 * `registerStrictTool`. We build a tiny in-process harness with a mock
 * McpServer, register a toy tool, and invoke the wrapped handler the SDK
 * would call. No real tool / GHCi / child-process required.
 *
 * What this catches that unit tests do NOT:
 *   - wiring between `registerStrictTool` and `injectStalenessWarning`
 *   - the path resolution via `activeBundlePath` override
 *   - env-based opt-out (`HASKELL_FLOWS_STALENESS_WARN=0`)
 *
 * Safety: uses a temp-dir bundle via `_setBundlePathOverrideForTests`, so
 * the real `dist/index.js` is NEVER touched — critical because this test
 * runs in-place against the live build tree.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { mkdtemp, writeFile, utimes, rm } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { z } from "zod";
import { registerStrictTool, type ToolContext } from "../../tools/registry.js";
import {
  _resetStalenessCacheForTests,
  _setBootTimeForTests,
  _setBundlePathOverrideForTests,
} from "../../staleness.js";

function makeCtx(projectDir: string): ToolContext {
  return {
    getSession: async () => {
      throw new Error("not used in staleness test");
    },
    getProjectDir: () => projectDir,
    getBaseDir: () => projectDir,
    resetQuickCheckState: () => {},
    getRulesNotice: async () => null,
    resetRulesCache: () => {},
    getWorkflowState: () =>
      ({
        modules: new Map(),
        activeModule: null,
      } as unknown as ReturnType<ToolContext["getWorkflowState"]>),
    logToolExecution: () => {},
    getModuleProgress: () => undefined,
    updateModuleProgress: () => {},
    setOptionalToolAvailability: () => {},
  };
}

describe("staleness middleware — integration via registerStrictTool", () => {
  let tmp: string;
  let fixtureBundle: string;
  let savedEnv: string | undefined;

  beforeEach(async () => {
    tmp = await mkdtemp(path.join(os.tmpdir(), "staleness-integ-"));
    fixtureBundle = path.join(tmp, "index.js");
    savedEnv = process.env.HASKELL_FLOWS_STALENESS_WARN;
    _resetStalenessCacheForTests();
    _setBundlePathOverrideForTests(fixtureBundle);
  });

  afterEach(async () => {
    _setBundlePathOverrideForTests(null);
    _resetStalenessCacheForTests();
    if (savedEnv === undefined) {
      delete process.env.HASKELL_FLOWS_STALENESS_WARN;
    } else {
      process.env.HASKELL_FLOWS_STALENESS_WARN = savedEnv;
    }
    try {
      await rm(tmp, { recursive: true, force: true });
    } catch {
      /* ignore */
    }
  });

  it("injects _warning when the fixture bundle is stale vs the mocked boot time", async () => {
    // Boot 10 min ago.
    const boot = Date.now() - 10 * 60 * 1000;
    _setBootTimeForTests(boot);

    // Fixture bundle 3 min newer than boot → stale.
    await writeFile(fixtureBundle, "// fixture\n");
    const bundleMtime = (boot + 3 * 60 * 1000) / 1000;
    await utimes(fixtureBundle, bundleMtime, bundleMtime);

    let captured:
      | ((args: unknown, extra: unknown) => Promise<unknown>)
      | null = null;
    const mockServer = {
      registerTool: (_n: string, _m: unknown, h: unknown) => {
        captured = h as typeof captured;
      },
    } as unknown as McpServer;

    registerStrictTool(
      mockServer,
      makeCtx(tmp),
      "toy_tool",
      "Returns OK",
      { n: z.number() },
      async ({ n }) => ({
        content: [
          { type: "text", text: JSON.stringify({ success: true, echoed: n }) },
        ],
      })
    );

    const response = (await captured!(
      { n: 42 },
      undefined
    )) as { content: Array<{ type: "text"; text: string }> };
    const body = JSON.parse(response.content[0]!.text);
    expect(body.echoed).toBe(42);
    expect(body._warning).toMatch(/MCP bundle on disk is/);
    expect(body._warning).toMatch(/Restart Claude Desktop/);
  });

  it("does NOT inject when the fixture bundle is older than boot time", async () => {
    const boot = Date.now();
    _setBootTimeForTests(boot);

    await writeFile(fixtureBundle, "// old bundle\n");
    const oldMtime = (boot - 30 * 60 * 1000) / 1000; // 30 min older
    await utimes(fixtureBundle, oldMtime, oldMtime);

    let captured: ((args: unknown, extra: unknown) => Promise<unknown>) | null = null;
    const mockServer = {
      registerTool: (_n: string, _m: unknown, h: unknown) => {
        captured = h as typeof captured;
      },
    } as unknown as McpServer;

    registerStrictTool(
      mockServer,
      makeCtx(tmp),
      "toy_tool_fresh",
      "Returns OK",
      {},
      async () => ({
        content: [{ type: "text", text: JSON.stringify({ success: true }) }],
      })
    );

    const response = (await captured!({}, undefined)) as {
      content: Array<{ type: "text"; text: string }>;
    };
    const body = JSON.parse(response.content[0]!.text);
    expect(body._warning).toBeUndefined();
  });

  it("does NOT inject when HASKELL_FLOWS_STALENESS_WARN=0 even if stale", async () => {
    process.env.HASKELL_FLOWS_STALENESS_WARN = "0";
    const boot = Date.now() - 30 * 60 * 1000;
    _setBootTimeForTests(boot);
    await writeFile(fixtureBundle, "// bundle\n");
    const bundleMtime = (boot + 20 * 60 * 1000) / 1000;
    await utimes(fixtureBundle, bundleMtime, bundleMtime);

    let captured: ((args: unknown, extra: unknown) => Promise<unknown>) | null = null;
    const mockServer = {
      registerTool: (_n: string, _m: unknown, h: unknown) => {
        captured = h as typeof captured;
      },
    } as unknown as McpServer;

    registerStrictTool(
      mockServer,
      makeCtx(tmp),
      "toy_tool_optout",
      "Returns OK",
      {},
      async () => ({
        content: [{ type: "text", text: JSON.stringify({ success: true }) }],
      })
    );

    const response = (await captured!({}, undefined)) as {
      content: Array<{ type: "text"; text: string }>;
    };
    const body = JSON.parse(response.content[0]!.text);
    expect(body._warning).toBeUndefined();
  });

  it("does NOT crash when the bundle path does not exist", async () => {
    // Override to a path that won't exist.
    _setBundlePathOverrideForTests(path.join(tmp, "does-not-exist.js"));
    const boot = Date.now() - 30 * 60 * 1000;
    _setBootTimeForTests(boot);

    let captured: ((args: unknown, extra: unknown) => Promise<unknown>) | null = null;
    const mockServer = {
      registerTool: (_n: string, _m: unknown, h: unknown) => {
        captured = h as typeof captured;
      },
    } as unknown as McpServer;

    registerStrictTool(
      mockServer,
      makeCtx(tmp),
      "toy_tool_missing",
      "Returns OK",
      {},
      async () => ({
        content: [{ type: "text", text: JSON.stringify({ success: true }) }],
      })
    );

    const response = (await captured!({}, undefined)) as {
      content: Array<{ type: "text"; text: string }>;
    };
    const body = JSON.parse(response.content[0]!.text);
    // Non-stale because the probe errored — no warning injected.
    expect(body._warning).toBeUndefined();
    expect(body.success).toBe(true);
  });
});
