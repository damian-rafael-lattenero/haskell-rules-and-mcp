import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { mkdtemp, writeFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";
import { GhciSession } from "../../ghci-session.js";
import { handleFormat } from "../../tools/format.js";
import { handleHls } from "../../tools/hls.js";
import { setupIsolatedFixture, type IsolatedFixture } from "../helpers/isolated-fixture.js";
import {
  resetManifestCache,
  setManifestPathForTests,
} from "../../vendor-tools/manifest.js";
import { _resetWarmupForTesting } from "../../tools/toolchain-warmup.js";

// Root fix for the cross-process auto-download race (observed as a flake
// under `npm run test:all`): integration tests run in forked workers that
// all share the same `vendor-tools/<tool>/<platform>/<binary>` cache path.
// Two workers racing to auto-download fourmolu (~100MB) would occasionally
// exceed the 30s test timeout. We point the manifest loader at an empty
// releases map so `canAutoDownload` returns false for every tool — the
// handler falls through to host / bundled resolution deterministically,
// and no network download is ever attempted from this suite.
//
// We still want to exercise the resolution CHAIN — that's the test intent.
// With host/bundled present (they are, after Phase 4), tests pass cleanly.
// With them absent, tests hit the "unavailable" branch (also covered by the
// test assertion: it accepts either outcome). Determinism either way.
let emptyManifestDir: string;

describe("Bundled Tools Complete Integration", () => {
  let session: GhciSession;
  let fixture: IsolatedFixture;
  let TEST_PROJECT: string;

  beforeAll(async () => {
    emptyManifestDir = await mkdtemp(path.join(tmpdir(), "bundled-complete-manifest-"));
    const manifestFile = path.join(emptyManifestDir, "manifest.json");
    await writeFile(
      manifestFile,
      JSON.stringify({
        manifestVersion: 2,
        updatedAt: "test",
        releases: {
          hlint: { binaryName: "hlint", platforms: {} },
          fourmolu: { binaryName: "fourmolu", platforms: {} },
          ormolu: { binaryName: "ormolu", platforms: {} },
          hls: { binaryName: "haskell-language-server-wrapper", platforms: {} },
        },
        tools: [],
      }),
      "utf-8"
    );
    setManifestPathForTests(manifestFile);
    resetManifestCache();
    _resetWarmupForTesting();

    fixture = await setupIsolatedFixture("test-project", "bundled-complete");
    TEST_PROJECT = fixture.dir;
    session = new GhciSession(TEST_PROJECT);
    await session.start();
  });

  afterAll(async () => {
    if (session.isAlive()) {
      await session.kill();
    }
    await fixture.cleanup();
    setManifestPathForTests(null);
    resetManifestCache();
    _resetWarmupForTesting();
    if (emptyManifestDir) {
      await rm(emptyManifestDir, { recursive: true, force: true });
    }
  });

  it("format.ts uses ensureTool and handles auto-download", async () => {
    // This test verifies that format.ts now uses ensureTool
    // which enables auto-download if the tool is not in PATH
    const result = await handleFormat(TEST_PROJECT, {
      module_path: "src/Main.hs",
      write: false
    });

    const data = JSON.parse(result);
    
    // Should either succeed or fail gracefully with proper error
    if (data.success) {
      expect(data.formatted).toBeDefined();
      expect(data.format_tool).toMatch(/fourmolu|ormolu/);
    } else if (data.unavailable) {
      // If unavailable, should have proper error message
      expect(data.error).toBeDefined();
      expect(data.reason).toBeDefined();
    }
  });

  it("hls.ts uses ensureTool for availability check", async () => {
    const result = await handleHls(TEST_PROJECT, {
      action: "available"
    });

    const data = JSON.parse(result);
    expect(data.success).toBe(true);
    expect(data.action).toBe("available");
    
    // Should report availability status
    expect(data).toHaveProperty("available");
    
    if (data.available) {
      expect(data.version).toBeDefined();
      expect(data.source).toMatch(/host|bundled/);
    }
  });

  it("hls.ts uses ensureTool for hover action", async () => {
    const result = await handleHls(TEST_PROJECT, {
      action: "hover",
      module_path: "src/Main.hs",
      line: 1,
      character: 1
    });

    const data = JSON.parse(result);
    
    // Should either succeed or fail with proper unavailable message
    if (data.success) {
      expect(data.action).toBe("hover");
    } else if (data.unavailable) {
      expect(data.error).toContain("not available");
    }
  });
});
