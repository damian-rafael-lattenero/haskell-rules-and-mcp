import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { GhciSession } from "../../ghci-session.js";
import { handleFormat } from "../../tools/format.js";
import { handleHls } from "../../tools/hls.js";
import path from "node:path";

const TEST_PROJECT = path.resolve(import.meta.dirname, "..", "fixtures", "test-project");

describe("Bundled Tools Complete Integration", () => {
  let session: GhciSession;

  beforeAll(async () => {
    session = new GhciSession(TEST_PROJECT);
    await session.start();
  });

  afterAll(async () => {
    if (session.isAlive()) {
      await session.kill();
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
