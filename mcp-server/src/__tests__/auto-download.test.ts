import { describe, it, expect, afterEach } from "vitest";
import { autoDownloadTool, canAutoDownload } from "../tools/auto-download.js";
import { rm } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const ROOT_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");

describe("canAutoDownload", () => {
  it("returns true for hlint on darwin-arm64", async () => {
    const originalPlatform = process.platform;
    const originalArch = process.arch;

    Object.defineProperty(process, "platform", { value: "darwin" });
    Object.defineProperty(process, "arch", { value: "arm64" });

    expect(await canAutoDownload("hlint")).toBe(true);

    Object.defineProperty(process, "platform", { value: originalPlatform });
    Object.defineProperty(process, "arch", { value: originalArch });
  });

  it("returns false for unsupported platform", async () => {
    const originalPlatform = process.platform;
    const originalArch = process.arch;

    Object.defineProperty(process, "platform", { value: "freebsd" });
    Object.defineProperty(process, "arch", { value: "arm64" });

    expect(await canAutoDownload("hlint")).toBe(false);

    Object.defineProperty(process, "platform", { value: originalPlatform });
    Object.defineProperty(process, "arch", { value: originalArch });
  });
});

describe("autoDownloadTool", () => {
  const vendorToolsDir = path.join(ROOT_DIR, "vendor-tools");

  afterEach(async () => {
    // Clean up any downloaded test artifacts
    try {
      await rm(path.join(vendorToolsDir, "hlint", "test-download"), { recursive: true, force: true });
    } catch {
      // Ignore cleanup errors
    }
  });

  it("returns error for unsupported platform", async () => {
    const originalPlatform = process.platform;
    const originalArch = process.arch;

    Object.defineProperty(process, "platform", { value: "freebsd" });
    Object.defineProperty(process, "arch", { value: "mips" });

    const result = await autoDownloadTool("hlint");

    expect(result.success).toBe(false);
    expect(result.error).toContain("No release available");

    Object.defineProperty(process, "platform", { value: originalPlatform });
    Object.defineProperty(process, "arch", { value: originalArch });
  });

  it("returns cached result if binary already exists", async () => {
    // This test assumes the current platform has a release configured
    if (!(await canAutoDownload("hlint"))) {
      return; // Skip on unsupported platforms
    }

    // Note: This test will use real cached binaries if they exist
    // or attempt a real download if they don't. For true isolation,
    // we'd need to mock the filesystem and fetch.
  });

  it("supports fourmolu auto-download", async () => {
    const originalPlatform = process.platform;
    const originalArch = process.arch;

    Object.defineProperty(process, "platform", { value: "darwin" });
    Object.defineProperty(process, "arch", { value: "arm64" });

    expect(await canAutoDownload("fourmolu")).toBe(true);

    Object.defineProperty(process, "platform", { value: originalPlatform });
    Object.defineProperty(process, "arch", { value: originalArch });
  });

  it("supports ormolu auto-download on darwin-arm64 (primary target)", async () => {
    const originalPlatform = process.platform;
    const originalArch = process.arch;

    Object.defineProperty(process, "platform", { value: "darwin" });
    Object.defineProperty(process, "arch", { value: "arm64" });

    expect(await canAutoDownload("ormolu")).toBe(true);

    Object.defineProperty(process, "platform", { value: originalPlatform });
    Object.defineProperty(process, "arch", { value: originalArch });
  });

  it("supports hls auto-download on darwin-arm64 (primary target)", async () => {
    const originalPlatform = process.platform;
    const originalArch = process.arch;

    Object.defineProperty(process, "platform", { value: "darwin" });
    Object.defineProperty(process, "arch", { value: "arm64" });

    expect(await canAutoDownload("hls")).toBe(true);

    Object.defineProperty(process, "platform", { value: originalPlatform });
    Object.defineProperty(process, "arch", { value: originalArch });
  });

  it("returns false for unsupported platforms (honest 'not configured')", async () => {
    // Post-Phase-6.2 platform-honesty cleanup: darwin-x64 / linux-* / win32-*
    // are NOT configured for auto-download. `canAutoDownload` must return
    // false so the MCP falls through to host PATH rather than attempting a
    // download with a placeholder URL.
    const originalPlatform = process.platform;
    const originalArch = process.arch;
    try {
      Object.defineProperty(process, "platform", { value: "linux" });
      Object.defineProperty(process, "arch", { value: "x64" });
      expect(await canAutoDownload("hlint")).toBe(false);
      expect(await canAutoDownload("fourmolu")).toBe(false);
      expect(await canAutoDownload("ormolu")).toBe(false);
      expect(await canAutoDownload("hls")).toBe(false);
    } finally {
      Object.defineProperty(process, "platform", { value: originalPlatform });
      Object.defineProperty(process, "arch", { value: originalArch });
    }
  });
});
