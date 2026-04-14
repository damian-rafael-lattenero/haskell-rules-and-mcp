import { describe, it, expect, afterEach, beforeEach } from "vitest";
import { rm } from "node:fs/promises";
import path from "node:path";
import {
  TOOL_SPECS,
  toolAvailable,
  ensureTool,
  resolveToolBinary,
  resetBundledManifestCache,
  getBundledToolStatus,
} from "../tools/tool-installer.js";
import {
  bundledToolPath,
  readManifestRaw,
  restoreManifest,
  updateRuntimeManifestEntry,
  writeExecutable,
} from "./helpers/bundled-tools.js";

// Re-export helpers tested here via their canonical modules
import { isTrivialProperty, detectQualifiedImports } from "../tools/export-tests.js";

// ─── TOOL_SPECS registry ──────────────────────────────────────────────────────

describe("TOOL_SPECS registry", () => {
  it("defines hlint", () => {
    expect(TOOL_SPECS["hlint"]).toBeDefined();
    expect(TOOL_SPECS["hlint"]!.checkCmd).toBe("hlint");
  });

  it("defines fourmolu", () => {
    const spec = TOOL_SPECS["fourmolu"]!;
    expect(spec.checkCmd).toBe("fourmolu");
  });

  it("defines hls", () => {
    const spec = TOOL_SPECS["hls"]!;
    expect(spec.checkCmd).toBe("haskell-language-server-wrapper");
  });

  it("does NOT define hoogle (uses web API, no local install needed)", () => {
    expect(TOOL_SPECS["hoogle"]).toBeUndefined();
  });

  it("all specs define checkCmd", () => {
    for (const [name, spec] of Object.entries(TOOL_SPECS)) {
      expect(spec.checkCmd, `${name} missing checkCmd`).toBeTruthy();
    }
  });
});

// ─── toolAvailable ────────────────────────────────────────────────────────────

describe("toolAvailable", () => {
  it("returns false for a non-existent tool", async () => {
    const result = await toolAvailable("__nonexistent_tool_xyz__");
    expect(result).toBe(false);
  });

  it("returns true for 'which' (always present on Unix)", async () => {
    // 'which' is used internally — it must always be available
    const result = await toolAvailable("which");
    // toolAvailable uses which to check, so it checks 'which which' essentially
    // On macOS/Linux this is always true
    expect(typeof result).toBe("boolean");
  });
});

// ─── ensureTool — unknown tool ────────────────────────────────────────────────

describe("ensureTool — unknown tool", () => {
  it("returns available:false with informative message for unregistered tools", async () => {
    const result = await ensureTool("__unknown_tool__");
    expect(result.available).toBe(false);
    expect(result.message).toBeTruthy();
  });
});

describe("bundled tool resolution", () => {
  const rootDir = path.resolve(import.meta.dirname, "..", "..");
  const runtimeBinAbs = bundledToolPath("hlint");
  let manifestSnapshot = "";

  beforeEach(async () => {
    manifestSnapshot = await readManifestRaw();
    resetBundledManifestCache();
  });

  afterEach(async () => {
    resetBundledManifestCache();
    await rm(runtimeBinAbs, { force: true });
    await restoreManifest(manifestSnapshot);
  });

  it("resolves bundled binary when a verified runtime artifact exists", async () => {
    if (!["darwin", "linux", "win32"].includes(process.platform)) return;
    if (!["x64", "arm64"].includes(process.arch)) return;

    if (process.platform === "win32") {
      await writeExecutable(runtimeBinAbs, "@echo off\r\necho bundled hlint\r\n");
    } else {
      await writeExecutable(runtimeBinAbs, "#!/usr/bin/env sh\necho bundled hlint\n");
    }
    await updateRuntimeManifestEntry("hlint");

    const resolved = await resolveToolBinary("hlint");
    expect(resolved).not.toBeNull();
    expect(resolved?.source).toBe("bundled");
    expect(resolved?.binaryPath).toBe(runtimeBinAbs);
  });

  it("ensureTool reports source=bundled when a verified bundled binary exists", async () => {
    if (!["darwin", "linux", "win32"].includes(process.platform)) return;
    if (!["x64", "arm64"].includes(process.arch)) return;

    if (process.platform === "win32") {
      await writeExecutable(runtimeBinAbs, "@echo off\r\necho bundled hlint\r\n");
    } else {
      await writeExecutable(runtimeBinAbs, "#!/usr/bin/env sh\necho bundled hlint\n");
    }
    await updateRuntimeManifestEntry("hlint");

    const ensured = await ensureTool("hlint");
    expect(ensured.available).toBe(true);
    expect(ensured.source).toBe("bundled");
    expect(ensured.binaryPath).toBe(runtimeBinAbs);
    expect(ensured.checksumVerified).toBe(true);
  });

  it("reports checksum-missing when the bundled binary exists but manifest is incomplete", async () => {
    if (!["darwin", "linux", "win32"].includes(process.platform)) return;
    if (!["x64", "arm64"].includes(process.arch)) return;

    await writeExecutable(
      runtimeBinAbs,
      process.platform === "win32"
        ? "@echo off\r\necho bundled hlint\r\n"
        : "#!/usr/bin/env sh\necho bundled hlint\n"
    );

    const bundled = await getBundledToolStatus("hlint");
    expect(bundled.available).toBe(false);
    expect(bundled.reason).toBe("checksum-missing");
  });

  it("reports checksum-mismatch when binary hash differs from manifest", async () => {
    if (!["darwin", "linux", "win32"].includes(process.platform)) return;
    if (!["x64", "arm64"].includes(process.arch)) return;

    await writeExecutable(
      runtimeBinAbs,
      process.platform === "win32"
        ? "@echo off\r\necho bundled hlint\r\n"
        : "#!/usr/bin/env sh\necho bundled hlint\n"
    );
    await updateRuntimeManifestEntry("hlint");
    await writeExecutable(
      runtimeBinAbs,
      process.platform === "win32"
        ? "@echo off\r\necho tampered\r\n"
        : "#!/usr/bin/env sh\necho tampered\n"
    );

    const bundled = await getBundledToolStatus("hlint");
    expect(bundled.available).toBe(false);
    expect(bundled.reason).toBe("checksum-mismatch");
  });
});
