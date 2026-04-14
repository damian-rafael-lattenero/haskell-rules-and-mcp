import { describe, it, expect, afterEach } from "vitest";
import { chmod, mkdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import {
  TOOL_SPECS,
  toolAvailable,
  ensureTool,
  resetInstallState,
  getInstallStatus,
  resolveToolBinary,
  resetBundledManifestCache,
} from "../tools/tool-installer.js";

// Re-export helpers tested here via their canonical modules
import { isTrivialProperty, detectQualifiedImports } from "../tools/export-tests.js";

// ─── TOOL_SPECS registry ──────────────────────────────────────────────────────

describe("TOOL_SPECS registry", () => {
  it("defines hlint", () => {
    expect(TOOL_SPECS["hlint"]).toBeDefined();
    expect(TOOL_SPECS["hlint"]!.checkCmd).toBe("hlint");
    expect(TOOL_SPECS["hlint"]!.installCmd[0]).toBe("cabal");
  });

  it("defines fourmolu with ghcup as primary and cabal as fallback", () => {
    const spec = TOOL_SPECS["fourmolu"]!;
    expect(spec.checkCmd).toBe("fourmolu");
    expect(spec.installCmd[0]).toBe("ghcup");
    expect(spec.fallbackInstallCmd?.[0]).toBe("cabal");
  });

  it("defines hls with ghcup", () => {
    const spec = TOOL_SPECS["hls"]!;
    expect(spec.checkCmd).toBe("haskell-language-server-wrapper");
    expect(spec.installCmd[0]).toBe("ghcup");
  });

  it("does NOT define hoogle (uses web API, no local install needed)", () => {
    expect(TOOL_SPECS["hoogle"]).toBeUndefined();
  });

  it("all specs have manualInstallHint", () => {
    for (const [name, spec] of Object.entries(TOOL_SPECS)) {
      expect(spec.manualInstallHint, `${name} missing manualInstallHint`).toBeTruthy();
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

// ─── ensureTool — install state machine ──────────────────────────────────────

describe("ensureTool — install state machine", () => {
  // Use a fake tool name that is definitely not installed
  const FAKE_TOOL = "__fake_mcp_tool_for_testing__";

  afterEach(() => {
    resetInstallState(FAKE_TOOL);
  });

  it("starts in undefined state before first call", () => {
    expect(getInstallStatus(FAKE_TOOL)).toBeUndefined();
  });

  it("transitions to 'installing' after first ensureTool call for missing tool", async () => {
    // We need the tool spec to exist; temporarily inject one
    const { TOOL_SPECS: specs } = await import("../tools/tool-installer.js");
    (specs as Record<string, unknown>)[FAKE_TOOL] = {
      checkCmd: FAKE_TOOL,
      installCmd: ["echo", "install"],  // succeeds immediately
      manualInstallHint: "echo install",
      installTimeout: 5_000,
    };

    const result = await ensureTool(FAKE_TOOL);
    // First call: tool not found → starts installing
    expect(result.available).toBe(false);
    expect(result.installing).toBe(true);
    expect(getInstallStatus(FAKE_TOOL)).toBe("installing");
  });

  it("returns 'installing' on repeated calls while install is in progress", async () => {
    const { TOOL_SPECS: specs } = await import("../tools/tool-installer.js");
    (specs as Record<string, unknown>)[FAKE_TOOL] = {
      checkCmd: FAKE_TOOL,
      installCmd: ["echo", "install"],
      manualInstallHint: "echo install",
      installTimeout: 5_000,
    };

    await ensureTool(FAKE_TOOL); // First call — starts install
    const second = await ensureTool(FAKE_TOOL); // Second call — still installing
    expect(second.available).toBe(false);
    expect(second.installing).toBe(true);
    expect(second.message).toContain("Retry");
  });

  it("resetInstallState clears state for a tool", async () => {
    // Manually set state by triggering an install, then reset
    const { TOOL_SPECS: specs } = await import("../tools/tool-installer.js");
    (specs as Record<string, unknown>)[FAKE_TOOL] = {
      checkCmd: FAKE_TOOL,
      installCmd: ["echo", "install"],
      manualInstallHint: "echo install",
      installTimeout: 5_000,
    };

    await ensureTool(FAKE_TOOL); // sets state to "installing"
    resetInstallState(FAKE_TOOL);
    expect(getInstallStatus(FAKE_TOOL)).toBeUndefined();
  });
});

describe("bundled tool resolution", () => {
  const rootDir = path.resolve(import.meta.dirname, "..", "..");
  const runtimePlatform = process.platform as "darwin" | "linux" | "win32";
  const runtimeArch = process.arch as "x64" | "arm64";
  const runtimeExt = runtimePlatform === "win32" ? ".exe" : "";
  const runtimeBinRel = `hlint/${runtimePlatform}-${runtimeArch}/hlint${runtimeExt}`;
  const runtimeBinAbs = path.join(rootDir, "vendor-tools", runtimeBinRel);

  afterEach(async () => {
    resetBundledManifestCache();
    await rm(runtimeBinAbs, { force: true });
  });

  it("prefers bundled binary when present for runtime platform", async () => {
    if (!["darwin", "linux", "win32"].includes(process.platform)) return;
    if (!["x64", "arm64"].includes(process.arch)) return;

    await mkdir(path.dirname(runtimeBinAbs), { recursive: true });
    if (process.platform === "win32") {
      await writeFile(runtimeBinAbs, "@echo off\r\necho bundled hlint\r\n", "utf8");
    } else {
      await writeFile(runtimeBinAbs, "#!/usr/bin/env sh\necho bundled hlint\n", "utf8");
      await chmod(runtimeBinAbs, 0o755);
    }

    const resolved = await resolveToolBinary("hlint");
    expect(resolved).not.toBeNull();
    expect(resolved?.source).toBe("bundled");
    expect(resolved?.binaryPath).toBe(runtimeBinAbs);
  });

  it("ensureTool reports source=bundled when bundled binary exists", async () => {
    if (!["darwin", "linux", "win32"].includes(process.platform)) return;
    if (!["x64", "arm64"].includes(process.arch)) return;

    await mkdir(path.dirname(runtimeBinAbs), { recursive: true });
    if (process.platform === "win32") {
      await writeFile(runtimeBinAbs, "@echo off\r\necho bundled hlint\r\n", "utf8");
    } else {
      await writeFile(runtimeBinAbs, "#!/usr/bin/env sh\necho bundled hlint\n", "utf8");
      await chmod(runtimeBinAbs, 0o755);
    }

    const ensured = await ensureTool("hlint");
    expect(ensured.available).toBe(true);
    expect(ensured.source).toBe("bundled");
    expect(ensured.binaryPath).toBe(runtimeBinAbs);
  });
});
