import { execFile } from "node:child_process";
import path from "node:path";
import { access, readFile } from "node:fs/promises";
import { constants as fsConstants } from "node:fs";
import { createHash } from "node:crypto";
import { fileURLToPath } from "node:url";
import { autoDownloadTool, canAutoDownload } from "./auto-download.js";

const GHCUP_BIN = path.join(process.env.HOME ?? "/Users", ".ghcup", "bin");
const CABAL_BIN = path.join(process.env.HOME ?? "/Users", ".cabal", "bin");
export const TOOL_PATH = `${GHCUP_BIN}:${CABAL_BIN}:${process.env.PATH}`;
const ROOT_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");
const BUNDLED_MANIFEST_PATH = path.join(ROOT_DIR, "vendor-tools", "bundled-tools-manifest.json");

let bundledManifestCache: BundledToolsManifest | null = null;

type SupportedPlatform = "darwin" | "linux" | "win32";
type SupportedArch = "x64" | "arm64";

export interface BundledToolEntry {
  tool: string;
  version: string;
  platform: SupportedPlatform;
  arch: SupportedArch;
  filename: string;
  sha256?: string;
  provenance?: string;
}

export interface BundledToolsManifest {
  manifestVersion: number;
  updatedAt: string;
  tools: BundledToolEntry[];
}

const SHA256_PATTERN = /^[a-f0-9]{64}$/i;

export interface ToolSpec {
  /** Binary name used to check availability with `which`. */
  checkCmd: string;
}

export type BundledToolFailureReason =
  | "unsupported-platform"
  | "manifest-missing"
  | "entry-missing"
  | "binary-missing"
  | "not-executable"
  | "checksum-missing"
  | "checksum-mismatch";

export interface BundledToolStatus {
  available: boolean;
  source: "bundled";
  reason?: BundledToolFailureReason;
  message: string;
  binaryPath?: string;
  version?: string;
  provenance?: string;
  checksumVerified?: boolean;
}

/**
 * Tool registry: the set of optional tools the MCP can auto-install.
 * hoogle is intentionally absent — it uses the Hoogle web API.
 */
export const TOOL_SPECS: Record<string, ToolSpec> = {
  hlint: {
    checkCmd: "hlint",
  },
  fourmolu: {
    checkCmd: "fourmolu",
  },
  ormolu: {
    checkCmd: "ormolu",
  },
  hls: {
    checkCmd: "haskell-language-server-wrapper",
  },
};

// ─── Public API ───────────────────────────────────────────────────────────────

/** Check whether a tool binary exists in the MCP PATH. */
export async function toolAvailable(toolName: string): Promise<boolean> {
  const resolved = await resolveToolBinary(toolName);
  return resolved !== null;
}

export interface EnsureResult {
  /** True only when the tool is confirmed available right now. */
  available: boolean;
  installing?: boolean;
  justInstalled?: boolean;
  failed?: boolean;
  error?: string;
  source?: "bundled" | "host" | "installed";
  binaryPath?: string;
  version?: string;
  provenance?: string;
  checksumVerified?: boolean;
  bundledReason?: BundledToolFailureReason;
  /** Human-readable status message suitable for including in a tool response. */
  message: string;
}

export interface ResolvedToolBinary {
  source: "bundled" | "host" | "installed";
  binaryPath: string;
  version?: string;
}

export async function ensureTool(toolName: string): Promise<EnsureResult> {
  // Step 1: Check host PATH
  const hostBinary = await locateHostBinary(toolName);
  if (hostBinary) {
    return {
      available: true,
      source: "host",
      binaryPath: hostBinary.binaryPath,
      message: `${toolName} is available from host PATH`,
    };
  }

  // Step 2: Check existing bundled binary
  const bundled = await getBundledToolStatus(toolName);
  if (bundled.available) {
    return {
      available: true,
      source: "bundled",
      binaryPath: bundled.binaryPath,
      version: bundled.version,
      provenance: bundled.provenance,
      checksumVerified: bundled.checksumVerified,
      message: bundled.message,
    };
  }

  // Step 3: Try auto-download if supported
  if (await canAutoDownload(toolName as "hlint" | "fourmolu" | "ormolu" | "hls")) {
    const downloadResult = await autoDownloadTool(toolName as "hlint" | "fourmolu" | "ormolu" | "hls");
    if (downloadResult.success) {
      return {
        available: true,
        source: "installed",
        binaryPath: downloadResult.binaryPath,
        version: downloadResult.version,
        checksumVerified: downloadResult.checksumVerified,
        justInstalled: downloadResult.downloaded,
        message: downloadResult.message,
      };
    }
    // Download failed - include error in message
    return {
      available: false,
      error: downloadResult.error,
      message: `${bundled.message}. Auto-download failed: ${downloadResult.error}`,
    };
  }

  // Step 4: Not available
  return {
    available: false,
    bundledReason: bundled.reason,
    message: bundled.message,
  };
}

/** Reset install state for a tool (useful for testing or manual retry). */
export function resetInstallState(toolName: string): void {
  void toolName;
}

/** Expose state for testing. */
export function getInstallStatus(toolName: string): undefined {
  void toolName;
  return undefined;
}

export async function resolveToolBinary(toolName: string): Promise<ResolvedToolBinary | null> {
  const host = await locateHostBinary(toolName);
  if (host) return host;
  const bundled = await getBundledToolStatus(toolName);
  if (!bundled.available || !bundled.binaryPath) return null;
  return {
    source: "bundled",
    binaryPath: bundled.binaryPath,
    version: bundled.version,
  };
}

export function resetBundledManifestCache(): void {
  bundledManifestCache = null;
}

export async function getBundledToolStatus(toolName: string): Promise<BundledToolStatus> {
  const runtime = getRuntimePlatformArch();
  if (!runtime) {
    return {
      available: false,
      source: "bundled",
      reason: "unsupported-platform",
      message:
        `${toolName} is not available from the bundled toolchain on this platform ` +
        `(${process.platform}-${process.arch}). Supported runtime tuples are darwin/linux/win32 x x64/arm64.`,
    };
  }

  const manifest = await loadBundledManifest();
  if (!manifest) {
    return {
      available: false,
      source: "bundled",
      reason: "manifest-missing",
      message: `${toolName} is not available because the bundled tools manifest is missing or unreadable.`,
    };
  }

  const entry = manifest.tools.find(
    (item) =>
      item.tool === toolName &&
      item.platform === runtime.platform &&
      item.arch === runtime.arch
  );
  if (!entry) {
    return {
      available: false,
      source: "bundled",
      reason: "entry-missing",
      message:
        `${toolName} has no bundled entry for ${runtime.platform}-${runtime.arch}. ` +
        "Use a host installation or add a bundled artifact for this runtime.",
    };
  }

  const absolute = path.resolve(ROOT_DIR, "vendor-tools", entry.filename);
  if (!(await fileExists(absolute))) {
    return {
      available: false,
      source: "bundled",
      reason: "binary-missing",
      message:
        `${toolName} has a manifest entry for ${runtime.platform}-${runtime.arch}, ` +
        `but the bundled binary is missing at ${entry.filename}.`,
      version: entry.version,
      provenance: entry.provenance,
    };
  }

  if (!(await verifyExecutable(absolute))) {
    return {
      available: false,
      source: "bundled",
      reason: "not-executable",
      message:
        `${toolName} exists at ${entry.filename}, but it is not executable on this platform.`,
      binaryPath: absolute,
      version: entry.version,
      provenance: entry.provenance,
    };
  }

  const checksum = entry.sha256?.trim();
  if (!checksum || !SHA256_PATTERN.test(checksum)) {
    return {
      available: false,
      source: "bundled",
      reason: "checksum-missing",
      message:
        `${toolName} exists at ${entry.filename}, but the bundled manifest entry has no sha256. ` +
        "Populate version/provenance/sha256 before treating this bundle as production-ready.",
      binaryPath: absolute,
      version: entry.version,
      provenance: entry.provenance,
    };
  }

  const digest = await sha256File(absolute);
  if (digest !== checksum) {
    return {
      available: false,
      source: "bundled",
      reason: "checksum-mismatch",
      message:
        `${toolName} failed bundled checksum verification for ${entry.filename}. ` +
        `Expected ${checksum}, got ${digest}.`,
      binaryPath: absolute,
      version: entry.version,
      provenance: entry.provenance,
    };
  }

  return {
    available: true,
    source: "bundled",
    binaryPath: absolute,
    version: entry.version,
    provenance: entry.provenance,
    checksumVerified: true,
    message: `${toolName} available via bundled toolchain (${entry.version ?? "unknown version"})`,
  };
}

// ─── Private helpers ──────────────────────────────────────────────────────────

function getRuntimePlatformArch():
  | { platform: SupportedPlatform; arch: SupportedArch }
  | null {
  const platform = process.platform;
  const arch = process.arch;
  if (
    (platform === "darwin" || platform === "linux" || platform === "win32") &&
    (arch === "x64" || arch === "arm64")
  ) {
    return { platform, arch };
  }
  return null;
}

async function loadBundledManifest(): Promise<BundledToolsManifest | null> {
  if (bundledManifestCache) return bundledManifestCache;
  try {
    const raw = await readFile(BUNDLED_MANIFEST_PATH, "utf8");
    bundledManifestCache = JSON.parse(raw) as BundledToolsManifest;
    return bundledManifestCache;
  } catch {
    return null;
  }
}

async function fileExists(filePath: string): Promise<boolean> {
  try {
    await access(filePath, fsConstants.F_OK);
    return true;
  } catch {
    return false;
  }
}

async function verifyExecutable(filePath: string): Promise<boolean> {
  if (process.platform === "win32") return true;
  try {
    await access(filePath, fsConstants.X_OK);
    return true;
  } catch {
    return false;
  }
}

async function sha256File(filePath: string): Promise<string> {
  const raw = await readFile(filePath);
  return createHash("sha256").update(raw).digest("hex");
}

async function locateHostBinary(toolName: string): Promise<ResolvedToolBinary | null> {
  const checkCmd = TOOL_SPECS[toolName]?.checkCmd ?? toolName;
  const probe = process.platform === "win32" ? "where" : "which";

  return new Promise((resolve) => {
    execFile(
      probe,
      [checkCmd],
      { env: { ...process.env, PATH: TOOL_PATH } },
      (err, stdout) => {
        if (err) {
          resolve(null);
          return;
        }
        const first = stdout.split(/\r?\n/).map((l) => l.trim()).find(Boolean);
        if (!first) {
          resolve(null);
          return;
        }
        resolve({
          source: "host",
          binaryPath: first,
        });
      }
    );
  });
}

