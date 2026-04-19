/**
 * Auto-download system for bundled tools from GitHub Releases.
 * Downloads tools on-demand the first time they're needed.
 * Subsequent calls use the cached binary in vendor-tools/.
 *
 * Source of truth for tool versions + release URLs is
 * `vendor-tools/bundled-tools-manifest.json` (manifestVersion >= 2), accessed
 * via `src/vendor-tools/manifest.ts`. This module used to hold the URL matrix
 * inline; centralizing it prevents drift between manifest and runtime.
 */
import { mkdir, writeFile, chmod, access, readFile, unlink } from "node:fs/promises";
import { createWriteStream } from "node:fs";
import { createHash } from "node:crypto";
import path from "node:path";
import { fileURLToPath } from "node:url";

import type {
  PlatformTarget,
  ReleaseEntry,
  SupportedArch,
  SupportedPlatform,
  SupportedTool,
} from "../vendor-tools/manifest.js";
import {
  enumerateConfiguredReleases,
  getReleaseEntry,
} from "../vendor-tools/manifest.js";

export type { SupportedTool, SupportedPlatform, SupportedArch } from "../vendor-tools/manifest.js";

const ROOT_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");
const VENDOR_TOOLS_DIR = path.join(ROOT_DIR, "vendor-tools");

const SHA256_PATTERN = /^[a-f0-9]{64}$/i;

/**
 * Maximum time a single download attempt may take. Tuned for the largest
 * asset we ship (HLS, ~180MB) on a typical developer connection. Much
 * shorter than the agent-side MCP timeout so failures are local and
 * cancellable instead of bubbling up as opaque hangs.
 */
const DOWNLOAD_TIMEOUT_MS = 5 * 60 * 1000; // 5 minutes

/**
 * Per-URL concurrency guard: we remember in-flight downloads (keyed by the
 * final `binaryPath`) and make concurrent callers await the same promise
 * instead of racing to write the same file. Replaces what would otherwise
 * be corrupted partial binaries under parallel tool calls.
 */
const IN_FLIGHT: Map<string, Promise<void>> = new Map();

function hasVerifiableChecksum(entry: Pick<ReleaseEntry, "sha256">): boolean {
  if (!entry.sha256) return false;
  return SHA256_PATTERN.test(entry.sha256.trim());
}

async function computeSHA256(filePath: string): Promise<string> {
  const content = await readFile(filePath);
  return createHash("sha256").update(content).digest("hex");
}

async function downloadFile(url: string, destPath: string): Promise<void> {
  const controller = new AbortController();
  const timeoutHandle = setTimeout(() => controller.abort(), DOWNLOAD_TIMEOUT_MS);

  try {
    const response = await fetch(url, { signal: controller.signal });
    if (!response.ok) {
      throw new Error(`Download failed: ${response.status} ${response.statusText}`);
    }
    if (!response.body) {
      throw new Error("Response body is null");
    }

    await mkdir(path.dirname(destPath), { recursive: true });
    const fileStream = createWriteStream(destPath);

    try {
      const reader = response.body.getReader();
      // Reading respects controller.signal — an abort will surface as an
      // error thrown from reader.read(), which we translate to a clear
      // timeout message below.
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;
        fileStream.write(value);
      }

      await new Promise<void>((resolve, reject) => {
        fileStream.end(() => resolve());
        fileStream.on("error", reject);
      });
    } catch (streamErr) {
      // Ensure the file descriptor is released before bubbling the error.
      fileStream.destroy();
      throw streamErr;
    }
  } catch (err) {
    if ((err as Error).name === "AbortError") {
      throw new Error(
        `Download timed out after ${DOWNLOAD_TIMEOUT_MS / 1000}s: ${url}`
      );
    }
    throw err;
  } finally {
    clearTimeout(timeoutHandle);
  }
}

/**
 * Serializes `downloadFile` calls against the same destination path. Two
 * callers racing on the same binary see the same underlying promise; once
 * it resolves (or rejects), the slot is freed so later retries behave as
 * fresh downloads.
 */
async function downloadFileExclusive(url: string, destPath: string): Promise<void> {
  const inFlight = IN_FLIGHT.get(destPath);
  if (inFlight) {
    return inFlight;
  }
  const promise = downloadFile(url, destPath).finally(() => {
    IN_FLIGHT.delete(destPath);
  });
  IN_FLIGHT.set(destPath, promise);
  return promise;
}

export interface AutoDownloadResult {
  success: boolean;
  binaryPath?: string;
  version?: string;
  downloaded?: boolean;
  cached?: boolean;
  checksumVerified?: boolean;
  checksumState?: "verified" | "missing";
  error?: string;
  message: string;
}

/**
 * Auto-download a tool if not already present.
 * Returns the path to the binary (either cached or freshly downloaded).
 */
export async function autoDownloadTool(tool: SupportedTool): Promise<AutoDownloadResult> {
  const platform = process.platform as SupportedPlatform;
  const arch = process.arch as SupportedArch;
  const target = `${platform}-${arch}` as PlatformTarget;

  const lookup = await getReleaseEntry(tool, target);
  if (!lookup) {
    return {
      success: false,
      error: `No release available for ${tool} on ${platform}-${arch}`,
      message: `${tool} is not available for your platform (${platform}-${arch}). Please install manually.`,
    };
  }
  const { entry: release, binaryName } = lookup;

  // Check if already downloaded
  const toolDir = path.join(VENDOR_TOOLS_DIR, tool, target);
  const binaryPath = path.join(toolDir, binaryName);

  try {
    await access(binaryPath);
    // Binary exists - verify it's executable
    try {
      await chmod(binaryPath, 0o755);
    } catch {
      // Already executable or can't change permissions
    }

    const verifyCached = hasVerifiableChecksum(release);
    if (verifyCached) {
      const actualSHA = await computeSHA256(binaryPath);
      if (actualSHA !== release.sha256!.trim()) {
        throw new Error(
          `Cached binary checksum mismatch: expected ${release.sha256}, got ${actualSHA}`
        );
      }
    }

    return {
      success: true,
      binaryPath,
      version: release.version,
      cached: true,
      checksumVerified: verifyCached,
      checksumState: verifyCached ? "verified" : "missing",
      message: verifyCached
        ? `Using cached ${tool} ${release.version} (checksum verified)`
        : `Using cached ${tool} ${release.version} (checksum metadata missing)`,
    };
  } catch {
    // Binary doesn't exist - download it
  }

  // Download the binary — tries the primary URL first, then falls back to
  // an upstream URL if configured. Each attempt runs its own checksum check.
  await mkdir(toolDir, { recursive: true });
  const tempPath = `${binaryPath}.download`;

  const attempts: Array<{
    label: "primary" | "fallback";
    url: string;
    sha256: string | undefined;
  }> = [{ label: "primary", url: release.url, sha256: release.sha256 }];
  if (release.fallbackUrl) {
    attempts.push({
      label: "fallback",
      url: release.fallbackUrl,
      sha256: release.fallbackSha256,
    });
  }

  const attemptErrors: Array<{ label: string; url: string; error: string }> = [];

  for (const attempt of attempts) {
    try {
      await downloadFileExclusive(attempt.url, tempPath);
      await chmod(tempPath, 0o755);

      const hasChecksum = !!(attempt.sha256 && SHA256_PATTERN.test(attempt.sha256.trim()));
      if (hasChecksum) {
        const actualSHA = await computeSHA256(tempPath);
        if (actualSHA !== attempt.sha256!.trim()) {
          throw new Error(
            `Checksum mismatch (${attempt.label}): expected ${attempt.sha256}, got ${actualSHA}`
          );
        }
      }

      await writeFile(binaryPath, await readFile(tempPath));
      await chmod(binaryPath, 0o755);

      return {
        success: true,
        binaryPath,
        version: release.version,
        downloaded: true,
        checksumVerified: hasChecksum,
        checksumState: hasChecksum ? "verified" : "missing",
        message: hasChecksum
          ? `Downloaded ${tool} ${release.version} via ${attempt.label} (checksum verified)`
          : `Downloaded ${tool} ${release.version} via ${attempt.label} (checksum metadata missing)`,
      };
    } catch (error) {
      attemptErrors.push({
        label: attempt.label,
        url: attempt.url,
        error: error instanceof Error ? error.message : String(error),
      });
      // Clean up the partial .download so the next attempt (or a retry in a
      // future session) sees a clean slate instead of reusing a potentially
      // corrupt file. Swallow unlink errors — if the file is already gone we
      // are happy; if unlink fails we still want to record the download
      // error that triggered this branch.
      try { await unlink(tempPath); } catch { /* no-op */ }
    }
  }

  // Defensive: if all attempts fell through without returning, also make
  // sure the tempPath is gone before we propagate the error up.
  try { await unlink(tempPath); } catch { /* no-op */ }

  const summary = attemptErrors
    .map((e) => `${e.label}(${e.url}): ${e.error}`)
    .join(" | ");
  return {
    success: false,
    error: summary,
    message: `Failed to download ${tool} — all ${attempts.length} attempt(s) failed: ${summary}`,
  };
}

/**
 * Check if a tool can be auto-downloaded for the current platform.
 * Async because it consults the cached manifest.
 */
export async function canAutoDownload(tool: SupportedTool): Promise<boolean> {
  const platform = process.platform as SupportedPlatform;
  const arch = process.arch as SupportedArch;
  const target = `${platform}-${arch}` as PlatformTarget;
  return (await getReleaseEntry(tool, target)) !== undefined;
}

export interface ToolchainTupleStatus {
  tool: SupportedTool;
  target: PlatformTarget;
  autoDownloadConfigured: boolean;
  checksumConfigured: boolean;
  url?: string;
  version?: string;
  /**
   * Agent-readable explanation when the target is not configured. Present
   * only when `autoDownloadConfigured: false`. Tells the caller exactly
   * what fallback the MCP will use and how to get the tool manually.
   */
  note?: string;
}

/**
 * Produce the full cross-platform diagnostic matrix (4 tools × 6 targets).
 * Targets with no manifest entry appear as `autoDownloadConfigured: false`
 * with a `note` explaining the fallback path, so callers don't have to
 * guess whether a missing entry is a bug or an honest "not supported yet".
 */
export async function getToolchainTupleMatrix(): Promise<ToolchainTupleStatus[]> {
  const targets: PlatformTarget[] = [
    "darwin-arm64",
    "darwin-x64",
    "linux-arm64",
    "linux-x64",
    "win32-arm64",
    "win32-x64",
  ];
  const tools: SupportedTool[] = ["hlint", "fourmolu", "ormolu", "hls"];
  const rows: ToolchainTupleStatus[] = [];

  const configured = await enumerateConfiguredReleases();
  const index = new Map<string, (typeof configured)[number]>();
  for (const c of configured) index.set(`${c.tool}:${c.target}`, c);

  for (const tool of tools) {
    for (const target of targets) {
      const entry = index.get(`${tool}:${target}`);
      const row: ToolchainTupleStatus = {
        tool,
        target,
        autoDownloadConfigured: entry !== undefined,
        checksumConfigured: entry ? hasVerifiableChecksum(entry.entry) : false,
        url: entry?.entry.url,
        version: entry?.entry.version,
      };
      if (!row.autoDownloadConfigured) {
        // Agents reading this diagnostic need to know the fallback story.
        // `darwin-arm64` is the primary dev target so anything missing there
        // is a real bug; other targets genuinely aren't built yet.
        row.note =
          target === "darwin-arm64"
            ? `Unexpected: ${tool} has no manifest entry for the primary dev target. File an issue.`
            : `Not yet built for ${target}. ${tool} is resolved from host PATH on this platform — install via ghcup/brew/apt.`;
      }
      rows.push(row);
    }
  }
  return rows;
}
