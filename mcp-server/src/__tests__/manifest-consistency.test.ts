/**
 * Invariant regression guard for the bundled-tools manifest.
 *
 * The manifest has TWO places where a tool's sha256 can be written:
 *
 *   1. `releases[tool].platforms[target].sha256` — the hash of the file
 *      the auto-downloader will fetch from GitHub Releases.
 *   2. `tools[].sha256` (where `filename` matches the vendor-tools path)
 *      — the hash bundled binary verification checks against when the
 *      file already exists on disk under `vendor-tools/`.
 *
 * The two entries describe the SAME binary — they MUST agree, or
 * `getBundledToolStatus` returns `checksum-mismatch` even when the disk
 * content is identical to the configured release. A silent drift between
 * these fields was observed in session 4 (fourmolu darwin-arm64) and
 * caused confusing "bundledReason: checksum-mismatch" messages in
 * `ghci_toolchain_status` output.
 *
 * This test fails loudly if anyone edits one hash without the other.
 */
import { describe, it, expect } from "vitest";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import path from "node:path";

interface PlatformEntry {
  url?: string;
  sha256?: string;
  version?: string;
}
interface ReleaseToolSpec {
  binaryName: string;
  platforms: Record<string, PlatformEntry>;
}
interface ToolsEntry {
  tool: string;
  version?: string;
  platform: string;
  arch: string;
  filename: string;
  sha256?: string;
  provenance?: string;
}
interface Manifest {
  manifestVersion: number;
  releases: Record<string, ReleaseToolSpec>;
  tools: ToolsEntry[];
}

async function loadManifest(): Promise<Manifest> {
  const here = fileURLToPath(import.meta.url);
  // __tests__/ → src/ → mcp-server/ → vendor-tools/
  const mcpRoot = path.resolve(path.dirname(here), "..", "..");
  const raw = await readFile(
    path.join(mcpRoot, "vendor-tools", "bundled-tools-manifest.json"),
    "utf-8"
  );
  return JSON.parse(raw) as Manifest;
}

function buildTargetKey(platform: string, arch: string): string {
  return `${platform}-${arch}`;
}

describe("bundled-tools-manifest consistency", () => {
  it("every tools[] entry whose release has a sha256 matches it", async () => {
    const m = await loadManifest();
    const drift: string[] = [];

    for (const entry of m.tools) {
      const release = m.releases[entry.tool];
      if (!release) continue; // tool not configured for release — OK
      const target = buildTargetKey(entry.platform, entry.arch);
      const platformEntry = release.platforms[target];
      if (!platformEntry || !platformEntry.sha256) continue; // no configured hash on release side — OK
      if (!entry.sha256) continue; // no bundled hash to compare — OK (will be re-computed)

      if (platformEntry.sha256.toLowerCase().trim() !== entry.sha256.toLowerCase().trim()) {
        drift.push(
          `${entry.tool} ${target}: releases=${platformEntry.sha256.slice(0, 12)}… ` +
          `vs tools[].sha256=${entry.sha256.slice(0, 12)}…`
        );
      }
    }

    if (drift.length > 0) {
      throw new Error(
        "Hash drift between `releases[tool].platforms[target].sha256` and " +
          "`tools[].sha256` for the same binary:\n  " +
          drift.join("\n  ") +
          "\n\nBoth places describe the SAME file; edit them together."
      );
    }
  });

  it("PENDING_CHECKSUM_* sentinels only appear in uncontrolled platforms", async () => {
    const m = await loadManifest();
    const suspects: string[] = [];
    for (const entry of m.tools) {
      const s = entry.sha256 ?? "";
      if (s.includes("PENDING_CHECKSUM")) {
        // These are allowed for platforms we haven't built yet.
        // Just pin the invariant that when the provenance advertises
        // "auto-download-ready" for the darwin-arm64 dev platform, the
        // checksum is NOT a placeholder.
        const isDevTarget = entry.platform === "darwin" && entry.arch === "arm64";
        const advertisedReady = entry.provenance === "auto-download-ready";
        if (isDevTarget && advertisedReady) {
          suspects.push(
            `${entry.tool} ${entry.platform}-${entry.arch} advertises ` +
              `provenance=${entry.provenance} with placeholder sha256`
          );
        }
      }
    }
    expect(suspects).toEqual([]);
  });

  it("every tools[] entry references a filename shape that matches the binaryName", async () => {
    const m = await loadManifest();
    const mismatches: string[] = [];
    for (const entry of m.tools) {
      const release = m.releases[entry.tool];
      if (!release) continue;
      const expectedSuffix = `/${release.binaryName}`;
      const looksLikeWindows = entry.filename.endsWith(".exe");
      const okSuffix = looksLikeWindows
        ? entry.filename.endsWith(`${release.binaryName}.exe`)
        : entry.filename.endsWith(expectedSuffix);
      if (!okSuffix) {
        mismatches.push(
          `${entry.tool} ${entry.platform}-${entry.arch}: ` +
            `filename=${entry.filename} but binaryName=${release.binaryName}`
        );
      }
    }
    expect(mismatches).toEqual([]);
  });
});
