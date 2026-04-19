/**
 * Single source of truth for bundled tools metadata.
 * Reads vendor-tools/bundled-tools-manifest.json (manifestVersion 2+) once,
 * caches the parsed object, and exposes typed accessors for:
 *
 *   • `releases`  — per-tool × per-platform URLs used by auto-download
 *   • `tools`     — the legacy bundled-files list (provenance, sha256, path)
 *
 * Prior to manifestVersion 2 the release URL matrix lived hardcoded in
 * `src/tools/auto-download.ts`. That duplication caused drift (one file could
 * be updated without the other). The loader here is forward-compatible: when a
 * v1 manifest without `releases` is read, callers get an empty releases map
 * and auto-download degrades gracefully rather than throwing.
 */
import { readFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

export type SupportedTool = "hlint" | "fourmolu" | "ormolu" | "hls";
export type SupportedPlatform = "darwin" | "linux" | "win32";
export type SupportedArch = "x64" | "arm64";
export type PlatformTarget = `${SupportedPlatform}-${SupportedArch}`;

export interface ReleaseEntry {
  version: string;
  url: string;
  /** Hex-encoded SHA256. Optional — absent means "download without verification". */
  sha256?: string;
  /** Optional upstream fallback URL tried when primary fails. */
  fallbackUrl?: string;
  fallbackSha256?: string;
}

export interface ReleaseToolSpec {
  binaryName: string;
  platforms: Partial<Record<PlatformTarget, ReleaseEntry>>;
}

export interface BundledEntry {
  tool: SupportedTool;
  version: string;
  platform: SupportedPlatform;
  arch: SupportedArch;
  filename: string;
  sha256: string;
  provenance: "local-shim" | "auto-download-ready" | "placeholder" | string;
}

export interface BundledToolsManifest {
  manifestVersion: number;
  updatedAt: string;
  releases: Record<SupportedTool, ReleaseToolSpec>;
  tools: BundledEntry[];
}

const ROOT_DIR = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "..",
  ".."
);
const MANIFEST_PATH = path.join(
  ROOT_DIR,
  "vendor-tools",
  "bundled-tools-manifest.json"
);

let cached: BundledToolsManifest | null = null;

/**
 * For tests: drop the cached manifest so the next load reads fresh from disk.
 */
export function resetManifestCache(): void {
  cached = null;
}

/**
 * Override the default manifest location. Primarily for unit tests that
 * want to point the loader at a fixture JSON without copying it over the real
 * file. Passing `null` restores the default.
 */
let manifestPathOverride: string | null = null;
export function setManifestPathForTests(p: string | null): void {
  manifestPathOverride = p;
  cached = null;
}

/**
 * Resolution priority:
 *   1. `setManifestPathForTests(p)` — unit tests that want to override in-process
 *   2. `HASKELL_FLOWS_MANIFEST_PATH` env var — e2e tests that spawn the MCP
 *      as a subprocess and cannot use the in-process setter. Also useful for
 *      operators who want to pin a known-good manifest in CI.
 *   3. Default co-located `vendor-tools/bundled-tools-manifest.json`.
 */
function activeManifestPath(): string {
  if (manifestPathOverride) return manifestPathOverride;
  const envOverride = process.env.HASKELL_FLOWS_MANIFEST_PATH;
  if (envOverride && envOverride.trim().length > 0) return envOverride;
  return MANIFEST_PATH;
}

const SUPPORTED_TOOLS: readonly SupportedTool[] = [
  "hlint",
  "fourmolu",
  "ormolu",
  "hls",
] as const;

const EMPTY_RELEASES = (): Record<SupportedTool, ReleaseToolSpec> => ({
  hlint: { binaryName: "hlint", platforms: {} },
  fourmolu: { binaryName: "fourmolu", platforms: {} },
  ormolu: { binaryName: "ormolu", platforms: {} },
  hls: { binaryName: "haskell-language-server-wrapper", platforms: {} },
});

function isReleaseEntry(value: unknown): value is ReleaseEntry {
  if (typeof value !== "object" || value === null) return false;
  const v = value as Record<string, unknown>;
  return typeof v.version === "string" && typeof v.url === "string";
}

function isReleaseToolSpec(value: unknown): value is ReleaseToolSpec {
  if (typeof value !== "object" || value === null) return false;
  const v = value as Record<string, unknown>;
  if (typeof v.binaryName !== "string") return false;
  if (typeof v.platforms !== "object" || v.platforms === null) return false;
  for (const entry of Object.values(v.platforms)) {
    if (!isReleaseEntry(entry)) return false;
  }
  return true;
}

/**
 * Coerce loosely-typed JSON into `BundledToolsManifest`. Missing or malformed
 * sections degrade to empty equivalents rather than throwing, so old
 * manifests and partial manifests still produce usable output.
 */
function normalize(raw: unknown): BundledToolsManifest {
  const manifest: Partial<BundledToolsManifest> =
    typeof raw === "object" && raw !== null ? (raw as Partial<BundledToolsManifest>) : {};

  const releases = EMPTY_RELEASES();
  const rawReleases = (raw as { releases?: unknown })?.releases;
  if (typeof rawReleases === "object" && rawReleases !== null) {
    for (const tool of SUPPORTED_TOOLS) {
      const spec = (rawReleases as Record<string, unknown>)[tool];
      if (isReleaseToolSpec(spec)) {
        releases[tool] = spec;
      }
    }
  }

  const tools = Array.isArray(manifest.tools) ? (manifest.tools as BundledEntry[]) : [];

  return {
    manifestVersion:
      typeof manifest.manifestVersion === "number" ? manifest.manifestVersion : 1,
    updatedAt:
      typeof manifest.updatedAt === "string" ? manifest.updatedAt : "",
    releases,
    tools,
  };
}

export async function loadManifest(): Promise<BundledToolsManifest> {
  if (cached !== null) return cached;
  try {
    const text = await readFile(activeManifestPath(), "utf-8");
    const raw = JSON.parse(text) as unknown;
    cached = normalize(raw);
  } catch (err) {
    // File missing or invalid JSON → empty manifest. Callers already handle
    // "no release for platform" gracefully.
    cached = {
      manifestVersion: 0,
      updatedAt: "",
      releases: EMPTY_RELEASES(),
      tools: [],
    };
    // Surface parse errors in dev; missing file is fine in some test harnesses.
    if (err instanceof SyntaxError) {
      // eslint-disable-next-line no-console
      console.error(`[vendor-tools/manifest] failed to parse JSON: ${err.message}`);
    }
  }
  return cached;
}

/**
 * Get the release entry for a specific tool/platform, or undefined if not
 * configured. Reads the (cached) manifest under the hood.
 */
export async function getReleaseEntry(
  tool: SupportedTool,
  target: PlatformTarget
): Promise<{ entry: ReleaseEntry; binaryName: string } | undefined> {
  const manifest = await loadManifest();
  const spec = manifest.releases[tool];
  const entry = spec?.platforms?.[target];
  if (!entry) return undefined;
  return { entry, binaryName: spec.binaryName };
}

/**
 * Snapshot of configured platform targets across all tools — useful for CI
 * validation and the toolchain-status response matrix.
 */
export async function enumerateConfiguredReleases(): Promise<
  Array<{
    tool: SupportedTool;
    target: PlatformTarget;
    entry: ReleaseEntry;
    binaryName: string;
  }>
> {
  const manifest = await loadManifest();
  const out: Array<{
    tool: SupportedTool;
    target: PlatformTarget;
    entry: ReleaseEntry;
    binaryName: string;
  }> = [];
  for (const tool of SUPPORTED_TOOLS) {
    const spec = manifest.releases[tool];
    for (const [target, entry] of Object.entries(spec.platforms) as Array<
      [PlatformTarget, ReleaseEntry]
    >) {
      out.push({ tool, target, entry, binaryName: spec.binaryName });
    }
  }
  return out;
}
