/**
 * Staleness detector — compares the MCP bundle mtime (`dist/index.js`) with
 * the running process's boot time and reports whether the bundle on disk is
 * significantly newer than the code in memory.
 *
 * # Why
 *
 * The MCP process caches `dist/index.js` at startup. When the maintainer
 * edits TS and rebuilds, `dist/` is updated but the running process stays
 * on the old bundle until it is respawned. `mcp_reload_code` is the
 * documented path to fix that, but it was introduced in Fase 6 — any user
 * whose running MCP predates Fase 6 cannot see that tool (dogfood fails).
 *
 * This module is the fallback: a lightweight check that every tool wrapper
 * consults, which lets responses surface a `_warning` field whenever the
 * gap exceeds {@link STALENESS_THRESHOLD_MS}. The user is then prompted to
 * restart Claude Desktop to pick up fresh code — no special tool needed.
 *
 * # Performance / safety
 *
 * - `stat()` hits the filesystem, so the result is cached for
 *   {@link CACHE_TTL_MS}. Under normal tool-call cadence (seconds apart)
 *   this is one stat per minute at worst.
 * - On stat failure (ENOENT / permission) the detector returns
 *   `stale: false` and an `error` string — we never poison the pipeline
 *   with a warning based on a failed probe.
 * - All state is module-local with explicit reset hooks for tests; no
 *   globals leak outside the module.
 */
import { stat } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

/**
 * Threshold beyond which the bundle is considered "stale enough to warn".
 * Two minutes is a compromise: well above the file-system lag of a fresh
 * build (`tsc` finishes in ~3s) so a user who JUST rebuilt doesn't see a
 * flap, but short enough that the warning reliably appears on the next
 * tool call after a rebuild.
 */
export const STALENESS_THRESHOLD_MS = 120_000;

/**
 * How long a staleness check is cached before a re-stat. Tunable: 60s
 * trades 1 stat/minute for ~60s latency on the warning transition.
 */
export const CACHE_TTL_MS = 60_000;

/**
 * Boot time of the current process in ms since epoch. Captured at module
 * import — note this file is imported early (both `index.ts` and
 * `tools/registry.ts` touch it). Subsequent re-imports in tests do not
 * reset it. Use the `_setForTesting` hooks if you need to simulate a
 * specific boot time.
 */
let BOOT_TIME_MS = Date.now();

/** Cached check result. `null` = never probed; otherwise the last stat result. */
interface CachedResult {
  at: number;
  bundleMtimeMs?: number;
  error?: string;
}
let cached: CachedResult | null = null;

/**
 * Default bundle path — the caller of this module usually imports it via
 * `import.meta.url` inside `dist/tools/…`. We walk up to the compiled
 * `dist/index.js` because THAT is the entry point the MCP client re-reads
 * when the process is respawned.
 */
export function defaultBundlePath(importMetaUrl: string): string {
  const here = fileURLToPath(importMetaUrl);
  // Walk up from dist/**/*.js → dist/index.js
  const idx = here.lastIndexOf(`${path.sep}dist${path.sep}`);
  if (idx === -1) return here;
  return path.join(here.slice(0, idx + 5), "index.js");
}

/**
 * A single cached instance keyed by bundlePath. Usually the whole process
 * has one bundle, so we keep one entry; keyed storage is only there so
 * tests can simulate multiple bundles without reset.
 */
const perBundle: Map<string, CachedResult> = new Map();

export interface StalenessResult {
  /** True when the bundle is newer than the boot time by at least
   *  {@link STALENESS_THRESHOLD_MS}. */
  stale: boolean;
  /** Millis the bundle is ahead of boot time. Negative / undefined = not stale. */
  bundleAheadByMs?: number;
  /** Bundle mtime in ms since epoch, if readable. */
  bundleMtimeMs?: number;
  /** Process boot time in ms since epoch. */
  bootTimeMs: number;
  /** stat() error, if the probe itself failed. */
  error?: string;
  /** Whether this response was served from the cache vs a fresh stat. */
  cached: boolean;
}

export interface StalenessDeps {
  bundlePath: string;
  nowMs: () => number;
  bootTimeMs: () => number;
  /** Cache TTL override for tests. */
  cacheTtlMs?: number;
  /** Threshold override for tests. */
  thresholdMs?: number;
}

/**
 * Pure handler — fully dependency-injected for deterministic unit tests.
 */
export async function computeStaleness(deps: StalenessDeps): Promise<StalenessResult> {
  const now = deps.nowMs();
  const ttl = deps.cacheTtlMs ?? CACHE_TTL_MS;
  const threshold = deps.thresholdMs ?? STALENESS_THRESHOLD_MS;
  const bootTimeMs = deps.bootTimeMs();

  const hit = perBundle.get(deps.bundlePath);
  if (hit && now - hit.at < ttl) {
    return buildResult(hit, bootTimeMs, threshold, /* fromCache */ true);
  }

  let bundleMtimeMs: number | undefined;
  let error: string | undefined;
  try {
    const s = await stat(deps.bundlePath);
    bundleMtimeMs = s.mtimeMs;
  } catch (err) {
    error = (err as Error).message;
  }

  const fresh: CachedResult = { at: now, bundleMtimeMs, error };
  perBundle.set(deps.bundlePath, fresh);
  return buildResult(fresh, bootTimeMs, threshold, /* fromCache */ false);
}

function buildResult(
  entry: CachedResult,
  bootTimeMs: number,
  threshold: number,
  fromCache: boolean
): StalenessResult {
  if (entry.error !== undefined || entry.bundleMtimeMs === undefined) {
    return {
      stale: false,
      bootTimeMs,
      error: entry.error,
      cached: fromCache,
    };
  }
  const bundleAheadByMs = entry.bundleMtimeMs - bootTimeMs;
  return {
    stale: bundleAheadByMs >= threshold,
    bundleAheadByMs,
    bundleMtimeMs: entry.bundleMtimeMs,
    bootTimeMs,
    cached: fromCache,
  };
}

/** Human-readable warning message for the `_warning` response field. */
export function stalenessMessage(r: StalenessResult): string | null {
  if (!r.stale || r.bundleAheadByMs === undefined) return null;
  const minutes = Math.round(r.bundleAheadByMs / 60_000);
  return (
    `MCP bundle on disk is ${minutes} minute(s) newer than the running process. ` +
    "Restart Claude Desktop (or call mcp_reload_code if available) so TypeScript edits take effect."
  );
}

/**
 * Optional override for the bundle path used by the default
 * `registerStrictTool` wiring. Set to a fixture path in tests so the
 * middleware inspects the fixture's mtime, not the real dist bundle.
 *
 * This is intentionally a regular module-scope let; production code never
 * touches it. Exported hooks are the only mutation surface.
 */
let bundlePathOverride: string | null = null;

export function activeBundlePath(fallback: string): string {
  return bundlePathOverride ?? fallback;
}

// ───────────────────────────── test hooks ─────────────────────────────
export function _resetStalenessCacheForTests(): void {
  perBundle.clear();
  cached = null;
}

export function _setBootTimeForTests(ms: number): void {
  BOOT_TIME_MS = ms;
}

export function _setBundlePathOverrideForTests(p: string | null): void {
  bundlePathOverride = p;
}

export function bootTimeMs(): number {
  return BOOT_TIME_MS;
}
