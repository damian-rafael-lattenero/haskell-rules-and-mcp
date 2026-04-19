/**
 * Unit coverage for `mcp_reload_code` (Phase 6-a).
 *
 * The handler exposes every side effect as an injected dependency so we
 * can exercise every branch — staleness gate, rate-limit gate, dry-run,
 * confirmed exit scheduling — without actually killing the process.
 *
 * Security-relevant properties pinned by these tests:
 *   • CWE-400 (restart loop DoS): rate-limit rejects calls inside the window.
 *   • CWE-400 (no-op restart): refuses to exit when bundle is not newer
 *     than boot time.
 *   • CWE-754 (error handling): missing bundle returns failure instead of
 *     crashing.
 *   • CWE-20 (input validation): confirm defaults false → dry-run safe.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { mkdtemp, writeFile, rm, utimes } from "node:fs/promises";
import { tmpdir } from "node:os";
import path from "node:path";

import {
  handleReloadCode,
  _resetRateLimitForTesting,
  type ReloadDeps,
} from "../tools/mcp-reload-code.js";

/**
 * Build a dependency bag whose defaults are safe for tests (never exits,
 * never schedules real timers). Overrides let a test customize individual
 * pieces.
 */
function makeDeps(overrides: Partial<ReloadDeps> = {}): ReloadDeps & {
  exitCalls: number[];
  scheduleCalls: Array<{ fn: () => void; ms: number }>;
} {
  const exitCalls: number[] = [];
  const scheduleCalls: Array<{ fn: () => void; ms: number }> = [];
  const base: ReloadDeps = {
    bundlePath: "/non-existent/path/that/will/be/overridden.js",
    exitFn: (code: number) => exitCalls.push(code),
    scheduleFn: (fn, ms) => scheduleCalls.push({ fn, ms }),
    nowMs: () => 1_700_000_000_000,
    bootTimeMs: () => 1_700_000_000_000 - 60_000, // 60s ago by default
  };
  return Object.assign({}, base, overrides, { exitCalls, scheduleCalls });
}

async function makeStaleBundle(ageMs: number): Promise<string> {
  const dir = await mkdtemp(path.join(tmpdir(), "reload-code-"));
  const file = path.join(dir, "index.js");
  await writeFile(file, "// bundle\n", "utf-8");
  const mtime = new Date(Date.now() - ageMs);
  await utimes(file, mtime, mtime);
  return file;
}

async function makeFreshBundle(): Promise<string> {
  const dir = await mkdtemp(path.join(tmpdir(), "reload-code-"));
  const file = path.join(dir, "index.js");
  await writeFile(file, "// bundle\n", "utf-8");
  // mtime defaults to now — fresher than any sensible bootTime.
  return file;
}

describe("handleReloadCode (P6-a)", () => {
  beforeEach(() => {
    _resetRateLimitForTesting();
  });
  afterEach(async () => {
    vi.restoreAllMocks();
  });

  it("dry-run reports staleness without scheduling exit", async () => {
    const bundle = await makeFreshBundle();
    try {
      const deps = makeDeps({
        bundlePath: bundle,
        bootTimeMs: () => Date.now() - 60_000, // bundle is newer
      });
      const report = await handleReloadCode({}, deps);
      expect(report.success).toBe(true);
      expect(report.scheduledRestart).toBe(false);
      expect(report.reason).toContain("Dry-run");
      expect(report.reason).toContain("WOULD reload");
      expect(deps.exitCalls).toHaveLength(0);
      expect(deps.scheduleCalls).toHaveLength(0);
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });

  it("dry-run reports 'no-op' when bundle is older than boot time", async () => {
    const bundle = await makeStaleBundle(60_000);
    try {
      const deps = makeDeps({
        bundlePath: bundle,
        // Process booted AFTER the bundle was written.
        bootTimeMs: () => Date.now(),
      });
      const report = await handleReloadCode({}, deps);
      expect(report.success).toBe(true);
      expect(report.scheduledRestart).toBe(false);
      expect(report.reason).toContain("not newer");
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });

  it("confirm=true schedules an exit when the bundle is newer", async () => {
    const bundle = await makeFreshBundle();
    try {
      const deps = makeDeps({
        bundlePath: bundle,
        bootTimeMs: () => Date.now() - 60_000,
        nowMs: () => Date.now(),
      });
      const report = await handleReloadCode({ confirm: true }, deps);
      expect(report.success).toBe(true);
      expect(report.scheduledRestart).toBe(true);
      expect(report.reason).toContain("Scheduled process exit");
      expect(deps.scheduleCalls).toHaveLength(1);
      expect(deps.scheduleCalls[0]?.ms).toBeGreaterThan(0);
      // If we flush the scheduled callback, it should call the injected exitFn
      // with code 0 — not the real process.exit.
      deps.scheduleCalls[0]?.fn();
      expect(deps.exitCalls).toEqual([0]);
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });

  it("confirm=true REFUSES when bundle is not newer (CWE-400 no-op guard)", async () => {
    const bundle = await makeStaleBundle(60_000);
    try {
      const deps = makeDeps({
        bundlePath: bundle,
        bootTimeMs: () => Date.now(), // bundle is older
      });
      const report = await handleReloadCode({ confirm: true }, deps);
      expect(report.success).toBe(false);
      expect(report.scheduledRestart).toBe(false);
      expect(report.reason).toContain("not newer");
      expect(deps.exitCalls).toHaveLength(0);
      expect(deps.scheduleCalls).toHaveLength(0);
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });

  it("rate-limits a second confirm call inside the 10s window (CWE-400)", async () => {
    const bundle = await makeFreshBundle();
    try {
      const t0 = Date.now();
      let now = t0;
      const deps = makeDeps({
        bundlePath: bundle,
        bootTimeMs: () => t0 - 60_000,
        nowMs: () => now,
      });

      const first = await handleReloadCode({ confirm: true }, deps);
      expect(first.scheduledRestart).toBe(true);

      // Advance the clock by only 2s — well inside the 10s window.
      now = t0 + 2_000;
      const second = await handleReloadCode({ confirm: true }, deps);
      expect(second.success).toBe(false);
      expect(second.scheduledRestart).toBe(false);
      expect(second.reason).toContain("Rate-limited");
      expect(second.rateLimitedForMs).toBeGreaterThan(0);
      // The second call must NOT have scheduled a new exit.
      expect(deps.scheduleCalls).toHaveLength(1);
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });

  it("allows a new confirm call after the rate-limit window elapses", async () => {
    const bundle = await makeFreshBundle();
    try {
      const t0 = Date.now();
      let now = t0;
      const deps = makeDeps({
        bundlePath: bundle,
        bootTimeMs: () => t0 - 60_000,
        nowMs: () => now,
      });

      await handleReloadCode({ confirm: true }, deps);
      now = t0 + 15_000; // past the 10s window
      const later = await handleReloadCode({ confirm: true }, deps);
      expect(later.scheduledRestart).toBe(true);
      expect(deps.scheduleCalls).toHaveLength(2);
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });

  it("returns a clean error envelope when the bundle cannot be read (CWE-754)", async () => {
    const deps = makeDeps({
      bundlePath: "/definitely/does/not/exist/dist/index.js",
    });
    const report = await handleReloadCode({ confirm: true }, deps);
    expect(report.success).toBe(false);
    expect(report.scheduledRestart).toBe(false);
    expect(report.reason).toContain("Could not stat bundle");
    expect(deps.exitCalls).toHaveLength(0);
    expect(deps.scheduleCalls).toHaveLength(0);
  });

  it("treats confirm=false or missing as dry-run (CWE-20 safe default)", async () => {
    const bundle = await makeFreshBundle();
    try {
      const deps = makeDeps({
        bundlePath: bundle,
        bootTimeMs: () => Date.now() - 60_000,
      });
      const implicit = await handleReloadCode({}, deps);
      const explicit = await handleReloadCode({ confirm: false }, deps);
      expect(implicit.scheduledRestart).toBe(false);
      expect(explicit.scheduledRestart).toBe(false);
      expect(deps.scheduleCalls).toHaveLength(0);
      expect(deps.exitCalls).toHaveLength(0);
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });

  it("includes diagnostic fields useful for debugging", async () => {
    const bundle = await makeFreshBundle();
    try {
      const deps = makeDeps({
        bundlePath: bundle,
        bootTimeMs: () => Date.now() - 60_000,
      });
      const report = await handleReloadCode({}, deps);
      expect(report.bundleMtime).toMatch(/\d{4}-\d{2}-\d{2}T/);
      expect(report.bootTime).toMatch(/\d{4}-\d{2}-\d{2}T/);
      expect(typeof report.bundleAheadByMs).toBe("number");
      expect(report.bundleAheadByMs).toBeGreaterThan(0);
    } finally {
      await rm(path.dirname(bundle), { recursive: true, force: true });
    }
  });
});
