import { describe, it, expect, afterEach } from "vitest";
import { mkdtempSync, rmSync, existsSync, readFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  isTelemetryEnabled,
  recordToolCall,
  readTelemetry,
} from "../telemetry.js";

describe("telemetry opt-in gate", () => {
  it("is OFF when neither env var is set", () => {
    expect(isTelemetryEnabled({})).toBe(false);
  });

  it("is OFF when env var is '0' / 'false' / 'off' / 'no' / empty", () => {
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "0" })).toBe(false);
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "false" })).toBe(false);
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "off" })).toBe(false);
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "no" })).toBe(false);
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "" })).toBe(false);
  });

  it("is ON when env var is '1' / 'true' / any non-falsy string", () => {
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "1" })).toBe(true);
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "true" })).toBe(true);
    expect(isTelemetryEnabled({ HASKELL_FLOWS_TELEMETRY: "yes" })).toBe(true);
  });

  it("accepts the legacy alias ENABLE_HASKELL_FLOWS_TELEMETRY", () => {
    expect(isTelemetryEnabled({ ENABLE_HASKELL_FLOWS_TELEMETRY: "1" })).toBe(true);
  });
});

describe("recordToolCall (opt-in)", () => {
  const tmpDirs: string[] = [];
  function mkTmp(): string {
    const d = mkdtempSync(path.join(os.tmpdir(), "telemetry-"));
    tmpDirs.push(d);
    return d;
  }
  afterEach(() => {
    for (const d of tmpDirs.splice(0)) {
      try { rmSync(d, { recursive: true, force: true }); } catch { /* ignore */ }
    }
  });

  it("is a no-op when telemetry is disabled — does not create the file", async () => {
    const dir = mkTmp();
    await recordToolCall(dir, "ghci_load", true, { HASKELL_FLOWS_TELEMETRY: "0" });
    expect(existsSync(path.join(dir, ".haskell-flows", "telemetry.json"))).toBe(false);
  });

  it("creates the telemetry file on first recorded call when enabled", async () => {
    const dir = mkTmp();
    await recordToolCall(dir, "ghci_load", true, { HASKELL_FLOWS_TELEMETRY: "1" });
    const file = await readTelemetry(dir);
    expect(file).not.toBeNull();
    expect(file?.version).toBe(1);
    expect(file?.enabled).toBe(true);
    expect(file?.tools.ghci_load).toEqual(
      expect.objectContaining({ calls: 1, successes: 1, failures: 0 })
    );
  });

  it("accumulates successes and failures per tool across calls", async () => {
    const dir = mkTmp();
    const env = { HASKELL_FLOWS_TELEMETRY: "1" };
    await recordToolCall(dir, "ghci_load", true, env);
    await recordToolCall(dir, "ghci_load", false, env);
    await recordToolCall(dir, "ghci_load", true, env);
    await recordToolCall(dir, "ghci_quickcheck", true, env);

    const file = await readTelemetry(dir);
    expect(file?.tools.ghci_load).toEqual(
      expect.objectContaining({ calls: 3, successes: 2, failures: 1 })
    );
    expect(file?.tools.ghci_quickcheck).toEqual(
      expect.objectContaining({ calls: 1, successes: 1, failures: 0 })
    );
  });

  it("never includes a timestamp narrower than day-level (no HH:MM or args)", async () => {
    const dir = mkTmp();
    await recordToolCall(dir, "ghci_load", true, { HASKELL_FLOWS_TELEMETRY: "1" });
    const raw = readFileSync(path.join(dir, ".haskell-flows", "telemetry.json"), "utf-8");
    // Rough sanity check: no ISO time-of-day fragments, no argument echoes.
    expect(raw).not.toMatch(/T\d\d:\d\d/);
    expect(raw).not.toMatch(/module_path|property|function_name/);
  });

  it("does not throw when the projectDir is unwritable (read-only /dev/null)", async () => {
    // Using /dev/null/subpath which is guaranteed to fail; record must swallow.
    await expect(
      recordToolCall("/dev/null/haskell-flows-broken", "ghci_load", true, { HASKELL_FLOWS_TELEMETRY: "1" })
    ).resolves.toBeUndefined();
  });
});
