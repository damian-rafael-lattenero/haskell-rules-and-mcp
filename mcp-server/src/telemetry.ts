/**
 * Opt-in local tool-usage telemetry.
 *
 * Off by default. Enabled only when:
 *   - `HASKELL_FLOWS_TELEMETRY=1` is set in the process environment, or
 *   - `ENABLE_HASKELL_FLOWS_TELEMETRY` is set (legacy alias).
 *
 * Security invariants:
 *   - NEVER makes a network call. The collected data stays in
 *     `<projectDir>/.haskell-flows/telemetry.json`.
 *   - Records only aggregate counts: per-tool call count and success/failure
 *     tallies. No arguments, no outputs, no file paths, no timestamps
 *     narrower than day-level bucketing.
 *   - The file is written with standard permissions (no special chmod); the
 *     data is less sensitive than any Haskell source file already on disk.
 *   - Failures to write are swallowed — telemetry must never break a tool
 *     call or surface an error to the agent.
 *
 * This module is intended to help the project owner prioritize which tools
 * to keep or retire based on real usage, not to phone home.
 */

import { readFile, writeFile, mkdir } from "node:fs/promises";
import path from "node:path";

const ENV_KEYS = ["HASKELL_FLOWS_TELEMETRY", "ENABLE_HASKELL_FLOWS_TELEMETRY"];

export function isTelemetryEnabled(env: NodeJS.ProcessEnv = process.env): boolean {
  for (const k of ENV_KEYS) {
    const v = env[k];
    if (v === undefined) continue;
    const s = v.trim().toLowerCase();
    if (s === "" || s === "0" || s === "false" || s === "off" || s === "no") continue;
    return true;
  }
  return false;
}

export interface ToolStats {
  calls: number;
  successes: number;
  failures: number;
  lastDay: string; // YYYY-MM-DD (UTC)
}

export interface TelemetryFile {
  version: 1;
  enabled: true;
  createdAt: string; // YYYY-MM-DD (UTC)
  updatedAt: string; // YYYY-MM-DD (UTC)
  tools: Record<string, ToolStats>;
}

function todayUtc(): string {
  const d = new Date();
  return `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
}

function telemetryPath(projectDir: string): string {
  return path.join(projectDir, ".haskell-flows", "telemetry.json");
}

async function loadOrInit(projectDir: string): Promise<TelemetryFile> {
  const p = telemetryPath(projectDir);
  try {
    const raw = await readFile(p, "utf-8");
    const parsed = JSON.parse(raw) as TelemetryFile;
    if (parsed.version === 1 && parsed.enabled === true && typeof parsed.tools === "object") {
      return parsed;
    }
  } catch {
    // Missing / malformed — start fresh.
  }
  const now = todayUtc();
  return { version: 1, enabled: true, createdAt: now, updatedAt: now, tools: {} };
}

/**
 * Record a single tool execution. No-op (and fast) when telemetry is off.
 *
 * Designed to be called from inside the registerStrictTool wrapper after the
 * handler resolves (success or rejection). Swallows all IO errors so it
 * cannot affect tool behavior.
 */
export async function recordToolCall(
  projectDir: string,
  toolName: string,
  success: boolean,
  env: NodeJS.ProcessEnv = process.env
): Promise<void> {
  if (!isTelemetryEnabled(env)) return;
  try {
    const dir = path.dirname(telemetryPath(projectDir));
    await mkdir(dir, { recursive: true });
    const file = await loadOrInit(projectDir);
    const today = todayUtc();
    const stats = file.tools[toolName] ?? { calls: 0, successes: 0, failures: 0, lastDay: today };
    stats.calls += 1;
    if (success) stats.successes += 1;
    else stats.failures += 1;
    stats.lastDay = today;
    file.tools[toolName] = stats;
    file.updatedAt = today;
    await writeFile(telemetryPath(projectDir), JSON.stringify(file, null, 2), "utf-8");
  } catch {
    // Telemetry MUST NOT break tool calls. Silently swallow IO errors.
  }
}

/** For tests + diagnostics: read the current telemetry file if present. */
export async function readTelemetry(projectDir: string): Promise<TelemetryFile | null> {
  try {
    const raw = await readFile(telemetryPath(projectDir), "utf-8");
    return JSON.parse(raw) as TelemetryFile;
  } catch {
    return null;
  }
}
