/**
 * Background warmup for optional toolchain binaries (hlint, fourmolu, ormolu, hls).
 *
 * Strategy:
 *   - The first tool call of any kind triggers `ensureToolchainWarmupStarted()`.
 *   - Downloads run concurrently in the background; callers never block on startup.
 *   - In-flight download promises are cached per tool so a race (e.g. two lint
 *     calls before warmup finishes) does not trigger two downloads.
 *   - Once warmup completes (success or failure), the result is propagated to
 *     the WorkflowState via `setOptionalToolAvailability(...)` so `_guidance`
 *     reflects reality.
 *   - `awaitTool(tool)` is used by tools that need the binary immediately — it
 *     either awaits the in-flight warmup or falls through to a fresh
 *     `ensureTool()` call if no warmup is active for that tool.
 *
 * Security:
 *   - This module adds NO new network behavior: all downloads go through
 *     `ensureTool()` which validates SHA256 checksums when the release manifest
 *     configures them, and rejects mismatches. The warmup only changes WHEN
 *     the download is triggered (eagerly at first tool call) vs HOW (same
 *     code path as before).
 *   - Warmup never executes a downloaded binary — only fetches it to
 *     `vendor-tools/` for later use by tools that invoke `execFile`.
 */

import { ensureTool, type EnsureResult } from "./tool-installer.js";
import type { ToolContext } from "./registry.js";

type WarmupTool = "hlint" | "fourmolu" | "ormolu" | "hls";
type WorkflowCategory = "lint" | "format" | "hls";

const DEFAULT_WARMUP_TOOLS: WarmupTool[] = ["hlint", "fourmolu", "hls"];

const warmups = new Map<WarmupTool, Promise<EnsureResult>>();
let warmupStarted = false;

function toWorkflowCategory(tool: WarmupTool): WorkflowCategory {
  if (tool === "hlint") return "lint";
  if (tool === "fourmolu" || tool === "ormolu") return "format";
  return "hls";
}

async function warmupTool(ctx: ToolContext, tool: WarmupTool): Promise<EnsureResult> {
  try {
    const result = await ensureTool(tool);
    const category = toWorkflowCategory(tool);
    // Only downgrade `format` to unavailable if BOTH fourmolu and ormolu failed,
    // since either can satisfy the formatter gate. Simplest conservative choice:
    // mark available on any positive result; let `ghci_toolchain_status` refine.
    if (result.available) {
      ctx.setOptionalToolAvailability(category, "available");
    } else if (category !== "format") {
      ctx.setOptionalToolAvailability(category, "unavailable");
    }
    return result;
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return {
      available: false,
      error: `warmup failed: ${message}`,
      message: `warmup failed for ${tool}: ${message}`,
    };
  }
}

/**
 * Kick off background downloads for optional toolchain binaries. Idempotent:
 * the first call fires the warmup, subsequent calls are no-ops. Never throws
 * and never awaits — returns immediately to keep the first tool call snappy.
 */
export function ensureToolchainWarmupStarted(
  ctx: ToolContext,
  tools: WarmupTool[] = DEFAULT_WARMUP_TOOLS
): void {
  if (warmupStarted) return;
  warmupStarted = true;
  for (const tool of tools) {
    if (!warmups.has(tool)) {
      warmups.set(tool, warmupTool(ctx, tool));
    }
  }
}

/**
 * Resolve a toolchain binary, waiting for any in-flight warmup. If no warmup
 * was triggered for this tool, falls through to a direct `ensureTool()` call.
 * Races are deduped: multiple concurrent callers share the same promise.
 */
export async function awaitTool(tool: WarmupTool): Promise<EnsureResult> {
  const pending = warmups.get(tool);
  if (pending) return pending;
  const fresh = ensureTool(tool);
  warmups.set(tool, fresh);
  return fresh;
}

/**
 * For tests: reset internal state so warmup can be retriggered.
 * Never called at runtime.
 */
export function _resetWarmupForTesting(): void {
  warmupStarted = false;
  warmups.clear();
}
