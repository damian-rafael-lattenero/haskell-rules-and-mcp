/**
 * `mcp_reload_code` — schedule a graceful restart of the MCP Node process so
 * TypeScript edits made during an active Claude session take effect without
 * forcing a full Claude Desktop restart.
 *
 * # Why this exists
 *
 * The MCP process caches `dist/index.js` at startup. Any TS edit → rebuild
 * cycle produces a newer `dist/` on disk, but the running process is
 * unaware until it is killed and respawned. `mcp_restart` only restarts the
 * GHCi child; it does not reload the Node bundle. This gap (OBS-2 in the
 * Phase-5 debug-inspection) blocks agents from iterating on the MCP itself
 * within a single Claude session.
 *
 * # Design
 *
 * Dry-run by default: the tool reports whether a restart would reload new
 * code (based on `dist/index.js` mtime vs. boot time). Only when the caller
 * passes `confirm: true` does the process actually exit.
 *
 * Claude Desktop (and every MCP-compliant client we care about) respawns
 * the child process automatically after a stdio transport disconnect.
 * `process.exit(0)` after the response has been serialized and flushed is
 * therefore safe and recoverable.
 *
 * # Security surface (Node.js stack)
 *
 * This tool intentionally terminates the host process. Precautions:
 *
 *   • CWE-20 (improper input validation): Zod strict schema is enforced by
 *     `registerStrictTool`; `confirm` is the only accepted field.
 *   • CWE-400 (uncontrolled resource consumption / restart loop): we reject
 *     restarts whose `dist/index.js` mtime is not newer than this process's
 *     boot time AND we apply a rate limit so no more than one restart can
 *     be scheduled in a short window. An agent cannot accidentally wedge
 *     itself into a spin loop.
 *   • CWE-754 (exceptional condition handling): every fs call is guarded;
 *     on read error we return the error in the response instead of crashing.
 *   • No secrets touched. State is ephemeral by design — `properties.json`
 *     lives on disk and survives restart; in-memory state (GHCi session,
 *     workflow tracker, tool history) is deliberately lost.
 *   • No shell exec. `process.exit(0)` is called directly.
 *
 * # Rate-limiting
 *
 * The window is small (10 s) because the legitimate use case — "I just
 * rebuilt, reload now" — happens at human speed. It exists solely to
 * protect against an accidental recursive loop where a post-restart tool
 * call immediately re-invokes `mcp_reload_code` before the operator can
 * observe the result. The window is deliberately short so an operator
 * retrying after 30 s never hits it.
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { stat } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { type ToolContext, registerStrictTool, zBool } from "./registry.js";

/**
 * Process-wide boot time in milliseconds since epoch. Captured at module
 * import so re-imports (e.g. in tests) do not reset it. Used to detect
 * whether `dist/index.js` has been rewritten since this process started.
 */
const BOOT_TIME_MS = Date.now();

/** Timestamp of the last successful restart request, used for rate-limiting. */
let lastRestartAttemptMs = 0;

/** Minimum delay between consecutive restart attempts. */
const RESTART_RATE_LIMIT_MS = 10_000;

/**
 * Delay between serializing the response and calling process.exit. Must be
 * long enough for the stdio transport to flush the final JSON-RPC frame
 * and for the caller to observe the response on their end.
 */
const EXIT_DELAY_MS = 500;

export interface ReloadReport {
  success: boolean;
  /** Did the tool schedule an actual exit, or was it dry-run? */
  scheduledRestart: boolean;
  /** Why the restart was (or was not) scheduled. */
  reason: string;
  /** mtime of the running bundle file, if readable. */
  bundleMtime?: string;
  /** Boot time of the currently running process. */
  bootTime: string;
  /** How much newer dist/index.js is than this process, in ms. Negative / zero
   *  means the bundle is not newer — no reload would actually change code. */
  bundleAheadByMs?: number;
  /** When the exit will fire, if scheduled. */
  willExitAt?: string;
  /** Seconds until the rate-limit window expires, if blocked by it. */
  rateLimitedForMs?: number;
}

export interface ReloadDeps {
  /** Path to the bundle whose mtime we compare. Defaults to the current
   *  module's resolved URL (i.e. this compiled file's absolute path's dist
   *  entry point). Override in tests. */
  bundlePath: string;
  /** How we terminate the process. Default: `process.exit`. Override in tests. */
  exitFn: (code: number) => void;
  /** How we schedule the deferred exit. Default: `setTimeout`. Override in tests. */
  scheduleFn: (fn: () => void, ms: number) => void;
  /** Returns the current time. Override in tests. */
  nowMs: () => number;
  /** Returns the current BOOT_TIME_MS. */
  bootTimeMs: () => number;
}

function defaultBundlePath(): string {
  // `import.meta.url` points at this file inside dist/tools/. The "main"
  // bundle a client re-loads on restart is dist/index.js at the package
  // root of the compiled output — comparing its mtime captures any TS
  // edit anywhere in src/ that triggered a rebuild.
  const here = fileURLToPath(import.meta.url);
  // Walk up from dist/tools/mcp-reload-code.js → dist/index.js
  return here.replace(/tools\/mcp-reload-code\.js$/, "index.js");
}

/**
 * Pure handler — exposes every side-effect as an injected dependency so the
 * behavior is unit-testable without actually killing the process.
 */
export async function handleReloadCode(
  args: { confirm?: boolean },
  deps: ReloadDeps
): Promise<ReloadReport> {
  const confirm = args.confirm === true;
  const bootTimeMs = deps.bootTimeMs();
  const bootTimeIso = new Date(bootTimeMs).toISOString();

  let bundleMtimeMs: number | undefined;
  let bundleMtimeIso: string | undefined;
  let bundleAheadByMs: number | undefined;
  try {
    const s = await stat(deps.bundlePath);
    bundleMtimeMs = s.mtimeMs;
    bundleMtimeIso = s.mtime.toISOString();
    bundleAheadByMs = Math.round(bundleMtimeMs - bootTimeMs);
  } catch (err) {
    return {
      success: false,
      scheduledRestart: false,
      reason:
        `Could not stat bundle at ${deps.bundlePath} — refusing to schedule a restart into an unknown state. ` +
        `Underlying error: ${(err as Error).message}`,
      bootTime: bootTimeIso,
    };
  }

  // Dry-run path: always safe, never exits.
  if (!confirm) {
    const staleness =
      bundleAheadByMs !== undefined && bundleAheadByMs > 0
        ? `Bundle is ${Math.round(bundleAheadByMs / 1000)}s newer than the running process — a restart WOULD reload code.`
        : "Bundle is not newer than the running process — restart would be a no-op.";
    return {
      success: true,
      scheduledRestart: false,
      reason: `Dry-run. ${staleness} Call again with confirm=true to actually restart.`,
      bundleMtime: bundleMtimeIso,
      bootTime: bootTimeIso,
      bundleAheadByMs,
    };
  }

  // Staleness gate — refuse to restart into the same code.
  if (bundleAheadByMs === undefined || bundleAheadByMs <= 0) {
    return {
      success: false,
      scheduledRestart: false,
      reason:
        "Refusing to restart: dist/index.js is not newer than the running process. " +
        "Run `npm run build` in mcp-server first, then retry.",
      bundleMtime: bundleMtimeIso,
      bootTime: bootTimeIso,
      bundleAheadByMs,
    };
  }

  // Rate-limit gate — prevents restart-loop DoS via recursive client calls.
  const now = deps.nowMs();
  const sinceLast = now - lastRestartAttemptMs;
  if (lastRestartAttemptMs > 0 && sinceLast < RESTART_RATE_LIMIT_MS) {
    const wait = RESTART_RATE_LIMIT_MS - sinceLast;
    return {
      success: false,
      scheduledRestart: false,
      reason:
        `Rate-limited. Last restart was ${Math.round(sinceLast / 1000)}s ago; ` +
        `must wait ${Math.round(wait / 1000)}s before retrying.`,
      bundleMtime: bundleMtimeIso,
      bootTime: bootTimeIso,
      bundleAheadByMs,
      rateLimitedForMs: wait,
    };
  }
  lastRestartAttemptMs = now;

  // Schedule the exit. The response is returned FIRST so the client
  // observes it over stdio; the actual exit happens after the flush window.
  const willExitAt = new Date(now + EXIT_DELAY_MS).toISOString();
  deps.scheduleFn(() => {
    // Use explicit exit code 0 — the restart is intentional, not an error.
    // The client (Claude Desktop) respawns the stdio child on disconnect.
    deps.exitFn(0);
  }, EXIT_DELAY_MS);

  return {
    success: true,
    scheduledRestart: true,
    reason:
      `Scheduled process exit in ${EXIT_DELAY_MS}ms. The MCP client will respawn the child ` +
      "automatically on its next call. In-memory state (GHCi session, tool history) is discarded " +
      "by design; persisted state (.haskell-flows/properties.json) survives.",
    bundleMtime: bundleMtimeIso,
    bootTime: bootTimeIso,
    bundleAheadByMs,
    willExitAt,
  };
}

/**
 * Test-only hook to reset the rate-limit state. Do NOT call from production
 * code — it defeats the CWE-400 guardrail.
 */
export function _resetRateLimitForTesting(): void {
  lastRestartAttemptMs = 0;
}

export function register(server: McpServer, ctx: ToolContext): void {
  registerStrictTool(
    server,
    ctx,
    "mcp_reload_code",
    "Schedule a graceful restart of the MCP Node process so fresh TypeScript edits take effect. " +
      "Dry-run by default: returns whether the compiled bundle (dist/index.js) is newer than the " +
      "running process. Pass confirm=true to actually exit — the MCP client will respawn the child " +
      "automatically. mcp_restart, by contrast, only restarts GHCi; this one reloads the MCP bundle.",
    {
      confirm: zBool()
        .optional()
        .describe(
          "Set to true to actually perform the restart. Default false = dry-run. " +
            "Rate-limited (one restart per 10s) and gated on bundle staleness to prevent loops."
        ),
    },
    async ({ confirm }) => {
      const report = await handleReloadCode(
        { confirm },
        {
          bundlePath: defaultBundlePath(),
          exitFn: (code: number) => process.exit(code),
          scheduleFn: (fn, ms) => void setTimeout(fn, ms),
          nowMs: () => Date.now(),
          bootTimeMs: () => BOOT_TIME_MS,
        }
      );
      return {
        content: [{ type: "text" as const, text: JSON.stringify(report) }],
      };
    }
  );
}
