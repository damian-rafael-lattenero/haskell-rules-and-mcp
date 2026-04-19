import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { GhciSession } from "../ghci-session.js";
import type { WorkflowState, ModuleProgress } from "../workflow-state.js";
import { access } from "node:fs/promises";
import path from "node:path";
import { z } from "zod";
import {
  computeStaleness,
  stalenessMessage,
  defaultBundlePath,
  activeBundlePath,
  bootTimeMs,
  type StalenessResult,
} from "../staleness.js";

/**
 * Context passed to each tool's register function.
 * Provides access to session, project directory, and other shared state.
 */
export interface ToolContext {
  getSession: () => Promise<GhciSession>;
  getProjectDir: () => string;
  getBaseDir: () => string;
  resetQuickCheckState: () => void;
  /** Returns a notice string if rules are not installed, or null if OK. Cached. */
  getRulesNotice: () => Promise<string | null>;
  /** Reset the rules notice cache (call after ghci_setup installs rules). */
  resetRulesCache: () => void;
  /** Get the current workflow state. */
  getWorkflowState: () => WorkflowState;
  /** Log a tool execution to workflow history. */
  logToolExecution: (toolName: string, success: boolean) => void;
  /** Get progress for a specific module. */
  getModuleProgress: (modulePath: string) => ModuleProgress | undefined;
  /** Update progress for a specific module. */
  updateModuleProgress: (modulePath: string, updates: Partial<ModuleProgress>) => void;
  /** Track availability of optional bundled/host-backed tools. */
  setOptionalToolAvailability: (
    tool: "lint" | "format" | "hls",
    status: "unknown" | "available" | "unavailable"
  ) => void;
  /** Invalidate project discovery cache after creating a new project. */
  invalidateProjectsCache?: () => void;
}

export type RegisterFn = (server: McpServer, ctx: ToolContext) => void;

// ─── Boundary-tolerant primitive coercers ────────────────────────────────────
// The Claude Code → MCP SDK client currently serializes non-string JSON
// primitives as strings ("true"/"false" for bools, "42" for numbers,
// JSON-stringified arrays for arrays). These helpers let each tool accept
// BOTH the canonical JSON type AND the stringified form without weakening
// the `.strict()` rejection of *unknown keys* — they only relax the TYPE
// side of validation, not the KEY set.
//
// Security: unknown-keys rejection (Fase 1) is unaffected. Accepting a
// string-encoded boolean does not let a caller smuggle in new fields.

/** Accepts `true`/`false` OR `"true"`/`"false"`/`"1"`/`"0"`. */
export function zBool(): z.ZodType<boolean> {
  return z.union([
    z.boolean(),
    z
      .string()
      .toLowerCase()
      .transform((s, ctx) => {
        if (s === "true" || s === "1") return true;
        if (s === "false" || s === "0") return false;
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: `Expected boolean or "true"/"false"/"1"/"0", got "${s}"`,
        });
        return z.NEVER;
      }),
  ]);
}

/** Accepts a JSON number OR a numeric string. Rejects NaN / non-finite values. */
export function zNum(): z.ZodType<number> {
  return z.union([
    z.number().refine(Number.isFinite, "Must be finite"),
    z
      .string()
      .transform((s, ctx) => {
        const n = Number(s);
        if (!Number.isFinite(n)) {
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            message: `Expected number or numeric string, got "${s}"`,
          });
          return z.NEVER;
        }
        return n;
      }),
  ]);
}

/** Accepts `T[]` OR a JSON-stringified array which parses to `T[]`. */
export function zArray<T extends z.ZodTypeAny>(item: T): z.ZodType<z.infer<T>[]> {
  const arrSchema = z.array(item);
  return z.union([
    arrSchema,
    z
      .string()
      .transform((s, ctx) => {
        try {
          const parsed = JSON.parse(s);
          const result = arrSchema.safeParse(parsed);
          if (!result.success) {
            ctx.addIssue({
              code: z.ZodIssueCode.custom,
              message: `Stringified array failed to parse against inner schema`,
            });
            return z.NEVER;
          }
          return result.data;
        } catch {
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            message: `Expected array or JSON-encoded array string`,
          });
          return z.NEVER;
        }
      }),
  ]) as unknown as z.ZodType<z.infer<T>[]>;
}

/** Accepts `Record<string, T>` OR a JSON-stringified map. */
export function zRecord<T extends z.ZodTypeAny>(
  value: T
): z.ZodType<Record<string, z.infer<T>>> {
  const recSchema = z.record(z.string(), value);
  return z.union([
    recSchema,
    z
      .string()
      .transform((s, ctx) => {
        try {
          const parsed = JSON.parse(s);
          const result = recSchema.safeParse(parsed);
          if (!result.success) {
            ctx.addIssue({
              code: z.ZodIssueCode.custom,
              message: `Stringified record failed to parse against inner schema`,
            });
            return z.NEVER;
          }
          return result.data as Record<string, z.infer<T>>;
        } catch {
          ctx.addIssue({
            code: z.ZodIssueCode.custom,
            message: `Expected record or JSON-encoded record string`,
          });
          return z.NEVER;
        }
      }),
  ]) as unknown as z.ZodType<Record<string, z.infer<T>>>;
}

/**
 * Register a tool with a strict Zod schema — unknown keys are rejected.
 *
 * The MCP SDK's default behavior is to strip unknown keys silently (Zod's
 * `.object()` default). This wrapper wraps the shape in `.strict()` so the
 * SDK's validator rejects unknown parameter names with a structured error
 * instead of letting them flow through unnoticed.
 *
 * Use this for any tool where an unknown param name could silently change
 * behavior (e.g. project-switch tools that behave differently when a param
 * is absent vs present).
 */
export function registerStrict<Shape extends z.ZodRawShape>(
  server: McpServer,
  options: {
    name: string;
    description: string;
    shape: Shape;
  },
  handler: (args: z.infer<z.ZodObject<Shape>>) => Promise<{
    content: Array<{ type: "text"; text: string }>;
  }>
): void {
  const strictSchema = z.object(options.shape).strict();
  // The SDK's registerTool accepts either a raw shape or a schema instance.
  // Passing a strict ZodObject makes the SDK run `.strict()` validation.
  server.registerTool(
    options.name,
    {
      description: options.description,
      inputSchema: strictSchema as unknown as Shape,
    },
    handler as unknown as Parameters<typeof server.registerTool>[2]
  );
}

/**
 * Drop-in replacement for `server.tool(name, description, shape, handler)`.
 *
 * Behaviors added on top of the plain SDK call:
 *   1. Wraps `shape` in `z.object(shape).strict()` so the SDK rejects unknown
 *      keys structurally (Bug 1 fix) instead of silently stripping them.
 *   2. Lazily triggers `ensureToolchainWarmupStarted(ctx)` on each call. The
 *      warmup is idempotent; only the first call actually kicks off downloads.
 *      This makes optional binaries (hlint, fourmolu, hls) available in the
 *      background without an explicit `ghci_toolchain_status` call.
 *
 * Security note: warmup never executes a downloaded binary — it only fetches
 * via `ensureTool()`, which enforces SHA256 verification when the release
 * manifest configures a checksum. Binaries run only when a concrete tool
 * (e.g. `ghci_lint`) explicitly invokes them with `execFile`.
 */
/**
 * Inject a `_warning` field into a tool response body when the MCP bundle
 * is detected as stale vs the running process. Conservative: ONLY modifies
 * responses whose first content text parses as a plain JSON object. Arrays,
 * non-JSON, and nested-content responses pass through untouched.
 *
 * Returns the response verbatim on any error — middleware must never make
 * things worse than not running at all.
 *
 * Exported for direct unit testing.
 */
export function injectStalenessWarning(
  result: { content: Array<{ type: "text"; text: string }> },
  staleness: StalenessResult
): { content: Array<{ type: "text"; text: string }> } {
  const warning = stalenessMessage(staleness);
  if (!warning) return result;
  const first = result.content?.[0];
  if (!first || first.type !== "text" || typeof first.text !== "string") {
    return result;
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(first.text);
  } catch {
    return result;
  }
  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    return result;
  }
  // Preserve any pre-existing `_warning` (tool authors can set their own).
  // If the field already exists and is a string, we APPEND; if it's an
  // array, we PUSH; otherwise we leave it alone.
  const obj = parsed as Record<string, unknown>;
  const existing = obj._warning;
  if (existing === undefined) {
    obj._warning = warning;
  } else if (typeof existing === "string") {
    // Avoid duplicating if the tool already emitted our exact message.
    if (!existing.includes("MCP bundle on disk is")) {
      obj._warning = `${existing} | ${warning}`;
    }
  } else if (Array.isArray(existing)) {
    if (!existing.some((x) => typeof x === "string" && x.includes("MCP bundle on disk is"))) {
      existing.push(warning);
    }
  }
  const newText = JSON.stringify(obj);
  const rest = result.content.slice(1);
  return {
    content: [{ type: "text", text: newText }, ...rest] as Array<{ type: "text"; text: string }>,
  };
}

export function registerStrictTool<Shape extends z.ZodRawShape>(
  server: McpServer,
  ctx: ToolContext,
  name: string,
  description: string,
  shape: Shape,
  // Handler type mirrors what the SDK's tool callback expects: the SDK has
  // already parsed args against the inputSchema before invoking this handler.
  handler: (args: z.infer<z.ZodObject<Shape>>, extra: unknown) => Promise<{
    content: Array<{ type: "text"; text: string }>;
  }>
): void {
  const strictSchema = z.object(shape).strict();

  const wrappedHandler = async (args: z.infer<z.ZodObject<Shape>>, extra: unknown) => {
    // Lazy imports to avoid circular deps with tools that import from here.
    const { ensureToolchainWarmupStarted } = await import("./toolchain-warmup.js");
    const { recordToolCall } = await import("../telemetry.js");

    ensureToolchainWarmupStarted(ctx);

    let success = false;
    try {
      const result = await handler(args, extra);
      // Best-effort inspection: if the handler returned a JSON body with
      // `success: false` we still classify the call as a failure for
      // telemetry purposes. Never throws — record ALL paths.
      const text = result.content?.[0]?.text;
      if (typeof text === "string") {
        try {
          const parsed = JSON.parse(text);
          success = parsed?.success !== false;
        } catch {
          success = true; // non-JSON text body — treat as success
        }
      } else {
        success = true;
      }

      // Middleware: surface a staleness warning when the MCP bundle on
      // disk is newer than the running process. Opt-out via env. Failure
      // to probe is non-fatal — we return the original result untouched.
      if (process.env.HASKELL_FLOWS_STALENESS_WARN !== "0") {
        try {
          const staleness = await computeStaleness({
            bundlePath: activeBundlePath(defaultBundlePath(import.meta.url)),
            nowMs: () => Date.now(),
            bootTimeMs: () => bootTimeMs(),
          });
          return injectStalenessWarning(result, staleness);
        } catch {
          // Pass-through on any probe error — middleware MUST NOT poison
          // the pipeline with an exception under any circumstance.
          return result;
        }
      }
      return result;
    } catch (err) {
      success = false;
      throw err;
    } finally {
      // Opt-in only — no-op + fast when HASKELL_FLOWS_TELEMETRY unset.
      void recordToolCall(ctx.getProjectDir(), name, success);
    }
  };

  // Use `registerTool` (not `server.tool`) because the latter's runtime
  // disambiguation treats a pre-built ZodObject as annotations — stripping
  // all argument validation. `registerTool` explicitly accepts either a raw
  // shape or an `AnySchema` in `inputSchema`, so passing a strict ZodObject
  // keeps the Zod validator active and rejects unknown keys at the SDK layer.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (server.registerTool as any)(
    name,
    { description, inputSchema: strictSchema },
    wrappedHandler
  );
}

/**
 * Create a cached rules-check function.
 * Checks once, then caches the result for the session lifetime.
 * Reset by calling the returned resetFn.
 */
export function createRulesChecker(
  getProjectDir: () => string,
  getBaseDir?: () => string
): {
  check: () => Promise<string | null>;
  reset: () => void;
} {
  let cached: string | null | undefined = undefined;
  let noticeShown = false;

  return {
    check: async () => {
      // Only show the notice once per session — repeating it 15+ times wastes LLM context
      if (noticeShown) return null;

      if (cached !== undefined) {
        if (cached !== null) noticeShown = true;
        return cached;
      }

      // Check project dir first, then walk up to repo root (base dir)
      const dirsToCheck = [getProjectDir()];
      if (getBaseDir) {
        const baseDir = getBaseDir();
        if (baseDir !== getProjectDir()) {
          dirsToCheck.push(baseDir);
        }
      }

      for (const dir of dirsToCheck) {
        const automationPath = path.join(dir, ".claude", "rules", "haskell-automation.md");
        try {
          await access(automationPath);
          cached = null; // Rules exist
          return cached;
        } catch {
          // Not found here, try next directory
        }
      }

      cached =
        "Optional: run ghci_setup() to install development rules in .claude/rules/. " +
        "All tools work without it.";
      noticeShown = true;
      return cached;
    },
    reset: () => {
      cached = undefined;
      // Keep noticeShown true — the notice only needs to show once per session,
      // not once per project switch. Resetting it causes repeated notices.
    },
  };
}
