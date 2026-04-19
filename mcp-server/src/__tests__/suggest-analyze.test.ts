/**
 * Unit coverage for the Fase 3 fix to `handleAnalyze`: the handler now loads
 * the target module before `:browse`, so the tool is idempotent regardless of
 * which module was most recently loaded in the session.
 *
 * We mock the GHCi session to record the command sequence and assert that
 * the first command issued is `:l <module_path>` (via `loadModule`).
 */
import { describe, it, expect } from "vitest";
import { handleAnalyze } from "../tools/suggest.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

describe("handleAnalyze (Fase 3 fix)", () => {
  it("calls loadModule(module_path) BEFORE `:browse`", async () => {
    const events: string[] = [];
    const session = createMockSession({
      loadModule: async (mp: string) => {
        events.push(`loadModule:${mp}`);
        return { output: "Ok, one module loaded.", success: true } as GhciResult;
      },
      execute: async (cmd: string): Promise<GhciResult> => {
        events.push(`exec:${cmd}`);
        if (cmd.startsWith(":browse")) {
          return { output: "eval :: Int -> Int", success: true };
        }
        return { output: "", success: true };
      },
    });

    const result = JSON.parse(
      await handleAnalyze(session, "src/Expr/Eval.hs", "/tmp")
    );
    expect(result.success).toBe(true);
    expect(result.mode).toBe("analyze");
    expect(events[0]).toBe("loadModule:src/Expr/Eval.hs");
    // `:browse` must come AFTER loadModule, not before.
    const browseIx = events.findIndex((e) => e.startsWith("exec::browse"));
    expect(browseIx).toBeGreaterThan(0);
  });

  it("short-circuits with a structured error when loadModule fails", async () => {
    const session = createMockSession({
      loadModule: async () =>
        ({ output: "src/Bad.hs:3:7: error: ...", success: false } as GhciResult),
      execute: async () => ({ output: "", success: true }),
    });
    const result = JSON.parse(
      await handleAnalyze(session, "src/Bad.hs", "/tmp")
    );
    expect(result.success).toBe(false);
    expect(result.mode).toBe("analyze");
    expect(result.error).toMatch(/Could not load/);
    expect(typeof result.loadOutput).toBe("string");
  });
});
