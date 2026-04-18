/**
 * Fase 4: `ghci_arbitrary` now accepts `module_path` and pre-loads it before
 * running `:i TypeName`, so callers don't hit the GHCi `:l replaces` eviction
 * bug. Unit test uses a mock session to assert loadModule happens before
 * infoOf when module_path is provided, and is skipped when it isn't.
 */
import { describe, it, expect } from "vitest";
import { handleArbitrary } from "../tools/arbitrary.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

const STUB_INFO_OUTPUT =
  `type Expr :: *
data Expr
  = Lit Int | Var String
  \t-- Defined at src/Expr/Syntax.hs:10:1`;

describe("handleArbitrary module_path pre-load (Fase 4)", () => {
  it("calls loadModule BEFORE infoOf when module_path is provided", async () => {
    const events: string[] = [];
    const session = createMockSession({
      loadModule: async (mp: string) => {
        events.push(`loadModule:${mp}`);
        return { output: "Ok, one module loaded.", success: true } as GhciResult;
      },
      infoOf: async (name: string) => {
        events.push(`infoOf:${name}`);
        return { output: STUB_INFO_OUTPUT, success: true } as GhciResult;
      },
    });

    const result = JSON.parse(
      await handleArbitrary(session, { type_name: "Expr", module_path: "src/Expr/Syntax.hs" })
    );
    expect(result.success).toBe(true);
    expect(events[0]).toBe("loadModule:src/Expr/Syntax.hs");
    expect(events[1]).toBe("infoOf:Expr");
  });

  it("skips pre-load when module_path is omitted", async () => {
    const events: string[] = [];
    const session = createMockSession({
      loadModule: async (mp: string) => {
        events.push(`loadModule:${mp}`);
        return { output: "", success: true } as GhciResult;
      },
      infoOf: async (name: string) => {
        events.push(`infoOf:${name}`);
        return { output: STUB_INFO_OUTPUT, success: true } as GhciResult;
      },
    });
    await handleArbitrary(session, { type_name: "Expr" });
    // No loadModule event — only infoOf.
    expect(events.filter((e) => e.startsWith("loadModule")).length).toBe(0);
    expect(events[0]).toBe("infoOf:Expr");
  });

  it("does not throw when loadModule itself fails — falls through to infoOf", async () => {
    const events: string[] = [];
    const session = createMockSession({
      loadModule: async () => {
        events.push("loadModule:thrown");
        throw new Error("compile error");
      },
      infoOf: async () => {
        events.push("infoOf");
        return { output: STUB_INFO_OUTPUT, success: true } as GhciResult;
      },
    });
    // Should not throw — the pre-load is best-effort.
    const result = JSON.parse(
      await handleArbitrary(session, { type_name: "Expr", module_path: "src/X.hs" })
    );
    expect(result.success).toBe(true);
    expect(events).toContain("loadModule:thrown");
    expect(events).toContain("infoOf");
  });
});
