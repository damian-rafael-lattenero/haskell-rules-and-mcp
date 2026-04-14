import { describe, it, expect, vi } from "vitest";
import { handleFlags } from "../tools/flags.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("handleFlags — set", () => {
  it("calls :set with the provided flags", async () => {
    const session = createMockSession({
      execute: async (cmd: string) => {
        if (cmd.includes(":set")) return { output: "", success: true };
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(
      await handleFlags(session, { action: "set", flags: "-XOverloadedStrings" })
    );
    expect(result.success).toBe(true);
    expect(result.action).toBe("set");
    expect(result.flags).toBe("-XOverloadedStrings");
  });

  it("returns error when flags param is missing", async () => {
    const session = createMockSession();
    const result = JSON.parse(await handleFlags(session, { action: "set" }));
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it("set sends :set command to session", async () => {
    const executeSpy = vi.fn(async (cmd: string) => {
      return { output: "", success: true };
    });
    const session = { ...createMockSession(), execute: executeSpy };
    await handleFlags(session, { action: "set", flags: "-Wall" });
    expect(executeSpy).toHaveBeenCalledWith(expect.stringContaining(":set"));
    expect(executeSpy).toHaveBeenCalledWith(expect.stringContaining("-Wall"));
  });
});

describe("handleFlags — unset", () => {
  it("calls :unset with the provided flags", async () => {
    const session = createMockSession();
    const result = JSON.parse(
      await handleFlags(session, { action: "unset", flags: "-XOverloadedStrings" })
    );
    expect(result.success).toBe(true);
    expect(result.action).toBe("unset");
  });

  it("returns error when flags param is missing for unset", async () => {
    const session = createMockSession();
    const result = JSON.parse(await handleFlags(session, { action: "unset" }));
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });
});

describe("handleFlags — list", () => {
  it("executes :show language and returns flags array", async () => {
    const SHOW_LANGUAGE_OUTPUT = `base language is Haskell2010
with the following modifiers:
  -XNoMonomorphismRestriction
  -XNondecreasingIndentation
  -XExtendedDefaultRules`;

    const session = createMockSession({
      execute: async (cmd: string) => {
        if (cmd.includes(":show language")) {
          return { output: SHOW_LANGUAGE_OUTPUT, success: true };
        }
        return { output: "", success: true };
      },
    });

    const result = JSON.parse(await handleFlags(session, { action: "list" }));
    expect(result.success).toBe(true);
    expect(Array.isArray(result.flags)).toBe(true);
    expect(result.flags.some((f: string) => f.includes("Haskell"))).toBe(true);
  });

  it("returns empty flags on minimal output", async () => {
    const session = createMockSession({
      execute: async () => ({ output: "No language pragmas in scope.", success: true }),
    });
    const result = JSON.parse(await handleFlags(session, { action: "list" }));
    expect(result.success).toBe(true);
    expect(Array.isArray(result.flags)).toBe(true);
  });
});

describe("handleFlags — unknown action", () => {
  it("returns error for unknown action", async () => {
    const session = createMockSession();
    const result = JSON.parse(await handleFlags(session, { action: "teleport" }));
    expect(result.success).toBe(false);
    expect(result.error).toMatch(/unknown action/i);
  });
});
