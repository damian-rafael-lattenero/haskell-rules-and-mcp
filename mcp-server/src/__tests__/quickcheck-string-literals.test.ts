import { describe, it, expect } from "vitest";
import { handleQuickCheck } from "../tools/quickcheck.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

describe("QuickCheck with string literals", () => {
  it("defines property via let-binding before running", async () => {
    const executedCmds: string[] = [];
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        executedCmds.push(cmd);
        if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
          return { output: "+++ OK, passed 100 tests.", success: true };
        }
        return { output: "", success: true };
      },
    });

    await handleQuickCheck(session, {
      property: '\\c -> runParser (satisfy "digit" isDigit) [c] == Right c',
    });

    // Should have a let-binding BEFORE the quickCheck command
    const letCmd = executedCmds.find((c) => c.startsWith("let __qcProp"));
    expect(letCmd).toBeDefined();
    expect(letCmd).toContain('satisfy "digit"');

    // quickCheck should reference the binding, not inline the property
    const qcCmd = executedCmds.find((c) => c.includes("quickCheckWith"));
    expect(qcCmd).toBeDefined();
    expect(qcCmd).toContain("__qcProp");
    expect(qcCmd).not.toContain("satisfy");
  });

  it("simple property without quotes still works via let-binding", async () => {
    const executedCmds: string[] = [];
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        executedCmds.push(cmd);
        if (cmd.includes("quickCheckWith")) {
          return { output: "+++ OK, passed 100 tests.", success: true };
        }
        return { output: "", success: true };
      },
    });

    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "\\x -> x == (x :: Int)",
      })
    );
    expect(result.success).toBe(true);
    expect(result.passed).toBe(100);

    // Still uses let-binding even for simple properties
    const letCmd = executedCmds.find((c) => c.startsWith("let __qcProp"));
    expect(letCmd).toBeDefined();
  });

  it("handles properties with single quotes", async () => {
    const executedCmds: string[] = [];
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        executedCmds.push(cmd);
        if (cmd.includes("quickCheckWith")) {
          return { output: "+++ OK, passed 100 tests.", success: true };
        }
        return { output: "", success: true };
      },
    });

    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "\\c -> c == 'a' || c /= 'a'",
      })
    );
    expect(result.success).toBe(true);

    const letCmd = executedCmds.find((c) => c.startsWith("let __qcProp"));
    expect(letCmd).toBeDefined();
    expect(letCmd).toContain("'a'");
  });
});
