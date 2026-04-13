import { describe, it, expect, vi, beforeEach } from "vitest";
import { handleQuickCheckBatch, resetQuickCheckState } from "../tools/quickcheck.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("handleQuickCheckBatch", () => {
  beforeEach(() => {
    resetQuickCheckState();
  });

  it("returns empty results for empty properties array", async () => {
    const session = createMockSession();
    const result = JSON.parse(
      await handleQuickCheckBatch(session, { properties: [] })
    );
    expect(result).toEqual({ success: true, count: 0, results: [] });
  });

  it("runs multiple properties and aggregates results", async () => {
    const session = createMockSession({
      execute: vi.fn(async (cmd: string) => {
        if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
          return { output: "+++ OK, passed 100 tests.", success: true };
        }
        return { output: "", success: true };
      }),
    });

    const result = JSON.parse(
      await handleQuickCheckBatch(session, {
        properties: ["prop1", "prop2", "prop3"],
      })
    );
    expect(result.success).toBe(true);
    expect(result.count).toBe(3);
    expect(result.results).toHaveLength(3);
    expect(result.results[0].success).toBe(true);
  });

  it("reports allPassed=false when any property fails", async () => {
    let qcCallIndex = 0;
    const session = createMockSession({
      execute: vi.fn(async (cmd: string) => {
        if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
          qcCallIndex++;
          if (qcCallIndex === 1) {
            return { output: "+++ OK, passed 100 tests.", success: true };
          }
          return {
            output: "*** Failed! Falsifiable (after 3 tests and 0 shrinks):\n42\n",
            success: true,
          };
        }
        return { output: "", success: true };
      }),
    });

    const result = JSON.parse(
      await handleQuickCheckBatch(session, {
        properties: ["passing_prop", "failing_prop"],
      })
    );
    expect(result.success).toBe(false);
    expect(result.count).toBe(2);
    expect(result.results[0].success).toBe(true);
    expect(result.results[1].success).toBe(false);
  });

  it("includes error details for failing properties", async () => {
    const session = createMockSession({
      execute: vi.fn(async (cmd: string) => {
        if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
          return {
            output: "*** Failed! Falsifiable (after 5 tests and 2 shrinks):\n0\n\n",
            success: true,
          };
        }
        return { output: "", success: true };
      }),
    });

    const result = JSON.parse(
      await handleQuickCheckBatch(session, {
        properties: ["\\x -> x > (0 :: Int)"],
      })
    );
    expect(result.results[0].success).toBe(false);
    expect(result.results[0].counterexample).toBe("0");
  });
});
