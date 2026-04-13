import { describe, it, expect, vi, beforeEach } from "vitest";
import { parseScopeError } from "../parsers/quickcheck-parser.js";
import { runPropertyWithAutoResolve, resetQuickCheckState, type LoadAllFn } from "../tools/quickcheck.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("parseScopeError", () => {
  it("detects 'not in scope' with quoted name", () => {
    const output =
      "<interactive>:34:5: error: [GHC-88464]\n" +
      "    Variable not in scope: runParser :: Parser Char -> t0";
    const result = parseScopeError(output);
    expect(result).toEqual({ type: "not-in-scope", names: ["runParser"] });
  });

  it("detects multiple 'not in scope' names", () => {
    const output =
      "Variable not in scope: 'runParser' :: a\n" +
      "Variable not in scope: 'mkState' :: b";
    const result = parseScopeError(output);
    expect(result).toEqual({
      type: "not-in-scope",
      names: ["runParser", "mkState"],
    });
  });

  it("detects ambiguous occurrence", () => {
    const output =
      "<interactive>:34:108: error: [GHC-87543]\n" +
      "    Ambiguous occurrence \u2018Success\u2019.\n" +
      "    It could refer to\n" +
      "       either 'Parser.Core.Success'\n" +
      "           or 'Test.QuickCheck.Success'";
    const result = parseScopeError(output);
    expect(result).toEqual({ type: "ambiguous", names: ["Success"] });
  });

  it("deduplicates ambiguous names", () => {
    const output =
      "Ambiguous occurrence 'Success'.\n" +
      "Ambiguous occurrence 'Success'.";
    const result = parseScopeError(output);
    expect(result).toEqual({ type: "ambiguous", names: ["Success"] });
  });

  it("returns null for non-scope errors", () => {
    const output = "+++ OK, passed 100 tests.\n";
    expect(parseScopeError(output)).toBeNull();
  });

  it("returns null for empty output", () => {
    expect(parseScopeError("")).toBeNull();
  });
});

describe("runPropertyWithAutoResolve", () => {
  beforeEach(() => {
    resetQuickCheckState();
  });

  it("returns result directly when property succeeds", async () => {
    const session = createMockSession({
      execute: vi.fn().mockResolvedValue({
        output: "+++ OK, passed 100 tests.",
        success: true,
      }),
    });

    const { result, autoResolved } = await runPropertyWithAutoResolve(
      session,
      "quickCheckWith (stdArgs { maxSuccess = 100 }) (\\x -> x == (x :: Int))"
    );
    expect(result.output).toContain("+++ OK");
    expect(autoResolved).toBe(false);
    // Should only be called once (no retries)
    expect(session.execute).toHaveBeenCalledTimes(1);
  });

  it("retries with load_all on 'not in scope' error", async () => {
    let callCount = 0;
    const session = createMockSession({
      execute: vi.fn(async () => {
        callCount++;
        if (callCount === 1) {
          return {
            output: "Variable not in scope: 'runParser'",
            success: false,
          };
        }
        // After reimport (call 2), and then retry (call 3)
        return { output: "+++ OK, passed 100 tests.", success: true };
      }),
      loadModules: vi.fn().mockResolvedValue({
        output: "Ok, 3 modules loaded.",
        success: true,
      }),
    });

    const mockLoadAll: LoadAllFn = vi.fn(async (s) => {
      await s.loadModules(["src/A.hs"], ["A"]);
      return true;
    });

    const { result, autoResolved } = await runPropertyWithAutoResolve(
      session,
      "some property",
      mockLoadAll
    );
    expect(autoResolved).toBe(true);
    expect(result.output).toContain("+++ OK");
    expect(mockLoadAll).toHaveBeenCalled();
  });

  it("retries with hiding on 'Ambiguous occurrence' error", async () => {
    let callCount = 0;
    const session = createMockSession({
      execute: vi.fn(async () => {
        callCount++;
        if (callCount === 1) {
          return {
            output: "Ambiguous occurrence \u2018Success\u2019.",
            success: false,
          };
        }
        // After reimport with hiding (call 2) and retry (call 3)
        return { output: "+++ OK, passed 100 tests.", success: true };
      }),
    });

    const { result, autoResolved } = await runPropertyWithAutoResolve(
      session,
      "some property"
    );
    expect(autoResolved).toBe(true);
    expect(result.output).toContain("+++ OK");
    // Verify reimport was called with hiding
    const executeCalls = (session.execute as ReturnType<typeof vi.fn>).mock.calls;
    const hidingCall = executeCalls.find(
      (c: unknown[]) => typeof c[0] === "string" && c[0].includes("hiding")
    );
    expect(hidingCall).toBeTruthy();
    expect(hidingCall![0]).toContain("hiding (Success)");
  });

  it("stops after MAX_RETRIES (2) attempts", async () => {
    const session = createMockSession({
      execute: vi.fn().mockResolvedValue({
        output: "Ambiguous occurrence \u2018Foo\u2019.",
        success: false,
      }),
    });

    const { result, autoResolved } = await runPropertyWithAutoResolve(
      session,
      "some property"
    );
    // Still ambiguous after retries
    expect(result.output).toContain("Ambiguous");
    expect(autoResolved).toBe(true);
    // 1 initial + 2 reimport commands + 2 retries = 5
    expect((session.execute as ReturnType<typeof vi.fn>).mock.calls.length).toBeLessThanOrEqual(5);
  });

  it("does not retry 'not in scope' without projectDir", async () => {
    const session = createMockSession({
      execute: vi.fn().mockResolvedValue({
        output: "Variable not in scope: 'foo'",
        success: false,
      }),
    });

    const { result, autoResolved } = await runPropertyWithAutoResolve(
      session,
      "some property"
      // no projectDir
    );
    expect(autoResolved).toBe(false);
    expect(result.output).toContain("not in scope");
    expect(session.execute).toHaveBeenCalledTimes(1);
  });
});
