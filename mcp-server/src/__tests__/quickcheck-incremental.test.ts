import { describe, it, expect } from "vitest";
import { handleQuickCheck } from "../tools/quickcheck.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

describe("handleQuickCheck — incremental mode", () => {
  function makeQCSession(qcOutput: string) {
    return createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.includes("import Test.QuickCheck")) {
          return { output: "", success: true };
        }
        if (cmd.includes("quickCheckWith") || cmd.includes("verboseCheckWith")) {
          return { output: qcOutput, success: true };
        }
        return { output: "", success: true };
      },
    });
  }

  it("includes incremental flag in response when incremental=true", async () => {
    const session = makeQCSession("+++ OK, passed 100 tests.\n");
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "\\x -> x == (x :: Int)",
        incremental: true,
      })
    );
    expect(result.incremental).toBe(true);
    expect(result.hint).toContain("passed");
  });

  it("includes failure hint when incremental property fails", async () => {
    const session = makeQCSession(
      "*** Failed! Falsifiable (after 3 tests):\n0\n"
    );
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "\\x -> x > 0",
        incremental: true,
      })
    );
    expect(result.incremental).toBe(true);
    expect(result.hint).toContain("FAILED");
  });

  it("does not include incremental flag when not set", async () => {
    const session = makeQCSession("+++ OK, passed 100 tests.\n");
    const result = JSON.parse(
      await handleQuickCheck(session, { property: "\\x -> x == (x :: Int)" })
    );
    expect(result.incremental).toBeUndefined();
  });
});

describe("handleQuickCheck — suggest mode", () => {
  function makeSuggestSession(typeOutput: string) {
    return createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.includes("import Test.QuickCheck")) {
          return { output: "", success: true };
        }
        return { output: "", success: true };
      },
      typeOf: { output: typeOutput, success: true },
    });
  }

  it("suggests properties for 'apply' based on type", async () => {
    const session = makeSuggestSession("apply :: Subst -> Type -> Type");
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "suggest",
        function_name: "apply",
      })
    );
    expect(result.mode).toBe("suggest");
    expect(result.function).toBe("apply");
    expect(result.suggestedProperties.length).toBeGreaterThan(0);
    expect(result.suggestedProperties[0].law).toBe("identity");
  });

  it("suggests properties for 'unify' with Either return", async () => {
    const session = makeSuggestSession("unify :: Type -> Type -> Either TypeError Subst");
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "suggest",
        function_name: "unify",
      })
    );
    expect(result.suggestedProperties.length).toBeGreaterThan(0);
    expect(result.suggestedProperties.some(
      (p: { law: string }) => p.law === "correctness"
    )).toBe(true);
  });

  it("suggests properties for 'composeSubst'", async () => {
    const session = makeSuggestSession("composeSubst :: Subst -> Subst -> Subst");
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "suggest",
        function_name: "composeSubst",
      })
    );
    expect(result.suggestedProperties.some(
      (p: { law: string }) => p.law.includes("composition")
    )).toBe(true);
  });

  it("returns empty suggestions for unknown function type", async () => {
    const session = makeSuggestSession("foo :: IO ()");
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "suggest",
        function_name: "foo",
      })
    );
    expect(result.mode).toBe("suggest");
    expect(result.suggestedProperties).toHaveLength(0);
    expect(result.hint).toContain("No automatic suggestions");
  });

  it("includes function type in response", async () => {
    const session = makeSuggestSession("map :: (a -> b) -> [a] -> [b]");
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "suggest",
        function_name: "map",
      })
    );
    expect(result.type).toContain("(a -> b)");
  });
});

describe("handleQuickCheck — retry on stale output", () => {
  it("retries when first execute returns non-QC output", async () => {
    let callCount = 0;
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.includes("import Test.QuickCheck")) {
          return { output: "", success: true };
        }
        if (cmd.includes("quickCheckWith")) {
          callCount++;
          if (callCount === 1) {
            // Simulate stale output from previous command
            return { output: '"flush"', success: true };
          }
          return { output: "+++ OK, passed 100 tests.\n", success: true };
        }
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(
      await handleQuickCheck(session, { property: "\\x -> x == (x :: Int)" })
    );
    expect(result.success).toBe(true);
    expect(result.passed).toBe(100);
  });

  it("does not retry when output contains QC markers", async () => {
    let callCount = 0;
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.includes("import Test.QuickCheck")) {
          return { output: "", success: true };
        }
        if (cmd.includes("quickCheckWith")) {
          callCount++;
          return { output: "+++ OK, passed 50 tests.\n", success: true };
        }
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(
      await handleQuickCheck(session, { property: "\\x -> True" })
    );
    expect(result.success).toBe(true);
    expect(callCount).toBe(1); // No retry — QC output was valid
  });
});

describe("handleQuickCheck — lambda escaping normalization (Bug 7)", () => {
  it("wraps bare lambda in parens via let-binding", async () => {
    let capturedLetCmd = "";
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.includes("import Test.QuickCheck")) {
          return { output: "", success: true };
        }
        if (cmd.startsWith("let __qcProp")) {
          capturedLetCmd = cmd;
          return { output: "", success: true };
        }
        if (cmd.includes("quickCheckWith")) {
          return { output: "+++ OK, passed 100 tests.\n", success: true };
        }
        return { output: "", success: true };
      },
    });
    await handleQuickCheck(session, { property: "\\pos c -> True" });
    // The lambda should be wrapped in parens inside the let-binding
    expect(capturedLetCmd).toContain("(\\pos c -> True)");
  });

  it("compilation error in property is flagged, not counted as logic failure", async () => {
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.includes("import Test.QuickCheck")) {
          return { output: "", success: true };
        }
        if (cmd.includes("quickCheckWith")) {
          return {
            output: "<interactive>:1:1: error:\n    Not in scope: 'nonExistent'\n",
            success: false,
          };
        }
        return { output: "", success: true };
      },
    });
    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "\\x -> nonExistent x == (x :: Int)",
        incremental: true,
      })
    );
    expect(result.success).toBe(false);
    expect(result.compilationError).toBe(true);
    expect(result.incremental).toBe(true);
  });

  it("does not double-wrap already parenthesized lambda", async () => {
    let capturedLetCmd = "";
    const session = createMockSession({
      execute: async (cmd: string): Promise<GhciResult> => {
        if (cmd.includes("import Test.QuickCheck")) {
          return { output: "", success: true };
        }
        if (cmd.startsWith("let __qcProp")) {
          capturedLetCmd = cmd;
          return { output: "", success: true };
        }
        if (cmd.includes("quickCheckWith")) {
          return { output: "+++ OK, passed 100 tests.\n", success: true };
        }
        return { output: "", success: true };
      },
    });
    await handleQuickCheck(session, { property: "(\\x -> x == (x :: Int))" });
    // Should NOT double-wrap in the let-binding
    expect(capturedLetCmd).not.toContain("((\\");
    expect(capturedLetCmd).toContain("(\\x -> x == (x :: Int))");
  });
});
