import { describe, it, expect, vi } from "vitest";
import { checkArbitraryInstances, handleQuickCheck } from "../tools/quickcheck.js";

function createMockSession(responses: Record<string, { output: string; success: boolean }>) {
  const session = {
    execute: vi.fn(async (cmd: string) => {
      for (const [pattern, response] of Object.entries(responses)) {
        if (cmd.includes(pattern)) return response;
      }
      return { output: "", success: true };
    }),
    typeOf: vi.fn(async (name: string) => {
      if (responses[`:t ${name}`]) return responses[`:t ${name}`]!;
      return { output: "", success: false };
    }),
    loadModules: vi.fn(async () => {}),
    isAlive: () => true,
  };
  return session as any;
}

describe("checkArbitraryInstances", () => {
  it("detects missing Arbitrary instances", async () => {
    const session = createMockSession({
      "arbitrary :: Gen ParseError": {
        output: "No instance for (Arbitrary ParseError)",
        success: false,
      },
      "arbitrary :: Gen Pos": {
        output: "No instance for (Arbitrary Pos)",
        success: false,
      },
    });
    const missing = await checkArbitraryInstances(session, "mergeErrors :: ParseError -> ParseError -> ParseError");
    expect(missing).toContain("ParseError");
  });

  it("returns empty when all Arbitrary instances exist", async () => {
    const session = createMockSession({
      "arbitrary :: Gen ParseError": {
        output: "arbitrary :: Gen ParseError",
        success: true,
      },
    });
    const missing = await checkArbitraryInstances(session, "formatError :: ParseError -> String");
    expect(missing).toHaveLength(0);
  });

  it("skips base types that always have Arbitrary", async () => {
    const session = createMockSession({});
    const missing = await checkArbitraryInstances(session, "foo :: Int -> String -> Bool -> Char -> ()");
    expect(missing).toHaveLength(0);
    // session.execute should NOT have been called for base types
    expect(session.execute).not.toHaveBeenCalled();
  });

  it("extracts concrete types from complex signatures", async () => {
    const session = createMockSession({
      "arbitrary :: Gen Pos": {
        output: "arbitrary :: Gen Pos",
        success: true,
      },
    });
    const missing = await checkArbitraryInstances(session, "advancePos :: Pos -> Char -> Pos");
    expect(missing).toHaveLength(0);
    // Should have checked Pos (not Char, which is a base type)
    expect(session.execute).toHaveBeenCalledTimes(1);
    expect(session.execute).toHaveBeenCalledWith(expect.stringContaining("Gen Pos"));
  });

  it("returns empty for type variable only signatures", async () => {
    const session = createMockSession({});
    const missing = await checkArbitraryInstances(session, "id :: a -> a");
    expect(missing).toHaveLength(0);
  });
});

describe("handleQuickCheck suggest mode", () => {
  it("returns missingArbitrary when instances are missing", async () => {
    const session = createMockSession({
      ":t mergeErrors": {
        output: "mergeErrors :: ParseError -> ParseError -> ParseError",
        success: true,
      },
      "arbitrary :: Gen ParseError": {
        output: "No instance for (Arbitrary ParseError)",
        success: false,
      },
      "import Test.QuickCheck": { output: "", success: true },
    });

    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "suggest",
        function_name: "mergeErrors",
      })
    );

    expect(result.mode).toBe("suggest");
    expect(result.missingArbitrary).toContain("ParseError");
    expect(result.suggestedProperties).toHaveLength(0);
    expect(result._guidance).toBeDefined();
    expect(result._guidance[0]).toContain("ghci_arbitrary");
  });

  it("provides specific rejection reasons for Arbitrary", async () => {
    // All Arbitrary instances exist, but a property fails type-checking with specific reason
    const session = createMockSession({
      ":t mergeErrors": {
        output: "mergeErrors :: ParseError -> ParseError -> ParseError",
        success: true,
      },
      "arbitrary :: Gen ParseError": {
        output: "arbitrary :: Gen ParseError",
        success: true,
      },
      "import Test.QuickCheck": { output: "", success: true },
      ":t (": {
        output: "No instance for (Eq ParseError)",
        success: false,
      },
    });

    const result = JSON.parse(
      await handleQuickCheck(session, {
        property: "suggest",
        function_name: "mergeErrors",
      })
    );

    // Some properties may be rejected with specific reasons
    if (result.rejectedProperties && result.rejectedProperties.length > 0) {
      const reasons = result.rejectedProperties.map((r: any) => r.reason);
      // Should NOT have generic "Property doesn't type-check" if specific reason available
      const hasSpecific = reasons.some(
        (r: string) => r.includes("Missing") || r.includes("not in scope") || r.includes("Type mismatch")
      );
      // At minimum, reasons should not all be the old generic message
      expect(reasons.every((r: string) => r === "Property doesn't type-check")).toBe(false);
    }
  });
});
