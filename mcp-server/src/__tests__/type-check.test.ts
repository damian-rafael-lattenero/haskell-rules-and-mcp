import { describe, it, expect } from "vitest";
import { handleTypeCheck } from "../tools/type-check.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("handleTypeCheck", () => {
  it("returns parsed type for valid expression", async () => {
    const session = createMockSession({
      typeOf: { output: "map :: (a -> b) -> [a] -> [b]", success: true },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "map" }));
    expect(result.success).toBe(true);
    expect(result.expression).toBe("map");
    expect(result.type).toBe("(a -> b) -> [a] -> [b]");
  });

  it("returns error for failed type lookup", async () => {
    const session = createMockSession({
      typeOf: { output: "Not in scope: 'foo'", success: false },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "foo" }));
    expect(result.success).toBe(false);
    expect(result.error).toContain("Not in scope");
  });

  it("detects deferred-out-of-scope-variables (Bug Fix 1)", async () => {
    const session = createMockSession({
      typeOf: {
        output: "<interactive>:1:1-19: warning: [GHC-88464] [-Wdeferred-out-of-scope-variables]\n    Variable not in scope: nonExistent\nnonExistent :: p",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "nonExistent" }));
    expect(result.success).toBe(false);
    expect(result.error).toContain("Variable not in scope");
  });

  it("detects 'Variable not in scope' without flag name", async () => {
    const session = createMockSession({
      typeOf: {
        output: "Variable not in scope: xyz\nxyz :: p",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "xyz" }));
    expect(result.success).toBe(false);
  });

  it("returns raw output when type cannot be parsed", async () => {
    const session = createMockSession({
      typeOf: { output: "some unexpected output format", success: true },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "x" }));
    expect(result.success).toBe(true);
    expect(result.raw).toBe("some unexpected output format");
  });

  it("handles multiline type output", async () => {
    const session = createMockSession({
      typeOf: {
        output: "foldr\n  :: (a -> b -> b) -> b -> [a] -> b",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "foldr" }));
    expect(result.success).toBe(true);
    expect(result.type).toContain("(a -> b -> b) -> b -> [a] -> b");
  });

  it("handles operator expressions", async () => {
    const session = createMockSession({
      typeOf: { output: "(+) :: Num a => a -> a -> a", success: true },
    });
    const result = JSON.parse(await handleTypeCheck(session, { expression: "(+)" }));
    expect(result.success).toBe(true);
    expect(result.expression).toBe("(+)");
  });
});
