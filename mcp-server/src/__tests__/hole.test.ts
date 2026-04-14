import { describe, it, expect } from "vitest";
import { handleHole } from "../tools/hole.js";
import { createMockSession } from "./helpers/mock-session.js";

// Simulated GHCi output for a typed hole
const ONE_HOLE_OUTPUT = `
src/Foo.hs:5:9: warning: [GHC-88464] [-Wtyped-holes]
    • Found hole: _result :: Int
      Or perhaps '_result' is mis-spelled, or not imported?
    • In the expression: _result
      In an equation for 'f': f x = _result
    • Relevant bindings include
        x :: Int (bound at src/Foo.hs:5:3)
      Valid hole fits include
        x :: Int (bound at src/Foo.hs:5:3)
        maxBound :: Int (imported from 'GHC.Enum')
`;

const TWO_HOLES_OUTPUT = `
src/Foo.hs:5:9: warning: [GHC-88464] [-Wtyped-holes]
    • Found hole: _a :: Int
    • In the expression: _a + _b
      In an equation for 'g': g = _a + _b
    • Relevant bindings include
      Valid hole fits include
        maxBound :: Int

src/Foo.hs:5:13: warning: [GHC-88464] [-Wtyped-holes]
    • Found hole: _b :: Int
    • In the expression: _a + _b
      In an equation for 'g': g = _a + _b
    • Relevant bindings include
      Valid hole fits include
        minBound :: Int
`;

describe("handleHole — unit (mock session)", () => {
  it("returns one hole with name and expectedType", async () => {
    const session = createMockSession({
      loadModule: { output: ONE_HOLE_OUTPUT, success: true },
    });
    const result = JSON.parse(
      await handleHole(session, { module_path: "src/Foo.hs" })
    );
    expect(result.success).toBe(true);
    expect(result.holes).toHaveLength(1);
    expect(result.holes[0].hole).toBe("_result");
    expect(result.holes[0].expectedType).toContain("Int");
  });

  it("returns fits for the hole", async () => {
    const session = createMockSession({
      loadModule: { output: ONE_HOLE_OUTPUT, success: true },
    });
    const result = JSON.parse(
      await handleHole(session, { module_path: "src/Foo.hs" })
    );
    expect(result.holes[0].validFits.length).toBeGreaterThan(0);
    expect(result.holes[0].validFits[0].name).toBeTruthy();
  });

  it("returns two holes when module has two", async () => {
    const session = createMockSession({
      loadModule: { output: TWO_HOLES_OUTPUT, success: true },
    });
    const result = JSON.parse(
      await handleHole(session, { module_path: "src/Foo.hs" })
    );
    expect(result.success).toBe(true);
    expect(result.holes).toHaveLength(2);
  });

  it("hole_name filter returns only the requested hole", async () => {
    const session = createMockSession({
      loadModule: { output: TWO_HOLES_OUTPUT, success: true },
    });
    const result = JSON.parse(
      await handleHole(session, { module_path: "src/Foo.hs", hole_name: "_a" })
    );
    expect(result.holes).toHaveLength(1);
    expect(result.holes[0].hole).toBe("_a");
  });

  it("returns empty holes array when no typed holes", async () => {
    const session = createMockSession({
      loadModule: { output: "Ok, one module loaded.", success: true },
    });
    const result = JSON.parse(
      await handleHole(session, { module_path: "src/Foo.hs" })
    );
    expect(result.success).toBe(true);
    expect(result.holes).toHaveLength(0);
  });

  it("module_path is required — missing returns error", async () => {
    const session = createMockSession();
    // @ts-expect-error intentionally missing module_path
    const result = JSON.parse(await handleHole(session, {}));
    expect(result.success).toBe(false);
    expect(result.error).toBeDefined();
  });

  it("relevant bindings are included in each hole", async () => {
    const session = createMockSession({
      loadModule: { output: ONE_HOLE_OUTPUT, success: true },
    });
    const result = JSON.parse(
      await handleHole(session, { module_path: "src/Foo.hs" })
    );
    const hole = result.holes[0];
    expect(Array.isArray(hole.relevantBindings)).toBe(true);
  });

  it("includes module_path in response", async () => {
    const session = createMockSession({
      loadModule: { output: ONE_HOLE_OUTPUT, success: true },
    });
    const result = JSON.parse(
      await handleHole(session, { module_path: "src/Foo.hs" })
    );
    expect(result.module_path).toBe("src/Foo.hs");
  });
});
