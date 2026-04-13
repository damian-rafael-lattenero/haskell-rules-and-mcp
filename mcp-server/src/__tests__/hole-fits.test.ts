import { describe, it, expect } from "vitest";
import { handleHoleFits } from "../tools/hole-fits.js";
import { parseHoleSummaries, parseTypedHoles } from "../parsers/hole-parser.js";
import { createMockSession } from "./helpers/mock-session.js";
import type { GhciResult } from "../ghci-session.js";

function makeHoleSession(loadOutput: string) {
  return createMockSession({
    execute: async (): Promise<GhciResult> => ({ output: "", success: true }),
    loadModule: { output: loadOutput, success: true },
  });
}

const SINGLE_HOLE_OUTPUT = `[1 of 1] Compiling Foo ( src/Foo.hs, interpreted )
src/Foo.hs:5:14: warning: [GHC-88464] [-Wtyped-holes]
    \u2022 Found hole: _ :: Int
    \u2022 In an equation for 'mystery': mystery xs = _
    \u2022 Relevant bindings include
        xs :: [Int] (bound at src/Foo.hs:5:9)
        mystery :: [Int] -> Int (bound at src/Foo.hs:5:1)
      Valid hole fits include
        maxBound :: forall a. Bounded a => a
          with maxBound @Int
          (imported from 'Prelude' at src/Foo.hs:1:8-16)
        minBound :: forall a. Bounded a => a
          with minBound @Int
          (imported from 'Prelude' at src/Foo.hs:1:8-16)
   |
5 | mystery xs = _
   |              ^
Ok, one module loaded.`;

const TWO_HOLES_OUTPUT = `src/Foo.hs:3:10: warning: [GHC-88464] [-Wtyped-holes]
    \u2022 Found hole: _a :: String
    \u2022 Relevant bindings include
        x :: Int (bound at src/Foo.hs:3:5)
      Valid hole fits include
        [] :: forall a. [a]
   |
3 | foo x = _a
   |          ^^
src/Foo.hs:6:12: warning: [GHC-88464] [-Wtyped-holes]
    \u2022 Found hole: _b :: Bool
    \u2022 Relevant bindings include
        y :: String (bound at src/Foo.hs:6:5)
      Valid hole fits include
        True :: Bool
        False :: Bool
   |
6 | bar y = _b
   |          ^^
Ok, one module loaded.`;

const NO_HOLES_OUTPUT = `[1 of 1] Compiling Foo ( src/Foo.hs, interpreted )
Ok, one module loaded.`;

const SUPPRESSED_FITS_OUTPUT = `src/Foo.hs:5:14: warning: [GHC-88464] [-Wtyped-holes]
    \u2022 Found hole: _ :: Int
    \u2022 Relevant bindings include
        x :: Int (bound at src/Foo.hs:5:5)
      Valid hole fits include
        x :: Int (bound at src/Foo.hs:5:5)
      (Some hole fits suppressed; use -fmax-valid-hole-fits=N)
   |
5 | foo x = _
   |         ^
Ok, one module loaded.`;

describe("handleHoleFits", () => {
  it("parses a single typed hole", async () => {
    const session = makeHoleSession(SINGLE_HOLE_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    expect(result.success).toBe(true);
    expect(result.holes).toHaveLength(1);

    const hole = result.holes[0];
    expect(hole.hole).toBe("_");
    expect(hole.expectedType).toBe("Int");
    expect(hole.location.file).toBe("src/Foo.hs");
    expect(hole.location.line).toBe(5);
    expect(hole.location.column).toBe(14);
  });

  it("extracts relevant bindings", async () => {
    const session = makeHoleSession(SINGLE_HOLE_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    const bindings = result.holes[0].relevantBindings;
    expect(bindings.length).toBeGreaterThanOrEqual(2);
    expect(bindings.some((b: any) => b.name === "xs" && b.type === "[Int]")).toBe(true);
    expect(bindings.some((b: any) => b.name === "mystery")).toBe(true);
  });

  it("extracts valid hole fits with specialization", async () => {
    const session = makeHoleSession(SINGLE_HOLE_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    const fits = result.holes[0].validFits;
    expect(fits.length).toBeGreaterThanOrEqual(2);
    expect(fits.some((f: any) => f.name === "maxBound")).toBe(true);
    const maxBound = fits.find((f: any) => f.name === "maxBound");
    expect(maxBound.specialization).toContain("maxBound @Int");
  });

  it("parses multiple holes", async () => {
    const session = makeHoleSession(TWO_HOLES_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    expect(result.holes).toHaveLength(2);
    expect(result.holes[0].hole).toBe("_a");
    expect(result.holes[0].expectedType).toBe("String");
    expect(result.holes[1].hole).toBe("_b");
    expect(result.holes[1].expectedType).toBe("Bool");
  });

  it("returns empty when no holes", async () => {
    const session = makeHoleSession(NO_HOLES_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    expect(result.success).toBe(true);
    expect(result.holes).toEqual([]);
    expect(result.summary).toContain("No typed holes");
  });

  it("detects suppressed fits", async () => {
    const session = makeHoleSession(SUPPRESSED_FITS_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    expect(result.holes).toHaveLength(1);
    expect(result.holes[0].suppressed).toBe(true);
  });

  it("includes summary with hole count", async () => {
    const session = makeHoleSession(TWO_HOLES_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    expect(result.summary).toBe("Found 2 typed hole(s)");
  });

  it("hole fits have source information", async () => {
    const session = makeHoleSession(SINGLE_HOLE_OUTPUT);
    const result = JSON.parse(await handleHoleFits(session, { module_path: "src/Foo.hs" }));
    const fit = result.holes[0].validFits.find((f: any) => f.name === "maxBound");
    expect(fit.source).toBeTruthy();
    expect(fit.source).toContain("imported from");
  });
});

// --- Tests for refinement hole fits (the "Valid refinement hole fits" variant) ---

const REFINEMENT_HOLE_OUTPUT = `src/Calc/Eval.hs:16:18: warning: [GHC-88464] [-Wtyped-holes]
    \u2022 Found hole: _ :: Either EvalError Double
    \u2022 In a case alternative: Var x -> _
    \u2022 Relevant bindings include
        x :: String (bound at src/Calc/Eval.hs:16:7)
        env :: Env (bound at src/Calc/Eval.hs:14:6)
      Valid refinement hole fits include
        Left (_ :: EvalError)
          where Left :: forall a b. a -> Either a b
          with Left @EvalError @Double
          (imported from 'Prelude' at src/Calc/Eval.hs:1:8-16)
        Right (_ :: Double)
          where Right :: forall a b. b -> Either a b
          with Right @EvalError @Double
          (imported from 'Prelude' at src/Calc/Eval.hs:1:8-16)
   |
16 |   Var x       -> _
   |                  ^
Ok, one module loaded.`;

describe("parseHoleSummaries — refinement fits", () => {
  it("parses 'Valid refinement hole fits include' correctly", () => {
    const holes = parseHoleSummaries(REFINEMENT_HOLE_OUTPUT);
    expect(holes).toHaveLength(1);
    expect(holes[0].expectedType).toBe("Either EvalError Double");
    expect(holes[0].topFits.length).toBeGreaterThanOrEqual(1);
  });

  it("extracts bindings before refinement fits section", () => {
    const holes = parseHoleSummaries(REFINEMENT_HOLE_OUTPUT);
    expect(holes[0].relevantBindings).toContainEqual(expect.stringContaining("x :: String"));
    expect(holes[0].relevantBindings).toContainEqual(expect.stringContaining("env :: Env"));
  });
});

describe("parseTypedHoles — refinement fits", () => {
  it("parses refinement fits into validFits", () => {
    const holes = parseTypedHoles(REFINEMENT_HOLE_OUTPUT);
    expect(holes).toHaveLength(1);
    expect(holes[0].validFits.length).toBeGreaterThanOrEqual(2);
    expect(holes[0].validFits.some((f) => f.name === "Left")).toBe(true);
    expect(holes[0].validFits.some((f) => f.name === "Right")).toBe(true);
  });

  it("extracts specialization for refinement fits", () => {
    const holes = parseTypedHoles(REFINEMENT_HOLE_OUTPUT);
    const leftFit = holes[0].validFits.find((f) => f.name === "Left");
    expect(leftFit).toBeDefined();
    expect(leftFit!.specialization).toContain("Left @EvalError @Double");
  });
});
