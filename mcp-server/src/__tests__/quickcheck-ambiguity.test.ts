/**
 * OBS-3 coverage: `parseAmbiguousTypeVariable` extracts the GHC
 * "Ambiguous type variable ‘a0’ arising from a use of ‘==’" pattern
 * produced when a QuickCheck property's return type cannot be inferred
 * under -fdefer-type-errors.
 */
import { describe, it, expect } from "vitest";
import { parseAmbiguousTypeVariable } from "../parsers/quickcheck-parser.js";

const REAL_GHC_OUTPUT = `
<interactive>:77:50: error: [GHC-39999]
    • Ambiguous type variable ‘a0’ arising from a use of ‘==’
      prevents the constraint ‘(Eq a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
`;

describe("parseAmbiguousTypeVariable (OBS-3)", () => {
  it("extracts the variable name and the triggering operator", () => {
    const r = parseAmbiguousTypeVariable(REAL_GHC_OUTPUT);
    expect(r).not.toBeNull();
    expect(r?.ambiguousVar).toBe("a0");
    expect(r?.suggestion).toContain("ambiguous 'a0'");
    expect(r?.suggestion).toContain("'=='");
    expect(r?.suggestion).toContain("type annotation");
  });

  it("suggests the eval-idiom annotation verbatim", () => {
    const r = parseAmbiguousTypeVariable(REAL_GHC_OUTPUT);
    expect(r?.suggestion).toContain("Either Error Int");
    expect(r?.suggestion).toContain("eval env e");
  });

  it("returns null on output without the ambiguity error", () => {
    expect(parseAmbiguousTypeVariable("Just a normal error\n")).toBeNull();
    expect(parseAmbiguousTypeVariable("")).toBeNull();
  });

  it("returns null when the error mentions ambiguous but not 'type variable'", () => {
    // This is a different error (ambiguous *occurrence* = name conflict),
    // handled by `parseScopeError` — must NOT match here.
    expect(
      parseAmbiguousTypeVariable("Ambiguous occurrence ‘lookup’\n")
    ).toBeNull();
  });

  it("handles the error embedded in surrounding GHCi noise", () => {
    const noisy = `
      Loading [1 of 4] Expr.Syntax
      ${REAL_GHC_OUTPUT}
      [2 of 4] Expr.Eval
      (deferred type error)
    `;
    expect(parseAmbiguousTypeVariable(noisy)).not.toBeNull();
  });

  it("handles different operator and variable names", () => {
    const custom = "Ambiguous type variable ‘t1’ arising from a use of ‘show’\n";
    const r = parseAmbiguousTypeVariable(custom);
    expect(r?.ambiguousVar).toBe("t1");
    expect(r?.suggestion).toContain("'show'");
  });
});
