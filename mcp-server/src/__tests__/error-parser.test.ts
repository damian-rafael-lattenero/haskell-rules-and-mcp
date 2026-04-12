import { describe, it, expect } from "vitest";
import { parseGhcErrors } from "../parsers/error-parser.js";

// Real GHC 9.12.2 output captured during session 7 testing.

const UNUSED_IMPORT_WARNING = `src/HM/TestWarn.hs:2:1-23: warning: [GHC-66111] [-Wunused-imports]
    The import of \u2018Data.List\u2019 is redundant
      except perhaps to import instances from \u2018Data.List\u2019
    To import instances alone, use: import Data.List()
  |
2 | import Data.List (sort)  -- unused import
  | ^^^^^^^^^^^^^^^^^^^^^^^`;

const MISSING_SIG_WARNING = `src/HM/TestWarn.hs:3:1-3: warning: [GHC-38417] [-Wmissing-signatures]
    Top-level binding with no type signature:
      foo :: forall {a}. Num a => a -> a
  |
3 | foo x = x + 1            -- missing signature
  | ^^^`;

const UNUSED_MATCH_WARNING = `src/HM/TestWarn.hs:5:5: warning: [GHC-40910] [-Wunused-matches]
    Defined but not used: \u2018x\u2019
  |
5 | bar x = 42               -- unused match
  |     ^`;

const INCOMPLETE_PATTERNS_WARNING = `src/HM/TestWarn.hs:7:1-16: warning: [GHC-62161] [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for \u2018baz\u2019:
        Patterns of type \u2018Maybe Int\u2019 not matched: Nothing
  |
7 | baz (Just n) = n          -- incomplete patterns
  | ^^^^^^^^^^^^^^^^`;

const TYPE_ERROR = `src/HM/TestErr.hs:3:7-10: error: [GHC-83865]
    \u2022 Couldn\u2019t match expected type \u2018Int\u2019 with actual type \u2018Bool\u2019
    \u2022 In the expression: True
      In an equation for \u2018foo\u2019: foo = True
  |
3 | foo = True
  |       ^^^^`;

const MULTI_WARNING_OUTPUT = `[1 of 1] Compiling HM.TestWarn      ( src/HM/TestWarn.hs, interpreted )
${UNUSED_IMPORT_WARNING}

${MISSING_SIG_WARNING}

${UNUSED_MATCH_WARNING}

${INCOMPLETE_PATTERNS_WARNING}

Ok, one module loaded.`;

describe("parseGhcErrors", () => {
  it("returns empty array for empty input", () => {
    expect(parseGhcErrors("")).toEqual([]);
  });

  it("returns empty array for noise-only output", () => {
    const noise = `[1 of 10] Compiling HM.Syntax ( src/HM/Syntax.hs, interpreted )
Ok, 10 modules loaded.`;
    expect(parseGhcErrors(noise)).toEqual([]);
  });

  it("parses a single warning with full multiline body", () => {
    const [w] = parseGhcErrors(UNUSED_IMPORT_WARNING);
    expect(w).toBeDefined();
    expect(w!.file).toBe("src/HM/TestWarn.hs");
    expect(w!.line).toBe(2);
    expect(w!.column).toBe(1);
    expect(w!.endColumn).toBe(23);
    expect(w!.severity).toBe("warning");
    expect(w!.code).toBe("GHC-66111");
    expect(w!.warningFlag).toBe("-Wunused-imports");
    // Key: message contains the FULL body, not just one line
    expect(w!.message).toContain("The import of");
    expect(w!.message).toContain("is redundant");
    expect(w!.message).toContain("except perhaps");
  });

  it("extracts warningFlag for each warning type", () => {
    const parsed = parseGhcErrors(MULTI_WARNING_OUTPUT);
    const flags = parsed.map((e) => e.warningFlag);
    expect(flags).toContain("-Wunused-imports");
    expect(flags).toContain("-Wmissing-signatures");
    expect(flags).toContain("-Wunused-matches");
    expect(flags).toContain("-Wincomplete-patterns");
  });

  it("captures full body for missing-signatures warning", () => {
    const [w] = parseGhcErrors(MISSING_SIG_WARNING);
    expect(w!.warningFlag).toBe("-Wmissing-signatures");
    expect(w!.message).toContain("Top-level binding with no type signature:");
    expect(w!.message).toContain("foo :: forall {a}. Num a => a -> a");
  });

  it("captures full body for unused-matches warning", () => {
    const [w] = parseGhcErrors(UNUSED_MATCH_WARNING);
    expect(w!.warningFlag).toBe("-Wunused-matches");
    expect(w!.message).toContain("Defined but not used:");
  });

  it("captures full body for incomplete-patterns warning", () => {
    const [w] = parseGhcErrors(INCOMPLETE_PATTERNS_WARNING);
    expect(w!.warningFlag).toBe("-Wincomplete-patterns");
    expect(w!.message).toContain("Pattern match(es) are non-exhaustive");
    expect(w!.message).toContain("not matched: Nothing");
  });

  it("parses multiple warnings from combined output", () => {
    const parsed = parseGhcErrors(MULTI_WARNING_OUTPUT);
    expect(parsed).toHaveLength(4);
    expect(parsed[0]!.code).toBe("GHC-66111");
    expect(parsed[1]!.code).toBe("GHC-38417");
    expect(parsed[2]!.code).toBe("GHC-40910");
    expect(parsed[3]!.code).toBe("GHC-62161");
  });

  it("extracts expected/actual from type error with Unicode quotes", () => {
    const [e] = parseGhcErrors(TYPE_ERROR);
    expect(e).toBeDefined();
    expect(e!.code).toBe("GHC-83865");
    expect(e!.severity).toBe("error");
    expect(e!.expected).toBe("Int");
    expect(e!.actual).toBe("Bool");
  });

  it("extracts context from type error", () => {
    const [e] = parseGhcErrors(TYPE_ERROR);
    expect(e!.context).toBe("True");
  });

  it("parses endColumn from range format", () => {
    const [w] = parseGhcErrors(UNUSED_IMPORT_WARNING);
    expect(w!.endColumn).toBe(23);
  });

  it("handles warning without endColumn", () => {
    const [w] = parseGhcErrors(UNUSED_MATCH_WARNING);
    expect(w!.endColumn).toBeUndefined();
  });

  it("does not confuse source line references for error headers", () => {
    // The "2 | import Data.List" line contains digits but should NOT be parsed as a header
    const parsed = parseGhcErrors(UNUSED_IMPORT_WARNING);
    expect(parsed).toHaveLength(1);
  });
});
