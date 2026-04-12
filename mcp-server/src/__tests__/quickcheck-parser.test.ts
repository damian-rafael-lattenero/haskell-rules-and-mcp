import { describe, it, expect } from "vitest";
import { parseQuickCheckOutput } from "../tools/quickcheck.js";

const PROP = "\\xs -> reverse (reverse xs) == (xs :: [Int])";

describe("parseQuickCheckOutput", () => {
  it("parses successful run (100 tests)", () => {
    const output = "+++ OK, passed 100 tests.\n";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(true);
    expect(result.passed).toBe(100);
    expect(result.property).toBe(PROP);
  });

  it("parses successful run (1 test, singular)", () => {
    const output = "+++ OK, passed 1 test.\n";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(true);
    expect(result.passed).toBe(1);
  });

  it("parses successful run (500 tests)", () => {
    const output = "+++ OK, passed 500 tests.\n";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(true);
    expect(result.passed).toBe(500);
  });

  it("parses failure with counterexample", () => {
    const output =
      "*** Failed! Falsifiable (after 1 test):\n" + "0\n\n";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(false);
    expect(result.passed).toBe(0);
    expect(result.counterexample).toBe("0");
  });

  it("parses failure with shrinks", () => {
    const output =
      "*** Failed! Falsifiable (after 12 tests and 5 shrinks):\n" +
      "3\n-2\n\n";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(false);
    expect(result.passed).toBe(11);
    expect(result.shrinks).toBe(5);
    expect(result.counterexample).toContain("3");
  });

  it("parses failure with 1 shrink (singular)", () => {
    const output =
      "*** Failed! Falsifiable (after 5 tests and 1 shrink):\n" +
      "42\n\n";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(false);
    expect(result.passed).toBe(4);
    expect(result.shrinks).toBe(1);
  });

  it("parses exception", () => {
    const output =
      "*** Failed! Exception: 'Prelude.head: empty list' (after 1 test):\n()";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(false);
    expect(result.error).toContain("Prelude.head: empty list");
  });

  it("parses exception with Unicode quotes", () => {
    const output =
      "*** Failed! Exception: \u2018Prelude.head: empty list\u2019 (after 1 test):\n()";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(false);
    expect(result.error).toContain("Prelude.head: empty list");
  });

  it("returns fallback error for garbage output", () => {
    const output = "some random garbage\nthat is not quickcheck output";
    const result = parseQuickCheckOutput(output, PROP);
    expect(result.success).toBe(false);
    expect(result.error).toContain("Couldn't parse QuickCheck output");
  });

  it("returns fallback error for empty output", () => {
    const result = parseQuickCheckOutput("", PROP);
    expect(result.success).toBe(false);
    expect(result.error).toContain("Couldn't parse QuickCheck output");
  });

  it("preserves property string in result", () => {
    const output = "+++ OK, passed 100 tests.\n";
    const result = parseQuickCheckOutput(output, "my custom prop");
    expect(result.property).toBe("my custom prop");
  });
});
