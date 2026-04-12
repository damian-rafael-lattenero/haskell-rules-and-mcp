import { describe, it, expect } from "vitest";
import { parseEvalOutput } from "../parsers/eval-output-parser.js";

describe("parseEvalOutput", () => {
  it("returns clean output when no warnings", () => {
    const parsed = parseEvalOutput("42");
    expect(parsed.result).toBe("42");
    expect(parsed.warnings).toEqual([]);
  });

  it("separates type-defaults warning from result", () => {
    const raw =
      "<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting the type variable 'a0' to type 'Integer'\n" +
      "    • In the expression: 1 + 2\n" +
      "\n" +
      "3";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("3");
    expect(parsed.warnings).toHaveLength(1);
    expect(parsed.warnings[0]).toContain("Defaulting the type variable");
  });

  it("handles multiple warnings before result", () => {
    const raw =
      "<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting the type variable 'a0'\n" +
      "\n" +
      "<interactive>:1:5: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting the type variable 'b0'\n" +
      "\n" +
      "5";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("5");
    expect(parsed.warnings).toHaveLength(2);
  });

  it("preserves multiline result", () => {
    const raw = "[1,2,\n 3,4,\n 5]";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("[1,2,\n 3,4,\n 5]");
    expect(parsed.warnings).toEqual([]);
  });

  it("handles empty output", () => {
    const parsed = parseEvalOutput("");
    expect(parsed.result).toBe("");
    expect(parsed.warnings).toEqual([]);
  });

  it("passes through error output as result", () => {
    const raw =
      "*** Exception: Prelude.head: empty list\n" +
      "CallStack (from HasCallStack):\n" +
      "  error, called at ...";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toContain("Exception");
    expect(parsed.warnings).toEqual([]);
  });

  it("handles warning with no result (void expression)", () => {
    const raw =
      "<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting the type variable\n";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("");
    expect(parsed.warnings).toHaveLength(1);
  });

  it("preserves raw output", () => {
    const raw = "hello\nworld";
    const parsed = parseEvalOutput(raw);
    expect(parsed.raw).toBe(raw);
  });

  it("handles Nothing result with type-defaults warning", () => {
    const raw =
      "<interactive>:1:1-11: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting the type variable 'a0' to type '()'\n" +
      "      in the following constraint\n" +
      "        Show a0 arising from a use of 'print'\n" +
      "    • In a stmt of an interactive GHCi command: print it\n" +
      "\n" +
      "Nothing";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("Nothing");
    expect(parsed.warnings).toHaveLength(1);
  });

  it("handles multiline warning continuation with source pointer", () => {
    const raw =
      "<interactive>:1:1-4: warning: [GHC-63394] [-Wx-partial]\n" +
      "    In the use of 'head'\n" +
      "    (imported from Prelude):\n" +
      ' "This is a partial function."\n' +
      "\n" +
      "*** Exception: Prelude.head: empty list";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toContain("*** Exception:");
    expect(parsed.warnings).toHaveLength(1);
    expect(parsed.warnings[0]).toContain("partial");
  });

  it("handles Just result with warning", () => {
    const raw =
      "<interactive>:1:1-13: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting the type variable 'a0' to type 'Integer'\n" +
      "\n" +
      "Just 42";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("Just 42");
    expect(parsed.warnings).toHaveLength(1);
  });

  it("handles list output with no warnings", () => {
    const parsed = parseEvalOutput("[1,2,3]");
    expect(parsed.result).toBe("[1,2,3]");
    expect(parsed.warnings).toEqual([]);
  });

  it("handles division by zero exception", () => {
    const raw =
      "<interactive>:1:1-7: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Defaulting type variable\n" +
      "\n" +
      "*** Exception: divide by zero";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("*** Exception: divide by zero");
    expect(parsed.warnings).toHaveLength(1);
  });

  it("handles show output with newlines", () => {
    const raw = '"hello\\nworld"';
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe('"hello\\nworld"');
  });

  it("handles only whitespace input", () => {
    const parsed = parseEvalOutput("   \n  ");
    expect(parsed.result).toBe("");
  });

  it("handles warning that spans many continuation lines", () => {
    const raw =
      "<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]\n" +
      "    • Line 1 of warning\n" +
      "    • Line 2 of warning\n" +
      "    • Line 3 of warning\n" +
      "    • Line 4 of warning\n" +
      "\n" +
      "42";
    const parsed = parseEvalOutput(raw);
    expect(parsed.result).toBe("42");
    expect(parsed.warnings).toHaveLength(1);
    expect(parsed.warnings[0]).toContain("Line 4");
  });
});
