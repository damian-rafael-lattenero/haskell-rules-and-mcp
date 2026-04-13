import { describe, it, expect } from "vitest";
import { groupBindingsIntoLetBlocks } from "../index.js";

/**
 * Legacy auto-let transformation logic (for reference — replaced by groupBindingsIntoLetBlocks).
 */
function autoLetFix(statements: string[]): string[] {
  return statements.map((s, i) => {
    const trimmed = s.trim();
    if (i === statements.length - 1) return s;
    if (
      trimmed.startsWith("let ") ||
      trimmed.startsWith("import ") ||
      trimmed.startsWith(":") ||
      trimmed === ""
    )
      return s;
    if (/^[\w'][\w']*\s+=(?!=)/.test(trimmed)) return `let ${s}`;
    return s;
  });
}

describe("eval statements auto-let (legacy behavior)", () => {
  it("auto-prefixes bare bindings with let", () => {
    expect(autoLetFix(["x = 42", "x + 1"])).toEqual(["let x = 42", "x + 1"]);
  });

  it("leaves let-prefixed bindings unchanged", () => {
    expect(autoLetFix(["let x = 42", "x"])).toEqual(["let x = 42", "x"]);
  });

  it("never prefixes the last statement", () => {
    expect(autoLetFix(["x = 42"])).toEqual(["x = 42"]);
  });

  it("does not prefix import statements", () => {
    expect(autoLetFix(["import Data.List", "sort [3,1,2]"])).toEqual([
      "import Data.List",
      "sort [3,1,2]",
    ]);
  });

  it("does not confuse == with =", () => {
    expect(autoLetFix(["x == 42", "True"])).toEqual(["x == 42", "True"]);
  });

  it("does not confuse /= with =", () => {
    expect(autoLetFix(["x /= 42", "y"])).toEqual(["x /= 42", "y"]);
  });

  it("handles primed names like x'", () => {
    expect(autoLetFix(["x' = 42", "x'"])).toEqual(["let x' = 42", "x'"]);
  });

  it("leaves empty lines unchanged", () => {
    expect(autoLetFix(["", "x = 42", "x"])).toEqual(["", "let x = 42", "x"]);
  });

  it("leaves GHCi commands unchanged", () => {
    expect(autoLetFix([":set -XOverloadedStrings", "x"])).toEqual([
      ":set -XOverloadedStrings",
      "x",
    ]);
  });
});

describe("groupBindingsIntoLetBlocks", () => {
  it("groups single binding into let (backward compatible)", () => {
    expect(groupBindingsIntoLetBlocks(["x = 42", "x + 1"])).toEqual([
      "let x = 42",
      "x + 1",
    ]);
  });

  it("groups recursive function equations into single let", () => {
    const result = groupBindingsIntoLetBlocks([
      "f 0 = 1",
      "f n = n * f (n-1)",
      "f 5",
    ]);
    expect(result).toHaveLength(2);
    expect(result[0]).toBe("let f 0 = 1\n    f n = n * f (n-1)");
    expect(result[1]).toBe("f 5");
  });

  it("groups mutually recursive bindings", () => {
    const result = groupBindingsIntoLetBlocks([
      "even' 0 = True",
      "even' n = odd' (n-1)",
      "odd' 0 = False",
      "odd' n = even' (n-1)",
      "even' 4",
    ]);
    expect(result).toHaveLength(2);
    expect(result[0]).toContain("let even' 0 = True");
    expect(result[0]).toContain("    odd' n = even' (n-1)");
    expect(result[1]).toBe("even' 4");
  });

  it("groups multiple consecutive bindings into one let", () => {
    const result = groupBindingsIntoLetBlocks(["x = 1", "y = 2", "x + y"]);
    expect(result).toHaveLength(2);
    expect(result[0]).toBe("let x = 1\n    y = 2");
    expect(result[1]).toBe("x + y");
  });

  it("does not group across non-binding lines", () => {
    const result = groupBindingsIntoLetBlocks([
      "import Data.Char",
      "f x = x + 1",
      "f 5",
    ]);
    expect(result).toHaveLength(3);
    expect(result[0]).toBe("import Data.Char");
    expect(result[1]).toBe("let f x = x + 1");
    expect(result[2]).toBe("f 5");
  });

  it("leaves let-prefixed bindings unchanged", () => {
    const result = groupBindingsIntoLetBlocks(["let x = 42", "x"]);
    expect(result).toEqual(["let x = 42", "x"]);
  });

  it("never prefixes the last statement", () => {
    const result = groupBindingsIntoLetBlocks(["x = 42"]);
    expect(result).toEqual(["x = 42"]);
  });

  it("handles empty statements", () => {
    expect(groupBindingsIntoLetBlocks([])).toEqual([]);
  });

  it("handles GHCi commands between bindings", () => {
    const result = groupBindingsIntoLetBlocks([
      ":set -XOverloadedStrings",
      "x = 42",
      "x",
    ]);
    expect(result).toEqual([":set -XOverloadedStrings", "let x = 42", "x"]);
  });

  it("handles primed names", () => {
    const result = groupBindingsIntoLetBlocks(["x' = 42", "x'"]);
    expect(result).toEqual(["let x' = 42", "x'"]);
  });
});
