import { describe, it, expect } from "vitest";
import { parseCompletionOutput } from "../parsers/completion-parser.js";

describe("parseCompletionOutput", () => {
  it("parses standard completion output", () => {
    const output = `6 6 "ma"\n"map"\n"mapM"\n"mapM_"\n"max"\n"maxBound"\n"maybe"`;
    const result = parseCompletionOutput(output);
    expect(result.total).toBe(6);
    expect(result.prefix).toBe("ma");
    expect(result.completions).toHaveLength(6);
    expect(result.completions).toContain("map");
    expect(result.completions).toContain("maybe");
  });

  it("parses single completion", () => {
    const output = `1 1 "mapM"\n"mapM"`;
    const result = parseCompletionOutput(output);
    expect(result.total).toBe(1);
    expect(result.completions).toEqual(["mapM"]);
  });

  it("handles empty output", () => {
    const result = parseCompletionOutput("");
    expect(result.completions).toEqual([]);
    expect(result.total).toBe(0);
    expect(result.prefix).toBe("");
  });

  it("handles zero completions", () => {
    const output = `0 0 "zzzzz"`;
    const result = parseCompletionOutput(output);
    expect(result.total).toBe(0);
    expect(result.completions).toEqual([]);
    expect(result.prefix).toBe("zzzzz");
  });

  it("strips quotes from completions", () => {
    const output = `2 2 "Da"\n"Data.List"\n"Data.Map"`;
    const result = parseCompletionOutput(output);
    expect(result.completions).toEqual(["Data.List", "Data.Map"]);
  });
});
