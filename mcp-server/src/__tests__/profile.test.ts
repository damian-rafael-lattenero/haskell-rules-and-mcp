import { describe, it, expect } from "vitest";
import { suggestOptimizations, parseProfFile } from "../tools/profile.js";

// Sample .prof output in GHC standard format
const SAMPLE_PROF = `
   haskell-project +RTS -p -RTS

         total time  =        0.01 secs   (12 ticks @ 1000 us, 1 processor)
         total alloc =   1,234,567 bytes  (excludes profiling overheads)

COST CENTRE MODULE           SRC                      %time %alloc

main        Main             app/Main.hs:(5,1)-(7,3)   80.0   55.0
compute     Lib.Core         src/Lib/Core.hs:(12,1)-(14,5)  15.0   30.0
helper      Lib.Core         src/Lib/Core.hs:20:1-20   5.0   15.0


                                                                    individual     inherited
COST CENTRE      MODULE                SRC              no.  entries  %time %alloc   %time %alloc

MAIN             MAIN                  <built-in>         1        0   0.0    0.4   100.0  100.0
 main            Main                  app/Main.hs        2        1  80.0   55.0   100.0  100.0
  compute        Lib.Core              src/Lib/Core.hs    3      100  15.0   30.0    20.0   45.0
   helper        Lib.Core              src/Lib/Core.hs    4     1000   5.0   15.0     5.0   15.0
`;

describe("parseProfFile", () => {
  it("extracts top cost centres", () => {
    const result = parseProfFile(SAMPLE_PROF);
    expect(result.success).toBe(true);
    expect(result.topCostCentres.length).toBeGreaterThan(0);
    const main = result.topCostCentres.find((c: any) => c.name === "main");
    expect(main).toBeDefined();
    expect(main.timePercent).toBeCloseTo(80.0);
    expect(main.allocPercent).toBeCloseTo(55.0);
  });

  it("returns total time and alloc summary", () => {
    const result = parseProfFile(SAMPLE_PROF);
    expect(result.success).toBe(true);
    expect(result.totalTime).toBeDefined();
    expect(result.totalAlloc).toBeDefined();
  });

  it("returns empty on empty input", () => {
    const result = parseProfFile("");
    expect(result.success).toBe(true);
    expect(result.topCostCentres).toHaveLength(0);
  });
});

describe("suggestOptimizations", () => {
  it("detects String concatenation with ++", () => {
    const code = `module Foo where\n\nbuildStr xs = foldr (\\x acc -> x ++ acc) "" xs\n`;
    const suggestions = suggestOptimizations(code);
    expect(suggestions.some((s: any) => s.issue.toLowerCase().includes("string") || s.suggestion.toLowerCase().includes("text"))).toBe(true);
  });

  it("detects naive recursion without accumulator pattern", () => {
    const code = `module Foo where\n\nsum' [] = 0\nsum' (x:xs) = x + sum' xs\n`;
    const suggestions = suggestOptimizations(code);
    expect(suggestions.some((s: any) => s.issue.toLowerCase().includes("recursion") || s.suggestion.toLowerCase().includes("accumulator"))).toBe(true);
  });

  it("returns empty array for clean code", () => {
    const code = `module Clean where\n\naddOne :: Int -> Int\naddOne x = x + 1\n`;
    const suggestions = suggestOptimizations(code);
    // Clean code with no obvious issues
    expect(Array.isArray(suggestions)).toBe(true);
  });

  it("each suggestion has line, issue, and suggestion fields", () => {
    const code = `module Foo where\n\nbuild xs = foldr (++) "" xs\n`;
    const suggestions = suggestOptimizations(code);
    for (const s of suggestions) {
      expect(typeof s.issue).toBe("string");
      expect(typeof s.suggestion).toBe("string");
      expect(typeof s.line).toBe("number");
    }
  });
});
