/**
 * Unit tests for `computeWeightedOverall`.
 *
 * The weighted overall answers the question "what fraction of the
 * total decision points in the codebase are covered?" by summing
 * numerators and denominators across every metric that reports an
 * `a/b` fraction. It is NOT the mean of percentages — that would
 * over-weight tiny metrics (e.g. 6 untested `if`s dragging a 221-element
 * pool down).
 */
import { describe, it, expect } from "vitest";
import { computeWeightedOverall } from "../tools/coverage.js";

describe("computeWeightedOverall", () => {
  it("returns undefined when no metrics have parseable fractions", () => {
    expect(
      computeWeightedOverall([
        { metric: "expressions", percent: 100 },
        { metric: "guards", percent: 87 },
      ])
    ).toBeUndefined();
  });

  it("returns undefined when metrics array is empty", () => {
    expect(computeWeightedOverall([])).toBeUndefined();
  });

  it("computes plain weighted average across a handful of metrics", () => {
    // 1/1 + 7/7 + 34/34 + 14/20 + 20/25 + 214/221 + 4/4 + 16/16
    //  =  (1+7+34+14+20+214+4+16) / (1+7+34+20+25+221+4+16)
    //  =  310 / 328
    //  ≈  94.51
    const result = computeWeightedOverall([
      { metric: "expressions", percent: 100, fraction: "1/1" },
      { metric: "boolean", percent: 100, fraction: "7/7" },
      { metric: "guards", percent: 100, fraction: "34/34" },
      { metric: "if conditions", percent: 70, fraction: "14/20" },
      { metric: "qualifiers", percent: 80, fraction: "20/25" },
      { metric: "alternatives", percent: 96, fraction: "214/221" },
      { metric: "local declarations", percent: 100, fraction: "4/4" },
      { metric: "top-level declarations", percent: 100, fraction: "16/16" },
    ]);
    expect(result).toBeCloseTo(94.51, 1);
  });

  it("ignores metrics without a fraction (contributes nothing to weighting)", () => {
    // 100% + metric-without-fraction should equal 100% — the unmarked one
    // neither adds nor subtracts.
    const result = computeWeightedOverall([
      { metric: "a", percent: 100, fraction: "10/10" },
      { metric: "b", percent: 70 }, // no fraction
    ]);
    expect(result).toBe(100);
  });

  it("tolerates whitespace inside fractions (`14 / 20`)", () => {
    expect(
      computeWeightedOverall([{ metric: "x", percent: 70, fraction: "14 / 20" }])
    ).toBe(70);
  });

  it("skips malformed fractions instead of crashing", () => {
    const result = computeWeightedOverall([
      { metric: "good", percent: 100, fraction: "10/10" },
      { metric: "bad", percent: 50, fraction: "not-a-fraction" },
      { metric: "zerodenom", percent: 0, fraction: "0/0" },
    ]);
    expect(result).toBe(100);
  });

  it("reproduces the 70%-vs-94% misleading-lowest example from backlog", () => {
    // This is EXACTLY the production case surfaced during the dogfood
    // session: a small `if-conditions` pool dragged `overallPercent` to
    // 70 even though most of the code was deeply covered.
    const metrics = [
      { metric: "expressions", percent: 100, fraction: "1/1" },
      { metric: "boolean coverage", percent: 100, fraction: "7/7" },
      { metric: "guards", percent: 100, fraction: "34/34" },
      { metric: "'if' conditions", percent: 70, fraction: "14/20" },
      { metric: "qualifiers", percent: 80, fraction: "20/25" },
      { metric: "alternatives", percent: 96, fraction: "214/221" },
      { metric: "local declarations", percent: 100, fraction: "4/4" },
      { metric: "top-level declarations", percent: 100, fraction: "16/16" },
    ];
    const lowest = Math.min(...metrics.map((m) => m.percent)); // 70
    const weighted = computeWeightedOverall(metrics)!; // ~94.5

    expect(lowest).toBe(70);
    expect(weighted).toBeGreaterThan(94);
    expect(weighted).toBeLessThan(95);
    // Confirm the weighted number is what an agent should surface to
    // a human looking at "how much of my code is tested?".
    expect(weighted - lowest).toBeGreaterThan(20);
  });

  it("returns exactly 100 when every fraction is saturated", () => {
    expect(
      computeWeightedOverall([
        { metric: "a", percent: 100, fraction: "1/1" },
        { metric: "b", percent: 100, fraction: "100/100" },
      ])
    ).toBe(100);
  });

  it("returns exactly 0 when every fraction is empty-hit", () => {
    expect(
      computeWeightedOverall([
        { metric: "a", percent: 0, fraction: "0/10" },
        { metric: "b", percent: 0, fraction: "0/5" },
      ])
    ).toBe(0);
  });

  it("rounds to 2 decimal places, matching the other coverage fields", () => {
    const result = computeWeightedOverall([
      { metric: "a", percent: 33, fraction: "1/3" },
    ])!;
    expect(result).toBeCloseTo(33.33, 2);
    // Exactly 2 decimals — no trailing 3s from floating-point drift.
    const asString = result.toString();
    const decimals = asString.split(".")[1] ?? "";
    expect(decimals.length).toBeLessThanOrEqual(2);
  });
});
