import { describe, expect, it } from "vitest";
import { parseCoverage } from "../tools/coverage.js";

describe("parseCoverage", () => {
  it("parses percentage rows from cabal/hpc output", () => {
    const rows = parseCoverage(`
    83% expressions used
    71.5% boolean coverage
    `);
    expect(rows).toHaveLength(2);
    expect(rows[0]).toEqual({ percent: 83, metric: "expressions used" });
    expect(rows[1]).toEqual({ percent: 71.5, metric: "boolean coverage" });
  });

  it("returns empty when no percentages are present", () => {
    expect(parseCoverage("no coverage rows")).toEqual([]);
  });
});
