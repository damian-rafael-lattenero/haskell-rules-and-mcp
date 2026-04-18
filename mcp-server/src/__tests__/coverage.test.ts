import { describe, expect, it } from "vitest";
import { parseCoverage, parseHpcIndexHtml } from "../tools/coverage.js";

describe("parseCoverage", () => {
  it("parses percentage rows from cabal/hpc output", () => {
    const rows = parseCoverage(`
    83% expressions used
    71.5% boolean coverage
    `);
    expect(rows).toHaveLength(2);
    expect(rows[0]).toMatchObject({ percent: 83, metric: "expressions used" });
    expect(rows[1]).toMatchObject({ percent: 71.5, metric: "boolean coverage" });
  });

  it("returns empty when no percentages are present", () => {
    expect(parseCoverage("no coverage rows")).toEqual([]);
  });

  it("parses a realistic `hpc report` summary and extracts fractions", () => {
    // This is the actual output shape of `hpc report <tix>`.
    const output = `
 99% expressions used (123/124)
 100% boolean coverage (2/2)
      100% guards (1/1), 0 always True, 0 always False, 0 unevaluated
 75% alternatives used (3/4)
 100% local declarations used (2/2)
 100% top-level declarations used (3/3)
`;
    const rows = parseCoverage(output);
    const metrics = rows.map((r) => r.metric);
    expect(metrics).toContain("expressions used");
    expect(metrics).toContain("alternatives used");
    const expressions = rows.find((r) => r.metric === "expressions used");
    expect(expressions?.percent).toBe(99);
    expect(expressions?.fraction).toBe("123/124");
  });

  it("accepts `NN % metric` with space before %", () => {
    const rows = parseCoverage("  83 % expressions used (1/1)\n");
    expect(rows).toHaveLength(1);
    expect(rows[0]!.percent).toBe(83);
    expect(rows[0]!.metric).toBe("expressions used");
  });

  it("ignores lines that look percentage-like but are not (e.g. '100' alone)", () => {
    expect(parseCoverage("100 is not a percent\nexpressions used\n")).toEqual([]);
  });
});

describe("parseHpcIndexHtml", () => {
  it("extracts percents + fractions from a typical hpc index", () => {
    const html = `
      <html><body>
      <table>
        <tr><th>Program Coverage Total</th></tr>
        <tr><td>99%</td><td>123/124</td></tr>
        <tr><td>100%</td><td>2/2</td></tr>
        <tr><td>100%</td><td>1/1</td></tr>
        <tr><td>0%</td><td>0/0</td></tr>
        <tr><td>75%</td><td>3/4</td></tr>
      </table>
      </body></html>
    `;
    const rows = parseHpcIndexHtml(html);
    expect(rows.length).toBeGreaterThanOrEqual(3);
    // First row maps to "expressions" by convention
    expect(rows[0]!.metric).toBe("expressions");
    expect(rows[0]!.percent).toBe(99);
    expect(rows[0]!.fraction).toBe("123/124");
  });

  it("returns [] for an empty or non-HTML input", () => {
    expect(parseHpcIndexHtml("")).toEqual([]);
    expect(parseHpcIndexHtml("<html></html>")).toEqual([]);
  });

  it("strips tags and is tolerant to &nbsp;", () => {
    const html = "<td>&nbsp;42%&nbsp;</td><td>&nbsp;1/2&nbsp;</td>";
    const rows = parseHpcIndexHtml(html);
    expect(rows[0]?.percent).toBe(42);
    expect(rows[0]?.fraction).toBe("1/2");
  });
});
