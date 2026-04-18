/**
 * Unit coverage for the basic-format-rules fallback used when
 * fourmolu/ormolu are not available. The goal is exhaustive coverage of the
 * lexically-safe rules only, no false positives.
 */
import { describe, it, expect } from "vitest";
import { analyzeBasicFormatRules } from "../parsers/basic-format-rules.js";

describe("analyzeBasicFormatRules", () => {
  it("reports no issues for clean content", () => {
    const a = analyzeBasicFormatRules("module M where\nfoo = 1\n");
    expect(a.issues).toEqual([]);
    expect(a.changed).toBe(false);
    expect(a.fixed).toBe("module M where\nfoo = 1\n");
  });

  it("strips trailing whitespace and records one issue per offending line", () => {
    const src = "foo = 1   \nbar = 2\t\n";
    const a = analyzeBasicFormatRules(src);
    const tw = a.issues.filter((i) => i.kind === "trailing-whitespace");
    expect(tw.length).toBe(2);
    expect(a.fixed).toBe("foo = 1\nbar = 2\n");
    expect(a.changed).toBe(true);
  });

  it("normalizes CRLF to LF once, regardless of how many lines", () => {
    const src = "a\r\nb\r\nc\r\n";
    const a = analyzeBasicFormatRules(src);
    expect(a.issues.filter((i) => i.kind === "crlf-line-endings").length).toBe(1);
    expect(a.fixed).toBe("a\nb\nc\n");
  });

  it("flags tabs in indentation but does not rewrite (width is project-policy)", () => {
    const src = "\tfoo = 1\n";
    const a = analyzeBasicFormatRules(src);
    expect(a.issues.some((i) => i.kind === "tabs-in-indentation")).toBe(true);
    // Tab preserved — we do NOT auto-convert to spaces.
    expect(a.fixed).toBe("\tfoo = 1\n");
  });

  it("adds a final newline when missing", () => {
    const src = "x = 1";
    const a = analyzeBasicFormatRules(src);
    expect(a.issues.some((i) => i.kind === "missing-final-newline")).toBe(true);
    expect(a.fixed).toBe("x = 1\n");
    expect(a.changed).toBe(true);
  });

  it("empty input produces no issues and does not add a trailing newline", () => {
    const a = analyzeBasicFormatRules("");
    expect(a.issues).toEqual([]);
    expect(a.fixed).toBe("");
    expect(a.changed).toBe(false);
  });

  it("applies multiple fixes in one pass (idempotent repeated call)", () => {
    const src = "foo = 1  \r\nbar = 2\r\n";
    const once = analyzeBasicFormatRules(src);
    const twice = analyzeBasicFormatRules(once.fixed);
    expect(twice.issues).toEqual([]);
    expect(twice.fixed).toBe(once.fixed);
  });
});
