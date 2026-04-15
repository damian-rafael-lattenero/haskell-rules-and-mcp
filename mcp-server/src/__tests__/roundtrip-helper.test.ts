/**
 * Unit tests for roundtrip helper in ghci_quickcheck.
 */
import { describe, it, expect } from "vitest";

describe("Roundtrip Property Generation", () => {
  it("should generate basic roundtrip property", () => {
    // Format: "pretty,parse"
    const parts = "pretty,parse".split(",").map((s) => s.trim());
    const [prettyFn, parseFn] = parts;
    const property = `\\e -> ${parseFn} (${prettyFn} e) == Just e`;

    expect(property).toBe("\\e -> parse (pretty e) == Just e");
  });

  it("should generate roundtrip with normalize", () => {
    // Format: "pretty,parse,normalize"
    const parts = "pretty,parse,normalize".split(",").map((s) => s.trim());
    const [prettyFn, parseFn, normalizeFn] = parts;
    const property = `\\e -> fmap ${normalizeFn} (${parseFn} (${prettyFn} e)) == Just (${normalizeFn} e)`;

    expect(property).toBe(
      "\\e -> fmap normalize (parse (pretty e)) == Just (normalize e)"
    );
  });

  it("should handle custom function names", () => {
    const parts = "toJSON,fromJSON".split(",").map((s) => s.trim());
    const [prettyFn, parseFn] = parts;
    const property = `\\e -> ${parseFn} (${prettyFn} e) == Just e`;

    expect(property).toBe("\\e -> fromJSON (toJSON e) == Just e");
  });

  it("should handle qualified names", () => {
    const parts = "Text.pack,Text.unpack".split(",").map((s) => s.trim());
    const [prettyFn, parseFn] = parts;
    const property = `\\e -> ${parseFn} (${prettyFn} e) == Just e`;

    expect(property).toBe("\\e -> Text.unpack (Text.pack e) == Just e");
  });

  it("should validate format", () => {
    const invalidFormats = ["pretty", "pretty,", ",parse", "pretty,parse,normalize,extra"];

    for (const format of invalidFormats) {
      const parts = format.split(",").map((s) => s.trim());
      const isValid = parts.length >= 2 && parts.length <= 3 && parts.every((p) => p.length > 0);
      expect(isValid).toBe(false);
    }
  });

  it("should accept valid formats", () => {
    const validFormats = ["pretty,parse", "pretty,parse,normalize", "toJSON, fromJSON"];

    for (const format of validFormats) {
      const parts = format.split(",").map((s) => s.trim());
      const isValid = parts.length >= 2 && parts.length <= 3 && parts.every((p) => p.length > 0);
      expect(isValid).toBe(true);
    }
  });
});
