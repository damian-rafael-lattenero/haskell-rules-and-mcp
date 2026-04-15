import { describe, expect, it } from "vitest";
import { validatePropertyText } from "../parsers/property-validator.js";

describe("validatePropertyText", () => {
  it("accepts a normal property", () => {
    const result = validatePropertyText("\\x -> x == x");
    expect(result.ok).toBe(true);
  });

  it("rejects GHCi command-like input", () => {
    const result = validatePropertyText(":t map");
    expect(result.ok).toBe(false);
    expect(result.issues.some((i) => i.code === "ghci-command")).toBe(true);
  });

  it("rejects unused lambda binder", () => {
    const result = validatePropertyText("\\e -> True");
    expect(result.ok).toBe(false);
    expect(result.issues.some((i) => i.code === "unused-binder")).toBe(true);
  });

  it("allows intentional underscore binders", () => {
    const result = validatePropertyText("\\_ -> True");
    expect(result.ok).toBe(true);
  });
});
