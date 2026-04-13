import { describe, it, expect } from "vitest";
import {
  TYPECLASS_LAWS,
  extractTypeclasses,
  findApplicableLaws,
} from "../laws/typeclass-laws.js";

describe("TYPECLASS_LAWS", () => {
  it("has laws for common typeclasses", () => {
    const typeclasses = new Set(TYPECLASS_LAWS.map((l) => l.typeclass));
    expect(typeclasses.has("Eq")).toBe(true);
    expect(typeclasses.has("Ord")).toBe(true);
    expect(typeclasses.has("Semigroup")).toBe(true);
    expect(typeclasses.has("Monoid")).toBe(true);
    expect(typeclasses.has("Functor")).toBe(true);
    expect(typeclasses.has("Monad")).toBe(true);
  });

  it("every law has a property template with {T} placeholder", () => {
    for (const law of TYPECLASS_LAWS) {
      expect(law.propertyTemplate).toContain("{T}");
    }
  });

  it("every law has required instances", () => {
    for (const law of TYPECLASS_LAWS) {
      expect(law.requiredInstances.length).toBeGreaterThan(0);
    }
  });
});

describe("extractTypeclasses", () => {
  it("extracts typeclass from simple instance", () => {
    const result = extractTypeclasses(["instance Eq MyType"]);
    expect(result).toEqual(["Eq"]);
  });

  it("extracts typeclass from constrained instance", () => {
    const result = extractTypeclasses(["instance Eq a => Eq (Maybe a)"]);
    expect(result).toEqual(["Eq"]);
  });

  it("handles multiple instances", () => {
    const result = extractTypeclasses([
      "instance Eq MyType",
      "instance Ord MyType",
      "instance Show MyType",
    ]);
    expect(result).toEqual(["Eq", "Ord", "Show"]);
  });

  it("handles [safe] qualifier", () => {
    const result = extractTypeclasses(["instance [safe] Eq MyType"]);
    // The [safe] is before the typeclass — our regex should still work
    expect(result.length).toBeGreaterThan(0);
  });

  it("filters empty strings", () => {
    const result = extractTypeclasses(["not an instance line"]);
    expect(result).toHaveLength(0);
  });
});

describe("findApplicableLaws", () => {
  it("finds Eq laws when type has Eq and Arbitrary", () => {
    const laws = findApplicableLaws("MyType", ["Eq", "Arbitrary"]);
    expect(laws.length).toBeGreaterThan(0);
    expect(laws.every((l) => l.typeclass === "Eq")).toBe(true);
    expect(laws.some((l) => l.lawName === "reflexivity")).toBe(true);
    expect(laws.some((l) => l.lawName === "symmetry")).toBe(true);
  });

  it("replaces {T} with concrete type name", () => {
    const laws = findApplicableLaws("MyType", ["Eq", "Arbitrary"]);
    for (const law of laws) {
      expect(law.property).toContain("MyType");
      expect(law.property).not.toContain("{T}");
    }
  });

  it("finds Monoid laws when type has Monoid + Eq + Arbitrary", () => {
    const laws = findApplicableLaws("MyType", ["Monoid", "Semigroup", "Eq", "Arbitrary"]);
    const monoidLaws = laws.filter((l) => l.typeclass === "Monoid");
    expect(monoidLaws.length).toBeGreaterThanOrEqual(2);
    expect(monoidLaws.some((l) => l.lawName === "left-identity")).toBe(true);
    expect(monoidLaws.some((l) => l.lawName === "right-identity")).toBe(true);
  });

  it("requires Eq for most laws", () => {
    const laws = findApplicableLaws("MyType", ["Ord", "Arbitrary"]);
    // Ord without Eq: only totality law doesn't require Eq
    // antisymmetry requires Eq, transitivity doesn't require Eq
    expect(laws.some((l) => l.lawName === "totality")).toBe(true);
    expect(laws.some((l) => l.lawName === "transitivity")).toBe(true);
  });

  it("returns empty for type with no matching instances", () => {
    const laws = findApplicableLaws("MyType", ["Show"]);
    expect(laws).toHaveLength(0);
  });

  it("treats Arbitrary as always available", () => {
    // findApplicableLaws skips the Arbitrary check — it assumes the caller verified
    const laws = findApplicableLaws("MyType", ["Eq"]);
    expect(laws.length).toBeGreaterThan(0);
  });
});
