import { describe, it, expect } from "vitest";
import { suggestConstructorProperties } from "../laws/constructor-laws.js";
import type { Constructor } from "../parsers/constructor-parser.js";

describe("suggestConstructorProperties", () => {
  const exprConstructors: Constructor[] = [
    { name: "Lit", fields: ["Int"] },
    { name: "Add", fields: ["Expr", "Expr"] },
    { name: "Neg", fields: ["Expr"] },
    { name: "Var", fields: ["String"] },
  ];

  describe("totality properties", () => {
    it("generates totality check for every constructor", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const totalityLaws = laws.filter(l => l.law.startsWith("totality:"));
      expect(totalityLaws).toHaveLength(4);
      expect(totalityLaws.map(l => l.law)).toContain("totality: Lit");
      expect(totalityLaws.map(l => l.law)).toContain("totality: Add");
      expect(totalityLaws.map(l => l.law)).toContain("totality: Neg");
      expect(totalityLaws.map(l => l.law)).toContain("totality: Var");
    });

    it("totality uses seq for crash detection", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const litTotality = laws.find(l => l.law === "totality: Lit")!;
      expect(litTotality.property).toContain("seq");
      expect(litTotality.property).toContain("True");
      expect(litTotality.confidence).toBe("high");
    });
  });

  describe("identity properties", () => {
    it("suggests identity for Lit (Int field matches Either _ Int return)", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const identity = laws.find(l => l.law.includes("identity") && l.law.includes("Lit"));
      expect(identity).toBeDefined();
      expect(identity!.property).toContain("Right a0");
      expect(identity!.confidence).toBe("medium");
    });

    it("does NOT suggest identity for Var (String != Int)", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const varIdentity = laws.find(l => l.law.includes("identity") && l.law.includes("Var"));
      expect(varIdentity).toBeUndefined();
    });

    it("does NOT suggest identity for recursive constructors", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const addIdentity = laws.find(l => l.law.includes("identity") && l.law.includes("Add"));
      expect(addIdentity).toBeUndefined();
    });
  });

  describe("homomorphism properties", () => {
    it("suggests homomorphism for Add (binary recursive) with + and *", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const addHomo = laws.filter(l => l.law.includes("homomorphism") && l.law.includes("Add"));
      // Suggests both + and * (type system filters wrong ones via :t)
      expect(addHomo.length).toBe(2);
      expect(addHomo.some(l => l.property.includes("liftA2 (+)"))).toBe(true);
      expect(addHomo.some(l => l.property.includes("liftA2 (*)"))).toBe(true);
      // All low confidence — no domain-specific guessing
      expect(addHomo.every(l => l.confidence === "low")).toBe(true);
    });

    it("suggests negate homomorphism for Neg (unary recursive)", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const negHomo = laws.find(l => l.law.includes("homomorphism") && l.law.includes("Neg"));
      expect(negHomo).toBeDefined();
      expect(negHomo!.property).toContain("fmap negate");
    });

    it("does NOT suggest homomorphism for non-recursive constructors", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], exprConstructors, "Expr", 1
      );
      const litHomo = laws.find(l => l.law.includes("homomorphism") && l.law.includes("Lit"));
      expect(litHomo).toBeUndefined();
    });
  });

  describe("argument positioning", () => {
    it("places ADT arg in correct position (first arg)", () => {
      const laws = suggestConstructorProperties(
        "simplify", "Expr", [], [{ name: "Lit", fields: ["Int"] }], "Expr", 0
      );
      const totality = laws.find(l => l.law === "totality: Lit")!;
      expect(totality.property).toContain("simplify (Lit a0)");
    });

    it("places ADT arg in correct position (second arg)", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"], [{ name: "Lit", fields: ["Int"] }], "Expr", 1
      );
      const totality = laws.find(l => l.law === "totality: Lit")!;
      expect(totality.property).toContain("eval e0 (Lit a0)");
    });
  });

  describe("Maybe return type", () => {
    it("suggests Just identity for matching fields", () => {
      const laws = suggestConstructorProperties(
        "safeLookup", "Maybe Int", ["String"],
        [{ name: "Known", fields: ["Int"] }], "Key", 1
      );
      const identity = laws.find(l => l.law.includes("identity"));
      expect(identity).toBeDefined();
      expect(identity!.property).toContain("Just a0");
    });
  });

  describe("direct return type (no wrapper)", () => {
    it("suggests direct unwrap identity", () => {
      const laws = suggestConstructorProperties(
        "getValue", "Int", [],
        [{ name: "MkVal", fields: ["Int"] }], "Val", 0
      );
      const identity = laws.find(l => l.law.includes("identity"));
      expect(identity).toBeDefined();
      expect(identity!.property).toContain("== a0");
    });
  });

  describe("nullary constructors", () => {
    it("generates totality for nullary constructors", () => {
      const laws = suggestConstructorProperties(
        "eval", "Either Error Int", ["Env"],
        [{ name: "Unit", fields: [] }], "Expr", 1
      );
      const totality = laws.find(l => l.law === "totality: Unit")!;
      expect(totality).toBeDefined();
      expect(totality.property).toContain("eval e0 Unit");
    });
  });
});
