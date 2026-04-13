import { describe, it, expect } from "vitest";
import { suggestFunctionProperties, type Sibling } from "../laws/function-laws.js";

describe("suggestFunctionProperties", () => {
  describe("endomorphism (a -> a)", () => {
    it("suggests idempotence and involution for Int -> Int", () => {
      const suggestions = suggestFunctionProperties("normalize", "normalize :: Int -> Int");
      const laws = suggestions.map((s) => s.law);
      expect(laws).toContain("idempotence");
      expect(laws).toContain("involution");
    });

    it("idempotence property references the function correctly", () => {
      const suggestions = suggestFunctionProperties("normalize", "normalize :: Int -> Int");
      const idem = suggestions.find((s) => s.law === "idempotence")!;
      expect(idem.property).toContain("normalize (normalize x)");
      expect(idem.property).toContain("Int");
      expect(idem.confidence).toBe("low");
    });
  });

  describe("binary operator (a -> a -> a)", () => {
    it("suggests associativity and commutativity", () => {
      const suggestions = suggestFunctionProperties("merge", "merge :: Int -> Int -> Int");
      const laws = suggestions.map((s) => s.law);
      expect(laws).toContain("associativity");
      expect(laws).toContain("commutativity");
    });

    it("associativity property is well-formed", () => {
      const suggestions = suggestFunctionProperties("add", "add :: Int -> Int -> Int");
      const assoc = suggestions.find((s) => s.law === "associativity")!;
      expect(assoc.property).toContain("add (add x y) z");
      expect(assoc.property).toContain("add x (add y z");
      expect(assoc.confidence).toBe("medium");
    });

    it("does not suggest binary op laws for a -> b -> a", () => {
      const suggestions = suggestFunctionProperties("foo", "foo :: Int -> String -> Int");
      const laws = suggestions.map((s) => s.law);
      expect(laws).not.toContain("associativity");
      expect(laws).not.toContain("commutativity");
    });
  });

  describe("roundtrip pairs (siblings)", () => {
    it("detects encode/decode roundtrip", () => {
      const siblings: Sibling[] = [
        { name: "encode", type: "encode :: String -> Bytes" },
        { name: "decode", type: "decode :: Bytes -> String" },
      ];
      const suggestions = suggestFunctionProperties("encode", "encode :: String -> Bytes", siblings);
      const roundtrip = suggestions.find((s) => s.law.includes("roundtrip"));
      expect(roundtrip).toBeDefined();
      expect(roundtrip!.property).toContain("decode (encode x)");
      expect(roundtrip!.confidence).toBe("high");
    });

    it("does not suggest roundtrip without matching inverse", () => {
      const siblings: Sibling[] = [
        { name: "encode", type: "encode :: String -> Bytes" },
        { name: "hash", type: "hash :: Bytes -> Int" },
      ];
      const suggestions = suggestFunctionProperties("encode", "encode :: String -> Bytes", siblings);
      const roundtrip = suggestions.find((s) => s.law.includes("roundtrip"));
      expect(roundtrip).toBeUndefined();
    });

    it("does not pair a function with itself", () => {
      const siblings: Sibling[] = [
        { name: "id", type: "id :: Int -> Int" },
      ];
      const suggestions = suggestFunctionProperties("id", "id :: Int -> Int", siblings);
      const roundtrip = suggestions.find((s) => s.law.includes("roundtrip"));
      expect(roundtrip).toBeUndefined();
    });
  });

  describe("list endomorphism ([a] -> [a])", () => {
    it("suggests length preservation", () => {
      const suggestions = suggestFunctionProperties("sort", "sort :: [Int] -> [Int]");
      const lenPres = suggestions.find((s) => s.law === "length preservation");
      expect(lenPres).toBeDefined();
      expect(lenPres!.property).toContain("length (sort xs) == length");
      expect(lenPres!.confidence).toBe("low");
    });
  });

  describe("no suggestions for complex types", () => {
    it("returns empty for IO actions", () => {
      const suggestions = suggestFunctionProperties("main", "main :: IO ()");
      expect(suggestions).toEqual([]);
    });

    it("returns empty for multi-arg functions with different types", () => {
      const suggestions = suggestFunctionProperties("foo", "foo :: Int -> String -> Bool -> IO ()");
      expect(suggestions).toEqual([]);
    });
  });
});
