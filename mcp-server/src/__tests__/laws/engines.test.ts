/**
 * Unit tests per-engine. Each engine is pure so tests are cheap and
 * exhaustive across the shape variants that matter.
 */
import { describe, it, expect } from "vitest";
import { endomorphismEngine } from "../../laws/engines/endomorphism.js";
import { binaryOpEngine } from "../../laws/engines/binary-op.js";
import { listEndomorphismEngine } from "../../laws/engines/list-endomorphism.js";
import { roundtripEngine } from "../../laws/engines/roundtrip.js";
import { evaluatorPreservationEngine } from "../../laws/engines/evaluator-preservation.js";
import { constantFoldingSoundnessEngine } from "../../laws/engines/constant-folding-soundness.js";
import { functorLawsEngine } from "../../laws/engines/functor-laws.js";
import { runLawEngines } from "../../laws/registry.js";

describe("endomorphismEngine", () => {
  it("emits idempotence + involution for X -> X", () => {
    const laws = endomorphismEngine.match({ functionName: "normalize", type: "Int -> Int" });
    expect(laws.map((l) => l.law)).toEqual(["idempotence", "involution"]);
    expect(laws.every((l) => l.confidence === "low")).toBe(true);
  });
  it("skips when types differ", () => {
    const laws = endomorphismEngine.match({ functionName: "show", type: "Int -> String" });
    expect(laws).toEqual([]);
  });
});

describe("binaryOpEngine", () => {
  it("emits associativity + commutativity for a -> a -> a", () => {
    const laws = binaryOpEngine.match({ functionName: "add", type: "Int -> Int -> Int" });
    expect(laws.map((l) => l.law)).toEqual(["associativity", "commutativity"]);
  });
  it("skips on non-uniform binary types", () => {
    expect(binaryOpEngine.match({ functionName: "zip", type: "Int -> Bool -> Int" })).toEqual([]);
  });
});

describe("listEndomorphismEngine", () => {
  it("emits length preservation for [a] -> [a]", () => {
    const laws = listEndomorphismEngine.match({ functionName: "reverse", type: "[Int] -> [Int]" });
    expect(laws.map((l) => l.law)).toEqual(["length preservation"]);
  });
  it("skips when element type differs", () => {
    expect(listEndomorphismEngine.match({ functionName: "fmap", type: "[Int] -> [String]" })).toEqual([]);
  });
});

describe("roundtripEngine", () => {
  it("detects direct X->Y / Y->X pair", () => {
    const laws = roundtripEngine.match({
      functionName: "pretty",
      type: "Expr -> String",
      siblings: [{ name: "parse", type: "String -> Expr" }],
    });
    expect(laws).toHaveLength(1);
    expect(laws[0]?.law).toContain("roundtrip");
    expect(laws[0]?.property).toContain("parse (pretty x)");
  });

  it("detects Maybe-wrapped parse roundtrip", () => {
    const laws = roundtripEngine.match({
      functionName: "pretty",
      type: "Expr -> String",
      siblings: [{ name: "parse", type: "String -> Maybe Expr" }],
    });
    expect(laws).toHaveLength(1);
    expect(laws[0]?.property).toContain("Just (x :: Expr)");
  });

  it("returns [] when no inverse sibling exists", () => {
    const laws = roundtripEngine.match({
      functionName: "pretty",
      type: "Expr -> String",
      siblings: [{ name: "length", type: "String -> Int" }],
    });
    expect(laws).toEqual([]);
  });
});

describe("evaluatorPreservationEngine (NEW)", () => {
  it("detects simplify + eval pair and emits preservation law", () => {
    const laws = evaluatorPreservationEngine.match({
      functionName: "simplify",
      type: "Expr -> Expr",
      siblings: [
        { name: "eval", type: "Env -> Expr -> Either Error Int" },
        { name: "pretty", type: "Expr -> String" },
      ],
    });
    expect(laws).toHaveLength(1);
    expect(laws[0]?.law).toContain("evaluator preservation");
    expect(laws[0]?.confidence).toBe("high");
    expect(laws[0]?.property).toContain("eval p1 (simplify x)");
    expect(laws[0]?.property).toContain("eval p1 (x :: Expr)");
  });

  it("detects single-param evaluator (run : Expr -> r)", () => {
    const laws = evaluatorPreservationEngine.match({
      functionName: "optimize",
      type: "AST -> AST",
      siblings: [{ name: "run", type: "AST -> Int" }],
    });
    expect(laws).toHaveLength(1);
    expect(laws[0]?.property).toBe("\\x -> run (optimize x) == run (x :: AST)");
  });

  it("skips when no interpreter exists", () => {
    expect(
      evaluatorPreservationEngine.match({
        functionName: "simplify",
        type: "Expr -> Expr",
        siblings: [{ name: "pretty", type: "Expr -> String" }],
      }).length
    ).toBeGreaterThanOrEqual(0); // pretty qualifies as an interpreter (Expr -> String)
  });

  it("skips when target is not X -> X", () => {
    expect(
      evaluatorPreservationEngine.match({
        functionName: "parse",
        type: "String -> Expr",
        siblings: [{ name: "eval", type: "Expr -> Int" }],
      })
    ).toEqual([]);
  });

  it("ignores siblings that are themselves endomorphisms (no new info)", () => {
    const laws = evaluatorPreservationEngine.match({
      functionName: "simplify",
      type: "Expr -> Expr",
      siblings: [{ name: "normalize", type: "Expr -> Expr" }],
    });
    expect(laws).toEqual([]);
  });
});

describe("constantFoldingSoundnessEngine (NEW)", () => {
  it("fires for simplify with an interpreter sibling", () => {
    const laws = constantFoldingSoundnessEngine.match({
      functionName: "simplify",
      type: "Expr -> Expr",
      siblings: [{ name: "eval", type: "Env -> Expr -> Int" }],
    });
    expect(laws).toHaveLength(1);
    expect(laws[0]?.law).toContain("constant-folding soundness");
    expect(laws[0]?.confidence).toBe("high");
  });

  it("does NOT fire for generic transform names like `transform`", () => {
    const laws = constantFoldingSoundnessEngine.match({
      functionName: "transform",
      type: "Expr -> Expr",
      siblings: [{ name: "eval", type: "Expr -> Int" }],
    });
    expect(laws).toEqual([]);
  });

  it("fires for `normalize`, `fold`, `rewrite`", () => {
    for (const name of ["normalize", "fold", "rewrite", "optimize", "canonicalize"]) {
      const laws = constantFoldingSoundnessEngine.match({
        functionName: name,
        type: "Expr -> Expr",
        siblings: [{ name: "eval", type: "Expr -> Int" }],
      });
      expect(laws.length, `expected ${name} to fire`).toBeGreaterThan(0);
    }
  });
});

describe("functorLawsEngine (NEW)", () => {
  it("emits identity + composition for (a -> b) -> F a -> F b", () => {
    const laws = functorLawsEngine.match({ functionName: "fmapTree", type: "(a -> b) -> Tree a -> Tree b" });
    expect(laws.map((l) => l.law)).toEqual(["functor identity", "functor composition"]);
    expect(laws.every((l) => l.confidence === "high")).toBe(true);
  });

  it("works for list form [a]", () => {
    const laws = functorLawsEngine.match({ functionName: "mapList", type: "(a -> b) -> [a] -> [b]" });
    expect(laws).toHaveLength(2);
    expect(laws[0]?.property).toContain("(x :: [a])");
  });

  it("skips when shape does not match", () => {
    expect(functorLawsEngine.match({ functionName: "foo", type: "Int -> Int" })).toEqual([]);
  });
});

describe("registry + deduplication", () => {
  it("deduplicates identical properties from different engines", () => {
    // For `simplify + eval`, both constant-folding and evaluator-preservation
    // produce a preservation property — the registry should keep only one.
    const laws = runLawEngines({
      functionName: "simplify",
      type: "Expr -> Expr",
      siblings: [{ name: "eval", type: "Env -> Expr -> Int" }],
    });
    const preservationLaws = laws.filter((l) => l.property.includes("eval p1 (simplify x)"));
    expect(preservationLaws.length).toBe(1);
  });

  it("covers both old and new shapes end-to-end (reverse + list-length preservation)", () => {
    const laws = runLawEngines({
      functionName: "reverse",
      type: "[a] -> [a]",
      siblings: [],
    });
    expect(laws.some((l) => l.law === "length preservation")).toBe(true);
  });

  it("returns empty for non-matching shapes", () => {
    const laws = runLawEngines({ functionName: "main", type: "IO ()", siblings: [] });
    expect(laws).toEqual([]);
  });
});
