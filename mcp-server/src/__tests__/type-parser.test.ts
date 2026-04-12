import { describe, it, expect } from "vitest";
import { parseTypeOutput, parseInfoOutput } from "../parsers/type-parser.js";

describe("parseTypeOutput", () => {
  it("parses simple type", () => {
    const result = parseTypeOutput("map :: (a -> b) -> [a] -> [b]");
    expect(result).not.toBeNull();
    expect(result!.expression).toBe("map");
    expect(result!.type).toBe("(a -> b) -> [a] -> [b]");
  });

  it("parses operator type with parens", () => {
    const result = parseTypeOutput("(++) :: forall a. [a] -> [a] -> [a]");
    expect(result).not.toBeNull();
    expect(result!.expression).toBe("(++)");
    expect(result!.type).toBe("forall a. [a] -> [a] -> [a]");
  });

  it("collapses multiline type", () => {
    const result = parseTypeOutput(
      "foldr\n  :: forall (t :: * -> *) a b.\n     Foldable t =>\n     (a -> b -> b) -> b -> t a -> b"
    );
    expect(result).not.toBeNull();
    expect(result!.expression).toBe("foldr");
    expect(result!.type).toContain("Foldable t =>");
    expect(result!.type).toContain("(a -> b -> b) -> b -> t a -> b");
    // No newlines in result
    expect(result!.type).not.toContain("\n");
  });

  it("returns null for empty input", () => {
    expect(parseTypeOutput("")).toBeNull();
  });

  it("returns null for garbage input", () => {
    expect(parseTypeOutput("no type here")).toBeNull();
  });

  it("handles expression with spaces", () => {
    const result = parseTypeOutput("map (+1) :: [Int] -> [Int]");
    expect(result).not.toBeNull();
    expect(result!.expression).toBe("map (+1)");
    expect(result!.type).toBe("[Int] -> [Int]");
  });

  it("handles constrained type with forall", () => {
    const result = parseTypeOutput("show :: forall a. Show a => a -> String");
    expect(result).not.toBeNull();
    expect(result!.type).toBe("forall a. Show a => a -> String");
  });

  it("handles tuple result type", () => {
    const result = parseTypeOutput("swap :: (a, b) -> (b, a)");
    expect(result).not.toBeNull();
    expect(result!.type).toBe("(a, b) -> (b, a)");
  });

  it("handles type with line number prefix from GHCi", () => {
    const result = parseTypeOutput("  id :: a -> a");
    expect(result).not.toBeNull();
    expect(result!.expression).toBe("id");
    expect(result!.type).toBe("a -> a");
  });
});

describe("parseInfoOutput", () => {
  it("parses a data type", () => {
    const output = `data Maybe a = Nothing | Just a
  \t-- Defined in 'GHC.Maybe'
instance Eq a => Eq (Maybe a)
instance Ord a => Ord (Maybe a)`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("data");
    expect(result.name).toBe("Maybe");
    expect(result.instances).toHaveLength(2);
    expect(result.instances![0]).toContain("Eq");
  });

  it("parses a class", () => {
    const output = `class Functor f where
  fmap :: (a -> b) -> f a -> f b
instance Functor Maybe
instance Functor []`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("class");
    expect(result.name).toBe("Functor");
    expect(result.instances).toHaveLength(2);
  });

  it("parses a function", () => {
    const output = "length :: forall (t :: * -> *) a. Foldable t => t a -> Int";
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("function");
    expect(result.name).toBe("length");
    expect(result.instances).toBeUndefined();
  });

  it("parses a newtype", () => {
    const output = "newtype Identity a = Identity a";
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("newtype");
    expect(result.name).toBe("Identity");
  });

  it("parses a type synonym", () => {
    const output = "type String = [Char]";
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("type-synonym");
    expect(result.name).toBe("String");
  });

  it("returns unknown for unrecognized input", () => {
    const result = parseInfoOutput("something weird");
    expect(result.kind).toBe("unknown");
  });

  // --- NEW: Kind classification from annotations (Bug Fix 4) ---
  it("classifies data type from kind annotation", () => {
    const output = `type Maybe :: * -> *\ndata Maybe a = Nothing | Just a\n  \t-- Defined in 'GHC.Maybe'\ninstance Eq a => Eq (Maybe a)`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("data");
    expect(result.name).toBe("Maybe");
  });

  it("classifies class from kind annotation", () => {
    const output = `type Container :: (* -> *) -> Constraint\nclass Container f where\n  empty :: f a\n  insert :: a -> f a -> f a`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("class");
    expect(result.name).toBe("Container");
  });

  it("classifies newtype from role annotation", () => {
    const output = `type role Wrap representational nominal\ntype Wrap :: forall {k}. (k -> *) -> k -> *\nnewtype Wrap f a = Wrap {unWrap :: f a}`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("newtype");
    expect(result.name).toBe("Wrap");
  });

  it("keeps type-synonym for actual type synonyms", () => {
    const output = `type String = [Char]\n  \t-- Defined in 'GHC.Internal.Base'`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("type-synonym");
  });

  it("classifies data with no instances", () => {
    const output = `type Either :: * -> * -> *\ndata Either a b = Left a | Right b`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("data");
    expect(result.name).toBe("Either");
    expect(result.instances).toBeUndefined();
  });

  it("handles class with superclass constraint", () => {
    const output = `class (Eq a) => Ord a where\n  compare :: a -> a -> Ordering\ninstance Ord Int`;
    const result = parseInfoOutput(output);
    expect(result.kind).toBe("class");
    expect(result.name).toBe("Ord");
  });

  it("handles empty input", () => {
    const result = parseInfoOutput("");
    expect(result.kind).toBe("unknown");
    expect(result.name).toBe("");
  });
});
