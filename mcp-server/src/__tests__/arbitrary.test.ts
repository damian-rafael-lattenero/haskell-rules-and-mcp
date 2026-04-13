import { describe, it, expect } from "vitest";
import { parseConstructors } from "../parsers/constructor-parser.js";
import { handleArbitrary } from "../tools/arbitrary.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("parseConstructors", () => {
  it("parses simple ADT with two constructors", () => {
    const result = parseConstructors("data Lit = LInt Integer | LBool Bool");
    expect(result).toHaveLength(2);
    expect(result[0]).toEqual({ name: "LInt", fields: ["Integer"] });
    expect(result[1]).toEqual({ name: "LBool", fields: ["Bool"] });
  });

  it("parses multi-field constructors", () => {
    const result = parseConstructors(
      "data Expr = Var Name | App Expr Expr | Lam Name Expr"
    );
    expect(result).toHaveLength(3);
    expect(result[0]).toEqual({ name: "Var", fields: ["Name"] });
    expect(result[1]).toEqual({ name: "App", fields: ["Expr", "Expr"] });
    expect(result[2]).toEqual({ name: "Lam", fields: ["Name", "Expr"] });
  });

  it("handles -- Defined at suffix", () => {
    const result = parseConstructors(
      "data Lit = LInt Integer | LBool Bool\n  \t-- Defined at src/Types.hs:5:1"
    );
    expect(result).toHaveLength(2);
    expect(result[0]!.name).toBe("LInt");
    expect(result[1]!.name).toBe("LBool");
  });

  it("handles empty constructors (unit type)", () => {
    const result = parseConstructors("data Unit = Unit");
    expect(result).toHaveLength(1);
    expect(result[0]).toEqual({ name: "Unit", fields: [] });
  });

  it("handles type kind annotation prefix", () => {
    const result = parseConstructors(
      "type Lit :: *\ndata Lit = LInt Integer | LBool Bool"
    );
    expect(result).toHaveLength(2);
    expect(result[0]!.name).toBe("LInt");
    expect(result[1]!.name).toBe("LBool");
  });

  it("handles newtype", () => {
    const result = parseConstructors("newtype Identity a = Identity a");
    expect(result).toHaveLength(1);
    expect(result[0]).toEqual({ name: "Identity", fields: ["a"] });
  });

  it("handles record syntax", () => {
    const result = parseConstructors(
      "data Person = Person { name :: String, age :: Int }"
    );
    expect(result).toHaveLength(1);
    expect(result[0]!.name).toBe("Person");
    expect(result[0]!.fields).toEqual(["String", "Int"]);
  });

  it("handles multiline definition joined", () => {
    const result = parseConstructors(
      "data Color\n  = Red\n  | Green\n  | Blue"
    );
    expect(result).toHaveLength(3);
    expect(result[0]!.name).toBe("Red");
    expect(result[1]!.name).toBe("Green");
    expect(result[2]!.name).toBe("Blue");
  });

  it("handles -- Defined in suffix", () => {
    const result = parseConstructors(
      "data Maybe a = Nothing | Just a\n  \t-- Defined in 'GHC.Maybe'"
    );
    expect(result).toHaveLength(2);
    expect(result[0]).toEqual({ name: "Nothing", fields: [] });
    expect(result[1]).toEqual({ name: "Just", fields: ["a"] });
  });
});

describe("handleArbitrary", () => {
  it("generates simple Arbitrary for non-recursive type", async () => {
    const session = createMockSession({
      infoOf: {
        output: "data Lit = LInt Integer | LBool Bool\n  \t-- Defined at src/Types.hs:5:1",
        success: true,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "Lit" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(true);
    expect(result.typeName).toBe("Lit");
    expect(result.isRecursive).toBe(false);
    expect(result.constructors).toHaveLength(2);
    expect(result.instance).toContain("instance Arbitrary (Lit)");
    expect(result.instance).toContain("oneof");
    expect(result.instance).toContain("LInt <$> arbitrary");
    expect(result.instance).toContain("LBool <$> arbitrary");
  });

  it("generates sized Arbitrary for recursive type", async () => {
    const session = createMockSession({
      infoOf: {
        output:
          "data Expr = Var Name | App Expr Expr | Lam Name Expr | Lit Lit\n  \t-- Defined at src/Types.hs:8:1",
        success: true,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "Expr" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(true);
    expect(result.typeName).toBe("Expr");
    expect(result.isRecursive).toBe(true);
    expect(result.constructors).toHaveLength(4);
    expect(result.instance).toContain("instance Arbitrary (Expr)");
    expect(result.instance).toContain("sized go");
    expect(result.instance).toContain("go 0");
    expect(result.instance).toContain("go n");
    expect(result.instance).toContain("sub");
    expect(result.instance).toContain("resize");
  });

  it("returns error for non-data types", async () => {
    const session = createMockSession({
      infoOf: {
        output: "type String = [Char]\n  \t-- Defined in 'GHC.Internal.Base'",
        success: true,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "String" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(false);
    expect(result.error).toContain("type-synonym");
  });

  it("returns error for Not in scope types", async () => {
    const session = createMockSession({
      infoOf: {
        output: "Not in scope: 'Nonexistent'",
        success: false,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "Nonexistent" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(false);
    expect(result.error).toContain("Not in scope");
  });

  it("handles type with kind annotation prefix", async () => {
    const session = createMockSession({
      infoOf: {
        output:
          "type Maybe :: * -> *\ndata Maybe a = Nothing | Just a\n  \t-- Defined in 'GHC.Maybe'\ninstance Eq a => Eq (Maybe a)",
        success: true,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "Maybe a" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(true);
    expect(result.typeName).toBe("Maybe a");
    expect(result.instance).toContain("(Arbitrary a) =>");
    expect(result.instance).toContain("instance (Arbitrary a) => Arbitrary (Maybe a)");
  });

  it("returns error for class types", async () => {
    const session = createMockSession({
      infoOf: {
        output: "class Functor f where\n  fmap :: (a -> b) -> f a -> f b",
        success: true,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "Functor" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(false);
    expect(result.error).toContain("class");
  });

  it("adds missing constraints when GHCi reports them (Bug Fix 4)", async () => {
    const session = createMockSession({
      infoOf: {
        output:
          "newtype Subst = Subst (Data.Map.Internal.Map String Type)\n  -- Defined at src/HM/Subst.hs:3:1",
        success: true,
      },
      executeBlock: {
        output: 'No instance for (Ord String) arising from a use of \'arbitrary\'\nNo instance for (Arbitrary Type) arising from a use of \'arbitrary\'',
        success: false,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "Subst" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(true);
    expect(result.instance).toContain("Ord String");
    expect(result.instance).toContain("Arbitrary Type");
    expect(result.hint).toContain("Added constraints");
  });

  it("does not add constraints when instance validates successfully", async () => {
    const session = createMockSession({
      infoOf: {
        output: "data Lit = LInt Integer | LBool Bool\n  -- Defined at src/HM/Syntax.hs:5:1",
        success: true,
      },
      executeBlock: {
        output: "",
        success: true,
      },
    });

    const raw = await handleArbitrary(session, { type_name: "Lit" });
    const result = JSON.parse(raw);

    expect(result.success).toBe(true);
    expect(result.instance).not.toContain("=>");
    expect(result.hint).not.toContain("Added constraints");
  });
});

describe("arbitrary — workflow state integration", () => {
  it("handleArbitrary returns success:true for valid types (register sets flag)", async () => {
    // The register wrapper in arbitrary.ts sets arbitraryInstancesDefined on success.
    // We test handleArbitrary directly — it returns { success: true } which the
    // register wrapper uses to trigger the update.
    const session = createMockSession({
      infoOf: {
        output: "data Pos = Pos Int Int\n  -- Defined at src/Pos.hs:1:1",
        success: true,
      },
      executeBlock: { output: "", success: true },
    });

    const raw = await handleArbitrary(session, { type_name: "Pos" });
    const result = JSON.parse(raw);
    expect(result.success).toBe(true);
    // The register function in arbitrary.ts checks this flag and calls
    // ctx.updateModuleProgress({ arbitraryInstancesDefined: true })
  });
});
