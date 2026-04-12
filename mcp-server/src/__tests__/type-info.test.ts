import { describe, it, expect } from "vitest";
import { handleTypeInfo } from "../tools/type-info.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("handleTypeInfo", () => {
  it("returns parsed info for data type", async () => {
    const session = createMockSession({
      infoOf: {
        output: "type Maybe :: * -> *\ndata Maybe a = Nothing | Just a\n  \t-- Defined in 'GHC.Maybe'\ninstance Eq a => Eq (Maybe a)",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeInfo(session, { name: "Maybe" }));
    expect(result.success).toBe(true);
    expect(result.kind).toBe("data");
    expect(result.name).toBe("Maybe");
    expect(result.instances).toContain("instance Eq a => Eq (Maybe a)");
  });

  it("returns parsed info for class", async () => {
    const session = createMockSession({
      infoOf: {
        output: "class Functor f where\n  fmap :: (a -> b) -> f a -> f b\ninstance Functor Maybe",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeInfo(session, { name: "Functor" }));
    expect(result.success).toBe(true);
    expect(result.kind).toBe("class");
    expect(result.name).toBe("Functor");
  });

  it("returns parsed info for function", async () => {
    const session = createMockSession({
      infoOf: {
        output: "map :: (a -> b) -> [a] -> [b]\n  \t-- Defined in 'GHC.Base'",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeInfo(session, { name: "map" }));
    expect(result.success).toBe(true);
    expect(result.kind).toBe("function");
  });

  it("returns error for unknown name", async () => {
    const session = createMockSession({
      infoOf: { output: "Not in scope: 'nonExistent'", success: false },
    });
    const result = JSON.parse(await handleTypeInfo(session, { name: "nonExistent" }));
    expect(result.success).toBe(false);
    expect(result.error).toContain("Not in scope");
  });

  it("returns parsed info for newtype with role annotation", async () => {
    const session = createMockSession({
      infoOf: {
        output: "type role Identity representational\ntype Identity :: * -> *\nnewtype Identity a = Identity a",
        success: true,
      },
    });
    const result = JSON.parse(await handleTypeInfo(session, { name: "Identity" }));
    expect(result.success).toBe(true);
    expect(result.kind).toBe("newtype");
    expect(result.name).toBe("Identity");
  });
});
