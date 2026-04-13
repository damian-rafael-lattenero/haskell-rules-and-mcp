import { describe, it, expect } from "vitest";
import { handleGoto, parseDefinitionLocation } from "../tools/goto.js";
import { createMockSession } from "./helpers/mock-session.js";

describe("parseDefinitionLocation", () => {
  it("parses 'Defined at' with file location", () => {
    const output = "add :: Int -> Int -> Int\n  \t-- Defined at src/TestLib.hs:3:1";
    const loc = parseDefinitionLocation(output);
    expect(loc).not.toBeNull();
    expect(loc!.file).toBe("src/TestLib.hs");
    expect(loc!.line).toBe(3);
    expect(loc!.column).toBe(1);
  });

  it("parses 'Defined in' with module name", () => {
    const output = "map :: (a -> b) -> [a] -> [b]\n  \t-- Defined in \u2018GHC.Base\u2019";
    const loc = parseDefinitionLocation(output);
    expect(loc).not.toBeNull();
    expect(loc!.module).toBe("GHC.Base");
    expect(loc!.file).toBeUndefined();
  });

  it("parses 'Defined in' with ASCII quotes", () => {
    const output = "map :: (a -> b) -> [a] -> [b]\n  \t-- Defined in 'GHC.Base'";
    const loc = parseDefinitionLocation(output);
    expect(loc).not.toBeNull();
    expect(loc!.module).toBe("GHC.Base");
  });

  it("returns null for output without definition", () => {
    expect(parseDefinitionLocation("just some text")).toBeNull();
  });
});

describe("handleGoto", () => {
  it("returns location for local definition", async () => {
    const session = createMockSession({
      infoOf: {
        output: "add :: Int -> Int -> Int\n  \t-- Defined at src/TestLib.hs:3:1",
        success: true,
      },
    });
    const result = JSON.parse(await handleGoto(session, { name: "add" }));
    expect(result.success).toBe(true);
    expect(result.location.file).toBe("src/TestLib.hs");
    expect(result.location.line).toBe(3);
  });

  it("returns module for library definition", async () => {
    const session = createMockSession({
      infoOf: {
        output: "map :: (a -> b) -> [a] -> [b]\n  \t-- Defined in 'GHC.Base'",
        success: true,
      },
    });
    const result = JSON.parse(await handleGoto(session, { name: "map" }));
    expect(result.success).toBe(true);
    expect(result.location.module).toBe("GHC.Base");
  });

  it("returns error for out-of-scope name", async () => {
    const session = createMockSession({
      infoOf: {
        output: "Variable not in scope: nonExistent",
        success: false,
      },
    });
    const result = JSON.parse(await handleGoto(session, { name: "nonExistent" }));
    expect(result.success).toBe(false);
  });
});
