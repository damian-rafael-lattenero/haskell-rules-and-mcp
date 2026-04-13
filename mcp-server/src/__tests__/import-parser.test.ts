import { describe, it, expect } from "vitest";
import { parseImportsOutput } from "../parsers/import-parser.js";

describe("parseImportsOutput", () => {
  it("parses implicit Prelude import", () => {
    const result = parseImportsOutput("import Prelude -- implicit");
    expect(result).toHaveLength(1);
    expect(result[0]!.module).toBe("Prelude");
    expect(result[0]!.implicit).toBe(true);
    expect(result[0]!.qualified).toBe(false);
  });

  it("parses qualified import with alias", () => {
    const result = parseImportsOutput("import Data.Map.Strict qualified as Map");
    expect(result).toHaveLength(1);
    expect(result[0]!.module).toBe("Data.Map.Strict");
    expect(result[0]!.qualified).toBe(true);
    expect(result[0]!.alias).toBe("Map");
  });

  it("parses import with specific items", () => {
    const result = parseImportsOutput("import Data.List ( sort, nub )");
    expect(result).toHaveLength(1);
    expect(result[0]!.module).toBe("Data.List");
    expect(result[0]!.items).toEqual(["sort", "nub"]);
  });

  it("parses simple import", () => {
    const result = parseImportsOutput("import TestLib");
    expect(result).toHaveLength(1);
    expect(result[0]!.module).toBe("TestLib");
    expect(result[0]!.qualified).toBe(false);
    expect(result[0]!.implicit).toBe(false);
  });

  it("parses multiple imports", () => {
    const output =
      "import Prelude -- implicit\n" +
      "import Data.Map.Strict qualified as Map\n" +
      "import TestLib";
    const result = parseImportsOutput(output);
    expect(result).toHaveLength(3);
    expect(result[0]!.module).toBe("Prelude");
    expect(result[1]!.module).toBe("Data.Map.Strict");
    expect(result[2]!.module).toBe("TestLib");
  });

  it("handles empty output", () => {
    expect(parseImportsOutput("")).toEqual([]);
  });

  it("skips non-import lines", () => {
    const output = "ghci> :show imports\nimport TestLib\nOk.";
    const result = parseImportsOutput(output);
    expect(result).toHaveLength(1);
    expect(result[0]!.module).toBe("TestLib");
  });

  it("parses pre-qualified import syntax", () => {
    const result = parseImportsOutput("import qualified Data.Set as Set");
    expect(result).toHaveLength(1);
    expect(result[0]!.module).toBe("Data.Set");
    expect(result[0]!.qualified).toBe(true);
    expect(result[0]!.alias).toBe("Set");
  });
});
