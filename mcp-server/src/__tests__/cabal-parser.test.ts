import { describe, it, expect } from "vitest";
import {
  extractPackageName,
  extractModules,
  moduleToFilePath,
} from "../parsers/cabal-parser.js";

const SAMPLE_CABAL = `cabal-version:      3.12
name:               haskell-rules-and-mcp
version:            0.1.0.0
synopsis:           Haskell rules and MCP experiments
license:            MIT

library
  exposed-modules:
    Lib
    HM.Syntax
    HM.Subst
    HM.Unify
    HM.Infer
    HM.Pretty
    Parser.Core
    Parser.Combinators
    Parser.Char
    Parser.HM
  build-depends:
    base >= 4.21 && < 5,
    containers >= 0.7
  hs-source-dirs:   src
  default-language:  GHC2024

executable haskell-rules-and-mcp
  main-is:          Main.hs
  build-depends:
    base >= 4.21 && < 5,
    haskell-rules-and-mcp
  hs-source-dirs:   app

test-suite properties
  type:             exitcode-stdio-1.0
  main-is:          Main.hs
  other-modules:    Properties
  build-depends:
    base >= 4.21 && < 5,
    QuickCheck >= 2.14,
    haskell-rules-and-mcp
  hs-source-dirs:   test
`;

describe("extractPackageName", () => {
  it("extracts name from cabal content", () => {
    expect(extractPackageName(SAMPLE_CABAL)).toBe("haskell-rules-and-mcp");
  });

  it("handles name with extra whitespace", () => {
    expect(extractPackageName("name:   my-project  \nversion: 0.1")).toBe(
      "my-project"
    );
  });

  it("returns null for content without name field", () => {
    expect(extractPackageName("version: 0.1\nlicense: MIT")).toBeNull();
  });

  it("returns null for empty content", () => {
    expect(extractPackageName("")).toBeNull();
  });

  it("is case-insensitive", () => {
    expect(extractPackageName("Name: MyProject")).toBe("MyProject");
  });
});

describe("extractModules", () => {
  it("extracts library modules", () => {
    const result = extractModules(SAMPLE_CABAL);
    expect(result.library).toContain("Lib");
    expect(result.library).toContain("HM.Syntax");
    expect(result.library).toContain("HM.Infer");
    expect(result.library).toContain("Parser.HM");
    expect(result.library).toHaveLength(10);
  });

  it("extracts empty library when no modules", () => {
    const content = `name: empty
library
  build-depends: base
`;
    const result = extractModules(content);
    expect(result.library).toEqual([]);
  });

  it("handles comma-separated modules", () => {
    const content = `name: test
library
  exposed-modules: Foo, Bar, Baz
  build-depends: base
`;
    const result = extractModules(content);
    expect(result.library).toEqual(["Foo", "Bar", "Baz"]);
  });

  it("handles inline modules on same line as field", () => {
    const content = `name: test
library
  exposed-modules: Foo
  build-depends: base
`;
    const result = extractModules(content);
    expect(result.library).toEqual(["Foo"]);
  });
});

describe("moduleToFilePath", () => {
  it("converts dotted module to path", () => {
    expect(moduleToFilePath("HM.Syntax")).toBe("src/HM/Syntax.hs");
  });

  it("handles single-component module", () => {
    expect(moduleToFilePath("Lib")).toBe("src/Lib.hs");
  });

  it("handles deeply nested module", () => {
    expect(moduleToFilePath("A.B.C.D")).toBe("src/A/B/C/D.hs");
  });

  it("uses custom source directory", () => {
    expect(moduleToFilePath("Foo", "lib")).toBe("lib/Foo.hs");
  });
});
