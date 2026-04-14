import { describe, expect, it } from "vitest";
import {
  extractImports,
  mapImportsToPackages,
  extractTestSuiteDeps,
} from "../tools/validate-cabal.js";

describe("extractImports", () => {
  it("extracts qualified and plain imports", () => {
    const code = `import qualified Data.Map.Strict as Map
import Data.List`;
    expect(extractImports(code)).toEqual([
      { module: "Data.Map.Strict", qualified: true, as: "Map" },
      { module: "Data.List", qualified: false },
    ]);
  });

  it("ignores non-import lines", () => {
    const code = `module Main where

main :: IO ()
main = pure ()`;
    expect(extractImports(code)).toEqual([]);
  });
});

describe("mapImportsToPackages", () => {
  it("maps Data.Map.Strict to containers", () => {
    expect(mapImportsToPackages(["Data.Map.Strict"])).toContain("containers");
  });

  it("maps Data.Text to text", () => {
    expect(mapImportsToPackages(["Data.Text"])).toContain("text");
  });

  it("maps Data.Vector to vector", () => {
    expect(mapImportsToPackages(["Data.Vector"])).toContain("vector");
  });

  it("ignores unknown modules", () => {
    expect(mapImportsToPackages(["Unknown.Module"])).toEqual([]);
  });
});

describe("extractTestSuiteDeps", () => {
  it("extracts dependencies from multiline stanza", () => {
    const cabal = `test-suite my-test
  type:             exitcode-stdio-1.0
  build-depends:
    base >= 4.20,
    containers,
    QuickCheck >= 2.14`;

    expect(extractTestSuiteDeps(cabal)).toEqual(["QuickCheck", "base", "containers"]);
  });

  it("extracts inline and continuation build-depends", () => {
    const cabal = `test-suite my-test
  build-depends: base >= 4.20,
                 containers,
                 text >= 2.0`;

    expect(extractTestSuiteDeps(cabal)).toEqual(["base", "containers", "text"]);
  });
});
