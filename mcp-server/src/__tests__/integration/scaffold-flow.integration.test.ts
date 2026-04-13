import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp, rm, mkdir, writeFile, readFile } from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import { handleScaffold } from "../../tools/scaffold.js";

/**
 * Integration test: scaffold → verify stubs → verify imports
 *
 * Tests the full scaffold flow that broke during the parser-combinators dogfooding:
 * 1. Create a .cabal with multiple modules
 * 2. Scaffold with mixed data types + function signatures
 * 3. Verify stubs compile correctly (data verbatim, functions with = undefined)
 * 4. Verify cross-module imports are generated
 * 5. Verify minimal stub overwrite works
 */

describe("scaffold flow integration", () => {
  let tmpDir: string;

  beforeEach(async () => {
    tmpDir = await mkdtemp(path.join(os.tmpdir(), "scaffold-flow-"));
    await mkdir(path.join(tmpDir, "src", "Parser"), { recursive: true });
    await writeFile(
      path.join(tmpDir, "test.cabal"),
      `cabal-version: 2.4
name: test-parser
version: 0.1.0.0

library
  exposed-modules:
    Parser.Error
    Parser.Core
    Parser.Char
    Parser.Combinators
    Parser.Run
  hs-source-dirs: src
  build-depends: base >= 4.14 && < 5, QuickCheck >= 2.14
  default-language: Haskell2010
  ghc-options: -Wall
`
    );
  });

  afterEach(async () => {
    await rm(tmpDir, { recursive: true, force: true });
  });

  it("creates all 5 modules with correct stubs from scratch", async () => {
    const result = JSON.parse(
      await handleScaffold(tmpDir, {
        "Parser.Error": [
          "data Pos = Pos { posLine :: Int, posCol :: Int } deriving (Show, Eq, Ord)",
          "data ParseError = ParseError { errPos :: Pos, errExpected :: [String], errFound :: String } deriving (Show, Eq)",
          "initialPos :: Pos",
          "advancePos :: Pos -> Char -> Pos",
          "mergeErrors :: ParseError -> ParseError -> ParseError",
        ],
        "Parser.Core": [
          "newtype Parser a = Parser { unParser :: String -> Pos -> Either ParseError (a, String, Pos) }",
          "runParser :: Parser a -> String -> Either ParseError (a, String, Pos)",
          "satisfy :: String -> (Char -> Bool) -> Parser Char",
        ],
        "Parser.Char": [
          "char :: Char -> Parser Char",
          "digit :: Parser Char",
          "letter :: Parser Char",
        ],
        "Parser.Combinators": [
          "choice :: [Parser a] -> Parser a",
          "between :: Parser open -> Parser close -> Parser a -> Parser a",
          "chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a",
        ],
        "Parser.Run": [
          "parse :: Parser a -> String -> Either ParseError a",
          "parseTest :: Show a => Parser a -> String -> IO ()",
        ],
      })
    );

    expect(result.success).toBe(true);
    expect(result.created).toHaveLength(5);

    // Parser.Error — data declarations verbatim, functions with = undefined
    const errorHs = await readFile(path.join(tmpDir, "src/Parser/Error.hs"), "utf-8");
    expect(errorHs).toContain("module Parser.Error where");
    expect(errorHs).toContain("data Pos = Pos { posLine :: Int, posCol :: Int } deriving (Show, Eq, Ord)");
    expect(errorHs).toContain("data ParseError = ParseError { errPos :: Pos");
    expect(errorHs).toContain("initialPos :: Pos");
    expect(errorHs).toContain("initialPos = undefined");
    expect(errorHs).toContain("advancePos = undefined");
    // Data declarations should NOT have = undefined
    expect(errorHs).not.toMatch(/data Pos.*= undefined/);
    // Error module should NOT import from itself
    expect(errorHs).not.toContain("import Parser.Error");

    // Parser.Core — imports Pos and ParseError from Parser.Error
    const coreHs = await readFile(path.join(tmpDir, "src/Parser/Core.hs"), "utf-8");
    expect(coreHs).toContain("module Parser.Core where");
    expect(coreHs).toContain("import Parser.Error (ParseError, Pos)");
    expect(coreHs).toContain("newtype Parser a = Parser { unParser :: String -> Pos -> Either ParseError (a, String, Pos) }");
    expect(coreHs).toContain("runParser = undefined");
    expect(coreHs).toContain("satisfy = undefined");

    // Parser.Char — imports Parser from Parser.Core
    const charHs = await readFile(path.join(tmpDir, "src/Parser/Char.hs"), "utf-8");
    expect(charHs).toContain("import Parser.Core (Parser)");
    expect(charHs).toContain("char :: Char -> Parser Char");
    expect(charHs).toContain("char = undefined");

    // Parser.Combinators — imports Parser from Parser.Core
    const combHs = await readFile(path.join(tmpDir, "src/Parser/Combinators.hs"), "utf-8");
    expect(combHs).toContain("import Parser.Core (Parser)");
    expect(combHs).toContain("chainl1 = undefined");

    // Parser.Run — imports from both Parser.Core and Parser.Error
    const runHs = await readFile(path.join(tmpDir, "src/Parser/Run.hs"), "utf-8");
    expect(runHs).toContain("import Parser.Core (Parser)");
    expect(runHs).toContain("import Parser.Error (ParseError)");
    expect(runHs).toContain("parse = undefined");
    expect(runHs).toContain("parseTest = undefined");
  });

  it("auto-scaffold then typed scaffold overwrites minimal stubs", async () => {
    // Step 1: auto-scaffold creates minimal stubs (simulates ghci_switch_project)
    const auto = JSON.parse(await handleScaffold(tmpDir));
    expect(auto.created).toHaveLength(5);

    // Verify minimal stubs
    const minStub = await readFile(path.join(tmpDir, "src/Parser/Error.hs"), "utf-8");
    expect(minStub.trim()).toBe("module Parser.Error where");

    // Step 2: typed scaffold overwrites minimal stubs with signatures
    const typed = JSON.parse(
      await handleScaffold(tmpDir, {
        "Parser.Error": [
          "data Pos = Pos Int Int",
          "initialPos :: Pos",
        ],
        "Parser.Core": [
          "newtype Parser a = Parser (String -> Either String (a, String))",
          "satisfy :: (Char -> Bool) -> Parser Char",
        ],
      })
    );

    // Both modules should be in created (overwritten from minimal)
    expect(typed.created).toContain("src/Parser/Error.hs");
    expect(typed.created).toContain("src/Parser/Core.hs");
    // Modules without signatures stay as-is
    expect(typed.alreadyExist).toContain("src/Parser/Char.hs");

    // Verify content was overwritten
    const errorHs = await readFile(path.join(tmpDir, "src/Parser/Error.hs"), "utf-8");
    expect(errorHs).toContain("data Pos = Pos Int Int");
    expect(errorHs).toContain("initialPos = undefined");

    const coreHs = await readFile(path.join(tmpDir, "src/Parser/Core.hs"), "utf-8");
    expect(coreHs).toContain("newtype Parser a = Parser");
    expect(coreHs).toContain("satisfy = undefined");
  });

  it("prefers public modules over .Internal in Hoogle results", async () => {
    // If the cabal has containers as dependency and a signature uses Map,
    // scaffold should import Data.Map (not Data.Map.Internal)
    await writeFile(
      path.join(tmpDir, "test.cabal"),
      `cabal-version: 2.4
name: test-parser
version: 0.1.0.0

library
  exposed-modules: Lib
  hs-source-dirs: src
  build-depends: base >= 4.14 && < 5, containers >= 0.6
  default-language: Haskell2010
`
    );
    await mkdir(path.join(tmpDir, "src"), { recursive: true });

    await handleScaffold(tmpDir, {
      "Lib": ["type Env = Map String Int", "lookup :: String -> Env -> Maybe Int"],
    });

    const content = await readFile(path.join(tmpDir, "src/Lib.hs"), "utf-8");
    // If Hoogle resolved Map, it should NOT be Data.Map.Internal
    if (content.includes("import")) {
      expect(content).not.toContain("Internal");
    }
  });

  it("does not overwrite modules with real implementations", async () => {
    // Create a module with real code
    await mkdir(path.join(tmpDir, "src", "Parser"), { recursive: true });
    await writeFile(
      path.join(tmpDir, "src/Parser/Error.hs"),
      "module Parser.Error where\n\ndata Pos = Pos Int Int deriving Show\n\ninitialPos :: Pos\ninitialPos = Pos 1 1\n"
    );

    const result = JSON.parse(
      await handleScaffold(tmpDir, {
        "Parser.Error": ["data Pos = Pos Float Float", "foo :: Int"],
      })
    );

    // Should NOT overwrite — file has real content beyond minimal stub
    expect(result.alreadyExist).toContain("src/Parser/Error.hs");
    expect(result.created).not.toContain("src/Parser/Error.hs");

    // Verify original content preserved
    const content = await readFile(path.join(tmpDir, "src/Parser/Error.hs"), "utf-8");
    expect(content).toContain("initialPos = Pos 1 1");
  });
});
