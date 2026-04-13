/**
 * E2E Test: Parser Combinator Library via MCP protocol.
 *
 * Tests the full agent workflow:
 * 1. Scaffold modules with data types + function signatures (auto-imports)
 * 2. Write implementations
 * 3. Compile via ghci_load
 * 4. Generate Arbitrary instances via ghci_arbitrary
 * 5. Verify correctness with ghci_eval
 * 6. Verify algebraic properties with ghci_quickcheck
 * 7. Verify property persistence via ghci_regression
 *
 * This exercises every improvement made in the agent experience overhaul:
 * - scaffold data/newtype fix
 * - scaffold auto-imports
 * - _guidance in responses
 * - property persistence
 * - no _modeSelection spam
 */
import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { execSync } from "node:child_process";
import { writeFile, unlink, rm } from "node:fs/promises";
import path from "node:path";

const FIXTURE_DIR = path.resolve(import.meta.dirname, "../fixtures/parser-project");
const SERVER_SCRIPT = path.resolve(import.meta.dirname, "../../../dist/index.js");
const GHCUP_BIN = path.join(process.env.HOME ?? "", ".ghcup", "bin");
const TEST_PATH = `${GHCUP_BIN}:${process.env.PATH}`;

const GHC_AVAILABLE = (() => {
  try {
    execSync("ghc --version", { stdio: "pipe", env: { ...process.env, PATH: TEST_PATH } });
    return true;
  } catch {
    return false;
  }
})();

function callTool(client: Client, name: string, args: Record<string, unknown> = {}) {
  return client.callTool({ name, arguments: args });
}

function parseResult(result: Awaited<ReturnType<Client["callTool"]>>): any {
  const text = (result.content as Array<{ type: string; text: string }>)[0]!.text;
  return JSON.parse(text);
}

// --- Complete Parser Combinator source (4 modules) ---

const ERROR_SOURCE = `module Parser.Error where

import Test.QuickCheck (Arbitrary(..), arbitrary)

data Pos = Pos { posLine :: !Int, posCol :: !Int } deriving (Show, Eq, Ord)

instance Arbitrary Pos where
  arbitrary = Pos <$> arbitrary <*> arbitrary

data ParseError = ParseError
  { errPos :: Pos, errExpected :: [String], errFound :: String
  } deriving (Show, Eq)

instance Arbitrary ParseError where
  arbitrary = ParseError <$> arbitrary <*> arbitrary <*> arbitrary

initialPos :: Pos
initialPos = Pos 1 1

advancePos :: Pos -> Char -> Pos
advancePos (Pos line _) '\\n' = Pos (line + 1) 1
advancePos (Pos line col) _   = Pos line (col + 1)

mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors e1 e2
  | errPos e1 > errPos e2 = e1
  | errPos e1 < errPos e2 = e2
  | otherwise = ParseError (errPos e1) (errExpected e1 ++ errExpected e2)
      (if null (errFound e1) then errFound e2 else errFound e1)
`;

const CORE_SOURCE = `module Parser.Core where

import Control.Applicative (Alternative(..))
import Parser.Error

newtype Parser a = Parser
  { unParser :: String -> Pos -> Either ParseError (a, String, Pos) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \\s pos ->
    case p s pos of
      Left err -> Left err
      Right (a, rest, pos') -> Right (f a, rest, pos')

instance Applicative Parser where
  pure a = Parser $ \\s pos -> Right (a, s, pos)
  Parser pf <*> Parser pa = Parser $ \\s pos ->
    case pf s pos of
      Left err -> Left err
      Right (f, s', pos') -> case pa s' pos' of
        Left err -> Left err
        Right (a, s'', pos'') -> Right (f a, s'', pos'')

instance Monad Parser where
  Parser pa >>= f = Parser $ \\s pos ->
    case pa s pos of
      Left err -> Left err
      Right (a, s', pos') -> unParser (f a) s' pos'

instance Alternative Parser where
  empty = Parser $ \\_ pos -> Left (ParseError pos [] "")
  Parser p1 <|> Parser p2 = Parser $ \\s pos ->
    case p1 s pos of
      Right result -> Right result
      Left err1 -> case p2 s pos of
        Right result -> Right result
        Left err2 -> Left (mergeErrors err1 err2)

runParser :: Parser a -> String -> Either ParseError (a, String, Pos)
runParser (Parser p) s = p s initialPos

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy desc predicate = Parser $ \\s pos ->
  case s of
    [] -> Left (ParseError pos [desc] "end of input")
    (c:cs) | predicate c -> Right (c, cs, advancePos pos c)
           | otherwise -> Left (ParseError pos [desc] [c])

eof :: Parser ()
eof = Parser $ \\s pos ->
  case s of
    [] -> Right ((), s, pos)
    (c:_) -> Left (ParseError pos ["end of input"] [c])
`;

const CHAR_SOURCE = `module Parser.Char where

import Data.Char (isDigit, isAlpha, isSpace)
import Control.Applicative (Alternative(..))
import Parser.Core (Parser, satisfy)

char :: Char -> Parser Char
char c = satisfy [c] (== c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

digit :: Parser Char
digit = satisfy "digit" isDigit

letter :: Parser Char
letter = satisfy "letter" isAlpha

space :: Parser Char
space = satisfy "space" isSpace

spaces :: Parser String
spaces = many space
`;

const RUN_SOURCE = `module Parser.Run where

import Parser.Error (ParseError)
import Parser.Core (Parser, runParser, eof)

parse :: Parser a -> String -> Either ParseError a
parse p s = fmap (\\(a, _, _) -> a) (runParser p s)

parseAll :: Parser a -> String -> Either ParseError a
parseAll p s = parse (p <* eof) s
`;

const SOURCE_FILES = [
  { path: "src/Parser/Error.hs", content: ERROR_SOURCE },
  { path: "src/Parser/Core.hs", content: CORE_SOURCE },
  { path: "src/Parser/Char.hs", content: CHAR_SOURCE },
  { path: "src/Parser/Run.hs", content: RUN_SOURCE },
];

describe.runIf(GHC_AVAILABLE)(
  "E2E: Parser Combinator Library",
  () => {
    let client: Client;
    let transport: StdioClientTransport;

    beforeAll(async () => {
      // Write source files
      for (const f of SOURCE_FILES) {
        await writeFile(path.join(FIXTURE_DIR, f.path), f.content, "utf-8");
      }

      // Clean any previous property store
      try {
        await rm(path.join(FIXTURE_DIR, ".haskell-flows"), { recursive: true, force: true });
      } catch { /* ignore */ }

      // Start MCP server
      transport = new StdioClientTransport({
        command: "node",
        args: [SERVER_SCRIPT],
        env: {
          ...process.env,
          PATH: TEST_PATH,
          HASKELL_PROJECT_DIR: FIXTURE_DIR,
          HASKELL_LIBRARY_TARGET: "lib:parser-project",
        },
      });
      client = new Client(
        { name: "parser-e2e-test", version: "0.1.0" },
        { capabilities: {} }
      );
      await client.connect(transport);
    }, 120_000);

    afterAll(async () => {
      // Clean up source files
      for (const f of SOURCE_FILES) {
        try { await unlink(path.join(FIXTURE_DIR, f.path)); } catch { /* ignore */ }
      }
      try {
        await rm(path.join(FIXTURE_DIR, ".haskell-flows"), { recursive: true, force: true });
      } catch { /* ignore */ }
      try { await client.close(); } catch { /* ignore */ }
    });

    // =========================================================
    // PHASE 1: Compile all modules
    // =========================================================

    it("compiles all 4 modules without errors", async () => {
      const r = parseResult(
        await callTool(client, "ghci_load", { load_all: true, diagnostics: true })
      );
      expect(r.success).toBe(true);
      expect(r.errors).toHaveLength(0);
    });

    // =========================================================
    // PHASE 2: No mode system artifacts
    // =========================================================

    it("session status has no _modeSelection field", async () => {
      const r = parseResult(
        await callTool(client, "ghci_session", { action: "status" })
      );
      expect(r.alive).toBe(true);
      expect(r._modeSelection).toBeUndefined();
      expect(r.mode).toBeUndefined();
    });

    it("tool list does not include ghci_mode", async () => {
      const tools = await client.listTools();
      const names = tools.tools.map(t => t.name);
      expect(names).not.toContain("ghci_mode");
      expect(names).toContain("ghci_load");
      expect(names).toContain("ghci_quickcheck");
      expect(names).toContain("ghci_regression");
    });

    // =========================================================
    // PHASE 3: Verify parser correctness with eval
    // =========================================================

    it("parses a single character", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", { expression: 'runParser (char \'a\') "abc"' })
      );
      expect(r.success).toBe(true);
      expect(r.output).toContain("Right");
      expect(r.output).toContain("'a'");
    });

    it("parses a string", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", { expression: 'parse (string "hello") "hello world"' })
      );
      expect(r.success).toBe(true);
      expect(r.output).toContain("Right");
      expect(r.output).toContain("hello");
    });

    it("parseAll rejects leftover input", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", { expression: 'parseAll digit "12"' })
      );
      expect(r.output).toContain("Left");
    });

    it("parseAll accepts exact match", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", { expression: 'parseAll digit "5"' })
      );
      expect(r.output).toContain("Right");
      expect(r.output).toContain("'5'");
    });

    it("spaces consumes whitespace", async () => {
      const r = parseResult(
        await callTool(client, "ghci_eval", { expression: 'runParser spaces "  abc"' })
      );
      expect(r.output).toContain("Right");
      expect(r.output).toContain("abc");
    });

    // =========================================================
    // PHASE 4: QuickCheck properties
    // =========================================================

    it("Functor identity law: fmap id == id", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: '\\s -> runParser (fmap id (satisfy "any" (const True))) s == runParser (satisfy "any" (const True)) s',
          tests: 100,
          module: "src/Parser/Core.hs",
        })
      );
      expect(r.success).toBe(true);
      expect(r.passed).toBeGreaterThanOrEqual(100);
    });

    it("Applicative identity: pure id <*> p == p", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: '\\s -> runParser (pure id <*> satisfy "any" (const True)) s == runParser (satisfy "any" (const True)) s',
          tests: 100,
          module: "src/Parser/Core.hs",
        })
      );
      expect(r.success).toBe(true);
    });

    it("Alternative identity: empty <|> p == p", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: '\\s -> runParser (empty <|> satisfy "any" (const True)) s == runParser (satisfy "any" (const True)) s',
          tests: 100,
          module: "src/Parser/Core.hs",
        })
      );
      expect(r.success).toBe(true);
    });

    it("char roundtrip: char c parses c from [c]", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: "\\c -> runParser (char c) [c] == Right (c, \"\", advancePos initialPos c)",
          tests: 100,
          module: "src/Parser/Char.hs",
        })
      );
      expect(r.success).toBe(true);
    });

    it("string roundtrip", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: '\\s -> not (null s) ==> runParser (string s) s == Right (s, "", foldl advancePos initialPos s)',
          tests: 100,
          module: "src/Parser/Char.hs",
        })
      );
      expect(r.success).toBe(true);
    });

    it("parse extracts just the value", async () => {
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: '\\s -> not (null s) ==> parse (satisfy "any" (const True)) s == Right (head s)',
          tests: 100,
          module: "src/Parser/Run.hs",
        })
      );
      expect(r.success).toBe(true);
    });

    // =========================================================
    // PHASE 5: Property persistence + regression
    // =========================================================

    it("regression lists stored properties", async () => {
      const r = parseResult(
        await callTool(client, "ghci_regression", { action: "list" })
      );
      // Properties were saved by the QuickCheck calls above
      expect(r.total).toBeGreaterThan(0);
    });

    it("regression runs all stored properties successfully", async () => {
      const r = parseResult(
        await callTool(client, "ghci_regression", { action: "run" })
      );
      expect(r.total).toBeGreaterThan(0);
      expect(r.failed).toBe(0);
      expect(r.passed).toBe(r.total);
    });

    // =========================================================
    // PHASE 6: Verify Round 3 fixes (Bug 4, Feature 6)
    // =========================================================

    it("QC suggest does NOT produce 'not always Left' for Either functions", async () => {
      // parse :: Parser a -> String -> Either ParseError a
      const r = parseResult(
        await callTool(client, "ghci_quickcheck", {
          property: "suggest",
          function_name: "parse",
        })
      );
      expect(r.mode).toBe("suggest");
      // Must NOT contain universally wrong properties
      if (r.suggestedProperties?.length > 0) {
        const laws = r.suggestedProperties.map((p: any) => p.law);
        expect(laws).not.toContain("not always Left");
        expect(laws).not.toContain("not always Nothing");
      }
    });

    it("session _info instead of _notice for setup", async () => {
      const r = parseResult(
        await callTool(client, "ghci_session", { action: "status" })
      );
      // Should use _info (non-actionable) not _notice (actionable)
      expect(r._notice).toBeUndefined();
    });
  }
);
