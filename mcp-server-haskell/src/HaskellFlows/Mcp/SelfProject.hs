-- | Detect whether the active project directory IS the source tree
-- of the haskell-flows MCP itself.
--
-- Rationale: when an agent is editing this codebase, the dogfood-fix-
-- in-place flow applies (Edit → Spec.hs regression test → commit+push
-- direct to master, keep going with stale binary). For ANY OTHER
-- project, that flow is irrelevant noise. Self-detection lets the
-- MCP nudge the dogfood flow only when it matters.
--
-- Heuristic: read @<projectDir>/haskell-flows-mcp.cabal@ (or
-- @<projectDir>/mcp-server-haskell/haskell-flows-mcp.cabal@ for the
-- repo-root case) and check the @name:@ field equals
-- @"haskell-flows-mcp"@.
--
-- Why cabal-name and not e.g. binary path:
--
--   * Independent of binary location — works for @cabal repl@, dev
--     clones, or a fork that kept the project name.
--   * Cheap (one stat + ~200 byte read).
--   * Robust to alt-named forks: if you renamed the package, you're
--     "not self" by design — the dogfood prompt wouldn't make sense
--     for your fork's workflow anyway.
--   * Beats env vars (footgun if stale across @ghc_project(switch)@).
--
-- Security: read-only. Reads exactly two file paths under the active
-- project directory, never escapes via "..", never spawns subprocesses,
-- never reaches network. All exceptions collapse to 'False' — a
-- detector that errs MUST err on the safe side ("not self") so the
-- dogfood prompt doesn't fire spuriously on someone else's project.
module HaskellFlows.Mcp.SelfProject
  ( detectSelfProject
  , parseCabalNameField
  , selfMutableSubdirs
  , selfCabalName
  ) where

import Control.Exception (SomeException, try)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | The cabal @name:@ field that identifies a haskell-flows MCP source
-- tree. Forks that rename the package are "not self" by design.
selfCabalName :: Text
selfCabalName = "haskell-flows-mcp"

-- | Subdirectories under a self-project where edits trigger the
-- dogfood-flow nudge. Relative to 'unProjectDir'. PR-4 starts with a
-- conservative set; 'enrichWithNextStep' tests @module_path@ against
-- this list before injecting the dogfood hint.
--
-- The list covers two layouts the agent sees in the wild:
--
--   * The 'mcp-server-haskell' subtree as 'projectDir' (cabal repl,
--     direct edit) — paths are @src/...@, @test/...@.
--   * The repo root as 'projectDir' (top-level integration) — paths
--     are @mcp-server-haskell/src/...@, @mcp-server-haskell/test/...@.
selfMutableSubdirs :: [FilePath]
selfMutableSubdirs =
  [ "src"
  , "test"
  , "test-e2e"
  , "mcp-server-haskell/src"
  , "mcp-server-haskell/test"
  , "mcp-server-haskell/test-e2e"
  ]

-- | Detect whether 'projectDir' is a haskell-flows MCP source tree.
-- Tries the cabal file directly under the projectDir first, then
-- under @<projectDir>/mcp-server-haskell/@ for the repo-root layout.
-- All errors (missing file, malformed cabal, IO exceptions) collapse
-- to 'False' — when in doubt, assume not-self and skip the dogfood
-- nudge.
detectSelfProject :: ProjectDir -> IO Bool
detectSelfProject pd = do
  let candidates =
        [ unProjectDir pd </> "haskell-flows-mcp.cabal"
        , unProjectDir pd </> "mcp-server-haskell" </> "haskell-flows-mcp.cabal"
        ]
  results <- traverse tryReadName candidates
  pure (Just selfCabalName `elem` results)
  where
    tryReadName :: FilePath -> IO (Maybe Text)
    tryReadName p = do
      ok <- doesFileExist p
      if not ok
        then pure Nothing
        else do
          eContents <- try (TIO.readFile p) :: IO (Either SomeException Text)
          case eContents of
            Left _  -> pure Nothing
            Right t -> pure (parseCabalNameField t)

-- | Extract the @name:@ field value from cabal file contents.
-- Returns 'Nothing' if the field is absent or malformed.
-- Case-insensitive on the field name; trims surrounding whitespace
-- on the value. Stops at the first @name:@ line — multiple ones in
-- a single .cabal would be a cabal error anyway.
parseCabalNameField :: Text -> Maybe Text
parseCabalNameField contents = listToMaybe
  [ T.strip rest
  | line <- T.lines contents
  , let stripped = T.strip line
  , let (key, afterColon) = T.breakOn ":" stripped
  , T.toLower (T.strip key) == "name"
  , Just rest <- [T.stripPrefix ":" afterColon]
  ]
