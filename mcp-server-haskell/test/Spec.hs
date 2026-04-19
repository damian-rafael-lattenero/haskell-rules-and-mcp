-- | Minimal smoke test suite for Phase 1.
--
-- Covers the two security-critical invariants we lock in at scaffolding
-- time, so a regression here fails the build before any tool is wired up:
--
-- 1. 'mkModulePath' rejects paths that escape the project directory.
-- 2. The error parser can round-trip a canonical GHC diagnostic line.
--
-- QuickCheck arrives in Phase 2 along with the property-lifecycle tool.
module Main where

import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)

import HaskellFlows.Parser.Error (parseGhcErrors, Severity (..), GhcError (..))
import HaskellFlows.Types
  ( PathError (..)
  , mkModulePath
  , mkProjectDir
  )

main :: IO ()
main = do
  results <-
    sequence
      [ test "mkProjectDir rejects relative"    testRejectsRelativeProject
      , test "mkModulePath accepts in-tree"      testAcceptsInTree
      , test "mkModulePath rejects traversal"    testRejectsTraversal
      , test "parseGhcErrors extracts header"    testParseHeader
      ]
  if and results then exitSuccess else exitFailure

test :: String -> IO Bool -> IO Bool
test name action = do
  ok <- action
  putStrLn ((if ok then "PASS  " else "FAIL  ") <> name)
  pure ok

testRejectsRelativeProject :: IO Bool
testRejectsRelativeProject =
  pure $ case mkProjectDir "relative/path" of
    Left (PathNotAbsolute _) -> True
    _                        -> False

testAcceptsInTree :: IO Bool
testAcceptsInTree = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> pure $ case mkModulePath pd "src/Foo.hs" of
      Right _ -> True
      _       -> False

testRejectsTraversal :: IO Bool
testRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> pure $ case mkModulePath pd "../../etc/passwd" of
      Left (PathEscapesProject {}) -> True
      _                            -> False

testParseHeader :: IO Bool
testParseHeader =
  let raw = T.unlines
        [ "src/Foo.hs:12:5: error: [GHC-83865]"
        , "    Couldn't match expected type ‘Int’ with actual type ‘Bool’"
        , ""
        ]
  in pure $ case parseGhcErrors raw of
       [e] -> geSeverity e == SevError
           && geLine e == 12
           && geColumn e == 5
       _   -> False
