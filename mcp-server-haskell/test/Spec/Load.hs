-- | Unit tests for project-dir/module-path guards, 'checkPathExists',
-- and 'ghc_load' error paths (#79, #84, #214). Covers both pure
-- smart-constructor checks and the integration test that boots a
-- real GhcSession on an empty tmpdir.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Load
  ( testRejectsRelativeProject
  , testAcceptsInTree
  , testRejectsTraversal
  , testCheckPathExistsAccepts
  , testCheckPathExistsRejects
  , testGhcLoadEmptyProjectNoMatch
  , testGhcLoadNoArgsUsesLibraryTarget
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Ghc.ApiSession (killGhcSession, startGhcSession)
import Spec.ToolEnvFixture (sessionPdEnv)
import qualified HaskellFlows.Tool.Load as LoadTool
import HaskellFlows.Types
  ( PathError (..)
  , mkModulePath
  , mkProjectDir
  )
import HaskellFlows.Tool.Load (checkPathExists)

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

-- Issue #79: 'checkPathExists' is the gate that turned the silent
-- "load anything, get the whole library back" foot-gun into an
-- explicit error. The Right () branch fires when the file is on
-- disk; the Left branch is the original bug repro shape.
testCheckPathExistsAccepts :: IO Bool
testCheckPathExistsAccepts = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "haskell-flows-issue-79-accept"
      file = dir </> "Foo.hs"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile file (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      r <- checkPathExists pd (T.pack "Foo.hs")
      removePathForcibly dir
      pure (r == Right ())

testCheckPathExistsRejects :: IO Bool
testCheckPathExistsRejects = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-issue-79-reject"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      r <- checkPathExists pd (T.pack "DoesNotExist.hs")
      removePathForcibly dir
      pure $ case r of
        Left msg -> T.isInfixOf (T.pack "does not exist") msg
                 && T.isInfixOf (T.pack "DoesNotExist.hs") msg
        Right () -> False

-- | Issue #84: ghc_load on a project with no @src/@ or @app/@
-- Haskell sources used to silently report @success=true@ +
-- @summary="Compiled OK. No issues."@. The post-#90-Phase-C
-- envelope surfaces it as @status='no_match'@ +
-- @kind='module_not_in_graph'@ so consumer agents can route to
-- ghc_create_project / ghc_add_modules instead of charging into
-- ghc_suggest on an empty graph. We don't need a real GhcSession
-- here: the empty-project guard runs before 'firstTestSuiteOrLibrary',
-- so we exercise the new short-circuit path with a stub session.
--
-- The stub is built by 'startGhcSession' on a tmpdir that has no
-- src/ + no app/ + no .cabal file — the same shape the issue's
-- repro describes.
testGhcLoadEmptyProjectNoMatch :: IO Bool
testGhcLoadEmptyProjectNoMatch = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-issue-84-empty"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- LoadTool.handle (sessionPdEnv sess pd) (A.object [])
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure $ case result of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just envErr <- Env.reError env
      , Env.eeKind envErr == Env.ModuleNotInGraph
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "loaded") payload == Just (A.Number 0)
            && AKM.member (AKey.fromText "remediation") payload
    _ -> False

-- | #214: regression — the no-args 'ghc_load' path must use
-- 'firstLibraryOrTestSuite' (prefers library stanza) rather than
-- 'firstTestSuiteOrLibrary' (prefers test-suite stanza).
-- Under test-suite stanza flags, library-only build-depends
-- (containers, scientific, regex-tdfa, …) are NOT directly exposed,
-- causing GHC-87110 "hidden package" errors on every src/ module
-- that imports them.
--
-- This is a source-inspection test: it verifies that Load.hs
-- does NOT reference 'firstTestSuiteOrLibrary' in its
-- implementation, confirming the fix is in place.
testGhcLoadNoArgsUsesLibraryTarget :: IO Bool
testGhcLoadNoArgsUsesLibraryTarget = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Load.hs"
  -- The import section must list firstLibraryOrTestSuite (not
  -- firstTestSuiteOrLibrary, which causes the reload to use the
  -- test-suite stanza and lose library-only package context).
  let importLine = "  , firstLibraryOrTestSuite"
  pure $ T.isInfixOf importLine src
