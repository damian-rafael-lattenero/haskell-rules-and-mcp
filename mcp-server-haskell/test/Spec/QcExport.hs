-- | Unit tests for 'Tool.QuickCheckExport' path validation, render
-- helpers, file-guard logic, and property-store directory management.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.QcExport
  ( testExportPathSrc
  , testExportPathLib
  , testExportPathTest
  , testExportPathNested
  , testExportPathLowercaseRejected
  , testExportPathNoSuffix
  , testExportRenderValidImports
  , testExportRenderDropsSelfImport
  , testExportRenderUnionsLibMods
  , testExportRenderDedupesLibAndProps
  , testExportGuardNewFile
  , testExportGuardGeneratedFile
  , testExportGuardScaffoldFile
  , testExportGuardHandWritten
  , testExportGuardForce
  , testExportHandleRefusesHandWritten
  , testPropStoreCreatesDir
  , testPropStoreResurrectsDir
  , testPropStoreConcurrentSaves
  ) where

import qualified Data.Aeson as A
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (isJust, isNothing)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import HaskellFlows.Data.PropertyStore
  ( StoredProperty (..)
  , loadAll
  , openStore
  , save
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Types (ProjectDir, mkProjectDir)
import qualified HaskellFlows.Types
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport

import Spec.Helpers (withTempProject)

-- | Internal: unwrap ProjectDir to its FilePath. Exported in the
-- production module but not used elsewhere in this test file; keep
-- it inline here so we can stat / rm under the validated root.
unProjectDirRaw :: ProjectDir -> FilePath
unProjectDirRaw = HaskellFlows.Types.unProjectDir

-- | The classic cases: 'src/Foo.hs' -> 'Foo', 'src/Foo/Bar.hs' -> 'Foo.Bar'.
testExportPathSrc :: IO Bool
testExportPathSrc = pure $
     QcExport.modulePathToModule "src/Foo.hs"        == Just "Foo"
  && QcExport.modulePathToModule "src/Foo/Bar.hs"    == Just "Foo.Bar"

-- | Library convention alias. Same semantics as 'src/'.
testExportPathLib :: IO Bool
testExportPathLib = pure $
     QcExport.modulePathToModule "lib/Foo.hs"        == Just "Foo"
  && QcExport.modulePathToModule "lib/Foo/Bar.hs"    == Just "Foo.Bar"

-- | BUG-02 core: test-suite helpers like @test/Gen.hs@ containing
-- @module Gen where@ used to be mis-mapped to @test.Gen@ — lowercase
-- first segment, not a valid Haskell module name. Pin the fix.
testExportPathTest :: IO Bool
testExportPathTest = pure $
     QcExport.modulePathToModule "test/Gen.hs"       == Just "Gen"
  && QcExport.modulePathToModule "test/Support/Fix.hs" == Just "Support.Fix"

-- | Paths with no leading convention-dir: take the whole path as
-- the module name (each segment still has to start uppercase).
testExportPathNested :: IO Bool
testExportPathNested = pure $
     QcExport.modulePathToModule "Main.hs"           == Just "Main"
  && QcExport.modulePathToModule "Foo/Bar.hs"        == Just "Foo.Bar"

-- | Paths containing lowercase segments (non-canonical layouts)
-- must return 'Nothing' — the renderer will omit a broken import
-- rather than emit invalid Haskell.
testExportPathLowercaseRejected :: IO Bool
testExportPathLowercaseRejected = pure $
     isNothing (QcExport.modulePathToModule "experiments/foo.hs")
  && isNothing (QcExport.modulePathToModule "src/support/Gen.hs")
  && isNothing (QcExport.modulePathToModule "src/.hidden/Foo.hs")

-- | Non-Haskell files are outright rejected (no @.hs@ suffix).
testExportPathNoSuffix :: IO Bool
testExportPathNoSuffix = pure $
     isNothing (QcExport.modulePathToModule "src/Foo.txt")
  && isNothing (QcExport.modulePathToModule "src/Foo")
  && isNothing (QcExport.modulePathToModule "")

-- | End-to-end: a property whose stored module is 'test/Gen.hs'
-- must generate an @import Gen@ line — never the old broken
-- @import test.Gen@. Exercises the fix through 'renderTestFile'.
testExportRenderValidImports :: IO Bool
testExportRenderValidImports = do
  let props =
        [ StoredProperty
            { spExpression = "\\(x :: Expr) -> simplify (simplify x) == simplify x"
            , spModule     = Just "test/Gen.hs"
            , spPassed     = 1
            , spUpdated    = 0
            , spCases     = 0
            }
        ]
      rendered = QcExport.renderTestFile props
  pure $ T.isInfixOf "import Gen"         rendered
      && not (T.isInfixOf "import test."  rendered)
      && not (T.isInfixOf "import test_"  rendered)

-- | Issue #40: when properties are persisted with @spModule =
-- "test/Spec.hs"@ — exactly the path the export writes — the
-- legacy renderer emitted a self-referential @import Spec@ in a
-- file that uses @module Main where@. The new renderer takes the
-- output's module-name hint and filters it out of the import set.
testExportRenderDropsSelfImport :: IO Bool
testExportRenderDropsSelfImport = do
  let props =
        [ StoredProperty
            { spExpression = "\\x -> x == (x :: Int)"
            , spModule     = Just "test/Spec.hs"
            , spPassed     = 1
            , spUpdated    = 0
            , spCases     = 0
            }
        ]
      -- The output will live at @test/Spec.hs@ → module hint "Spec".
      rendered = QcExport.renderTestFileWith (Just "Spec") [] props
  pure $ not (T.isInfixOf "import Spec" rendered)
      && T.isInfixOf "module Main where" rendered

-- | Issue #40 — properties authored at test scope reference
-- library identifiers ('simplify', 'eval', …) but their
-- 'spModule' carries no library-module trail. The renderer must
-- pick up the slack by importing every @exposed-modules:@ entry
-- from the project's library stanza so the emitted file compiles
-- standalone.
testExportRenderUnionsLibMods :: IO Bool
testExportRenderUnionsLibMods = do
  let props =
        [ StoredProperty
            { spExpression = "\\e -> eval emptyEnv (simplify e) == eval emptyEnv e"
            , spModule     = Just "test/Spec.hs"
            , spPassed     = 1
            , spUpdated    = 0
            , spCases     = 0
            }
        ]
      libMods = ["Expr.Syntax", "Expr.Simplify", "Expr.Eval"]
      rendered = QcExport.renderTestFileWith (Just "Spec") libMods props
  pure $ T.isInfixOf "import Expr.Syntax"   rendered
      && T.isInfixOf "import Expr.Simplify" rendered
      && T.isInfixOf "import Expr.Eval"     rendered
      && not (T.isInfixOf "import Spec" rendered)

-- | Issue #40: a library module that ALSO appears as a
-- property's @spModule@ must not be imported twice. The renderer
-- dedupes after sorting, so 'nub' on a sorted list does the job.
testExportRenderDedupesLibAndProps :: IO Bool
testExportRenderDedupesLibAndProps = do
  let props =
        [ StoredProperty
            { spExpression = "\\x -> simplify x == simplify (simplify x)"
            , spModule     = Just "src/Expr/Simplify.hs"
            , spPassed     = 1
            , spUpdated    = 0
            , spCases     = 0
            }
        ]
      libMods  = ["Expr.Simplify"]  -- already covered by spModule
      rendered = QcExport.renderTestFileWith Nothing libMods props
      occurrences = T.count "import Expr.Simplify" rendered
  pure (occurrences == 1)

--------------------------------------------------------------------------------
-- #131 — export guard: prevents overwriting hand-written Spec.hs
--------------------------------------------------------------------------------

-- | exportGuard returns Nothing (proceed) when the file does not exist.
testExportGuardNewFile :: IO Bool
testExportGuardNewFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-new-Spec.hs"
  removePathForcibly path
  result <- QcExport.exportGuard path False
  pure (isNothing result)

-- | exportGuard returns Nothing when the file starts with the generated header.
testExportGuardGeneratedFile :: IO Bool
testExportGuardGeneratedFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-gen-Spec.hs"
  TIO.writeFile path (QcExport.generatedHeader <> "\nmodule Main where\n")
  result <- QcExport.exportGuard path False
  pure (isNothing result)

-- | exportGuard returns Nothing when the file starts with the scaffold header
-- (produced by ghc_project(action="create")). Both headers are MCP-generated;
-- the scaffold stub is intentionally replaced by a real property suite.
testExportGuardScaffoldFile :: IO Bool
testExportGuardScaffoldFile = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-scaffold-Spec.hs"
  TIO.writeFile path (QcExport.scaffoldTestFileHeader <> "\nmodule Main where\n")
  result <- QcExport.exportGuard path False
  pure (isNothing result)

-- | exportGuard returns Just (refusal) when the file is hand-written (no header).
testExportGuardHandWritten :: IO Bool
testExportGuardHandWritten = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-handwritten-Spec.hs"
  TIO.writeFile path "-- Hand-written test suite\nmodule Main where\n"
  result <- QcExport.exportGuard path False
  pure (isJust result)

-- | exportGuard returns Nothing (proceed) when force=True, even for hand-written.
testExportGuardForce :: IO Bool
testExportGuardForce = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hf-guard-force-Spec.hs"
  TIO.writeFile path "-- Hand-written test suite\nmodule Main where\n"
  result <- QcExport.exportGuard path True
  pure (isNothing result)

-- | Integration: export handle refuses to overwrite a hand-written file.
testExportHandleRefusesHandWritten :: IO Bool
testExportHandleRefusesHandWritten =
  withTempProject $ \pd -> do
    store <- openStore pd
    let specDir  = unProjectDirRaw pd </> "test"
        specPath = specDir </> "Spec.hs"
    createDirectoryIfMissing True specDir
    TIO.writeFile specPath "-- Hand-written test suite\nmodule Main where\n"
    let args = object [ "action" .= ("export" :: T.Text) ]
    result <- QcExport.handle store pd args
    -- Encode the whole ToolResponse as JSON and search the text for the guard strings.
    let body = TL.toStrict (TLE.decodeUtf8 (A.encode result))
    pure (  "hand_written_file_guard" `T.isInfixOf` body
         || "Refusing to overwrite"   `T.isInfixOf` body)

--------------------------------------------------------------------------------
-- BUG-04 — PropertyStore cold-start resilience
--------------------------------------------------------------------------------

-- | BUG-04 core: a fresh project whose @.haskell-flows/@ dir
-- does not yet exist must still accept a first @save@. The fix
-- re-asserts @createDirectoryIfMissing True@ before every write.
testPropStoreCreatesDir :: IO Bool
testPropStoreCreatesDir = withTempProject $ \pd -> do
  -- Do NOT call 'openStore' upfront. Simulate the pathological
  -- case where the dir was cleaned between boot and the first
  -- save: mkdir removed, then save issued.
  removePathForcibly (unProjectDirRaw pd </> ".haskell-flows")
  store <- openStore pd
  removePathForcibly (unProjectDirRaw pd </> ".haskell-flows")
  save store "\\x -> x == (x :: Int)" (Just "src/Foo.hs")
  props <- loadAll store
  pure (length props == 1)

-- | BUG-04 defence-in-depth: an external @rm -rf .haskell-flows/@
-- between two saves must not leave the store in an unrecoverable
-- state — the second save recreates the dir and persists.
testPropStoreResurrectsDir :: IO Bool
testPropStoreResurrectsDir = withTempProject $ \pd -> do
  store <- openStore pd
  save store "\\x -> x == (x :: Int)" (Just "src/Foo.hs")
  -- Nuke the dir the way a user might via rm -rf.
  removePathForcibly (unProjectDirRaw pd </> ".haskell-flows")
  save store "\\x -> x + 0 == (x :: Int)" (Just "src/Foo.hs")
  props <- loadAll store
  -- After nuke + save, at least the 2nd property must be present.
  pure (any ((== "\\x -> x + 0 == (x :: Int)") . spExpression) props)

-- | BUG-04 companion: parallel saves must not race into an
-- inconsistent JSON. 10 concurrent saves → 10 distinct entries,
-- no truncation, no last-writer-wins.
testPropStoreConcurrentSaves :: IO Bool
testPropStoreConcurrentSaves = withTempProject $ \pd -> do
  store <- openStore pd
  let exprs = [ "\\x -> x + " <> T.pack (show i) <> " >= (x :: Int)"
              | i <- [1 .. 10 :: Int] ]
  mvs <- mapM (\e -> do
                 mv <- newEmptyMVar
                 _  <- forkIO (save store e (Just "src/X.hs")
                                 >> putMVar mv ())
                 pure mv) exprs
  -- Inner budget: if any single save hangs on the property-store
  -- lock (e.g. disk full, FS ACL weirdness under CI), we fail fast
  -- rather than waiting 60s for the outer 'test' timeout.
  m <- timeout 10_000_000 (mapM_ takeMVar mvs)
  case m of
    Nothing -> pure False
    Just () -> do
      props <- loadAll store
      pure (length props == 10)
