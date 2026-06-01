-- | Unit tests for regression classification/summarisation and
-- integration tests for evalIOString, queryExprType, loadForTarget,
-- hole-diagnostic capture, and load-after-deps-add.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.RegressionUnit
  ( testRegressionClassifyScope
  , testRegressionClassifyMissing
  , testRegressionClassifyPassedPassthrough
  , testRegressionClassifyQuiet
  , testRegressionSummariseCap
  , testEvalIOString
  , testQueryExprTypeIdAfterAutoLoad
  , testLoadForTargetLibrary
  , testHoleDiagnosticCapture
  , testLoadAfterDepsAdd
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly, doesFileExist)
import System.FilePath ((</>))
import GHC (InteractiveImport (IIDecl), TcRnExprMode (TM_Inst), exprType, mkModuleName, setContext, simpleImportDecl)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , evalIOString
  , killGhcSession
  , startGhcSession
  , withGhcSession
  )
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.Load as LoadTool
import qualified HaskellFlows.Tool.Regression as RegTool

import Spec.Helpers (withTempProject)

import Control.Exception (SomeException, try)
import Data.Maybe (isJust, isNothing)
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified HaskellFlows.Ghc.ApiSession as ApiSession
import qualified HaskellFlows.Tool.Type as TypeTool
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..))
import HaskellFlows.Parser.Hole (parseTypedHoles, TypedHole (..))
import Control.Monad (when)
import HaskellFlows.Ghc.CabalBootstrap (Target (..))
import HaskellFlows.Parser.Error (GhcError (..), renderGhciStyle)

-- | Issue #51: a stored property whose recorded module is no
-- longer in scope (e.g. @ghc_quickcheck_export@ overwrote
-- @test/Spec.hs@) used to be reported as a regression with
-- @raw: ""@. The classifier now sees the empty parsed result
-- + @"Variable not in scope"@ stderr and tags it as
-- 'load_failed'.
testRegressionClassifyScope :: IO Bool
testRegressionClassifyScope =
  let parsed   = QcUnparsed "\\x -> simplify x" ""
      stderr_  = "test/Spec.hs:7:1: Variable not in scope: simplify"
      result   = RegTool.classifyLoadFailure parsed stderr_
  in pure $ case result of
       Just msg -> "Variable not in scope" `T.isInfixOf` msg
       Nothing  -> False

-- | Issue #51: GHC's @Could not find module@ error is the other
-- common load-failure shape (e.g. when @cabal v2-repl@ rebuilt
-- after a @ghc_remove_modules@). It must also map to
-- 'load_failed', not to a regression.
testRegressionClassifyMissing :: IO Bool
testRegressionClassifyMissing =
  let parsed  = QcUnparsed "\\x -> True" ""
      stderr_ = "test/Spec.hs:7:1: error [GHC-87110] Could not find module 'Spec'"
      result  = RegTool.classifyLoadFailure parsed stderr_
  in pure (isJust result)

-- | Issue #51 — false-positive guard: a property that genuinely
-- failed at runtime (parser produced a non-Unparsed result) must
-- not be re-classified as load_failed even if some incidental
-- stderr was captured.
testRegressionClassifyPassedPassthrough :: IO Bool
testRegressionClassifyPassedPassthrough =
  let parsed   = QcPassed "\\x -> True" 200
      stderr_  = "Variable not in scope: foo"  -- noise, not load failure
      result   = RegTool.classifyLoadFailure parsed stderr_
  in pure (isNothing result)

-- | Issue #51: an unparsed result with NO load-failure marker in
-- stderr (e.g. a property that printed unrecognised text) must
-- stay unparsed — promotion to load_failed requires evidence.
testRegressionClassifyQuiet :: IO Bool
testRegressionClassifyQuiet =
  let parsed   = QcUnparsed "\\x -> True" ""
      stderr_  = "" -- nothing actionable
      result   = RegTool.classifyLoadFailure parsed stderr_
  in pure (isNothing result)

-- | Issue #51: cabal-repl can dump several KB of build-plan
-- noise on a load failure; the response payload caps it at
-- 600 chars + a truncation marker so the JSON-RPC line stays
-- manageable.
testRegressionSummariseCap :: IO Bool
testRegressionSummariseCap =
  let huge = T.replicate 2000 "x"
      out  = RegTool.summariseLoadError huge
  in pure (T.length out <= 700  -- 600 + truncation marker
       && "(truncated)" `T.isInfixOf` out)

-- | Phase-7 foundation: can we compile + run an 'IO String' action
-- in-process and read its result back? This is the primitive QC /
-- regression / determinism / IO-eval migrations depend on. If this
-- test ever regresses, those migrations are off the table until the
-- GHC API boundary changes.
testEvalIOString :: IO Bool
testEvalIOString = case mkProjectDir "/tmp" of
  Left _   -> pure False
  Right pd -> do
    sess   <- startGhcSession pd
    result <- withGhcSession sess $ do
      setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
      evalIOString "(return \"hello-from-ghc-api\") :: IO String"
    killGhcSession sess
    pure (result == "hello-from-ghc-api")

-- | Issue #80 regression anchor: the most fundamental query the
-- MCP exposes — ghc_type "id" — must always succeed regardless of
-- whether autoLoadProject ran. We stage a tiny project (no
-- .cabal — autoLoadProject path, no stanza flags) with one
-- module that imports Prelude only, start a GhcSession, run
-- 'queryExprType "id"' inside 'withGhcSession' (which auto-loads
-- the project on first use), and assert the renderer returns a
-- polymorphic identity signature.
--
-- Pre-fix this could (in some sessions) return a hidden-package
-- cascade because the interactive context's auto-imported set
-- referenced symbols not exposed under base-only DynFlags.
-- Codifies the working state so any future regression that
-- swaps the IC handling re-surfaces in the unit suite, before
-- the e2e harness catches it.
testQueryExprTypeIdAfterAutoLoad :: IO Bool
testQueryExprTypeIdAfterAutoLoad = do
  tmp <- getTemporaryDirectory
  let dir  = tmp </> "haskell-flows-issue-80"
      file = dir </> "src" </> "Foo.hs"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile file
    (T.pack "module Foo where\nimport Prelude\nfoo :: Int\nfoo = 1\n")
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      eRes <- try @SomeException $
        withGhcSession sess (TypeTool.queryExprType "id")
      killGhcSession sess
      removePathForcibly dir
      pure $ case eRes of
        Right t -> "a -> a" `T.isInfixOf` t
        Left _  -> False

-- | Wave-2 gate: 'loadForTarget' against /tmp/bench-project library
-- must compile Foo.hs cleanly (success=True, no errors). If the
-- fixture dir is missing, skip gracefully.
testLoadForTargetLibrary :: IO Bool
testLoadForTargetLibrary = case mkProjectDir "/tmp/bench-project" of
  Left _   -> pure True
  Right pd -> do
    exists <- doesFileExist "/tmp/bench-project/bench-project.cabal"
    if not exists
      then pure True
      else do
        sess <- startGhcSession pd
        (ok, diags) <- ApiSession.loadForTarget sess TargetLibrary ApiSession.Strict
        killGhcSession sess
        pure (ok && null diags)

-- | Diagnostic: prove whether 'loadForTarget' with 'Deferred' flavour
-- captures typed-hole warnings through the logger hook. Writes a
-- detailed trace to @/tmp/hole-hook-diag.log@ for inspection. If the
-- fixture dir or the @Hole.hs@ fixture is missing, skip gracefully.
testHoleDiagnosticCapture :: IO Bool
testHoleDiagnosticCapture = case mkProjectDir "/tmp/hole-fixture" of
  Left _   -> pure True
  Right pd -> do
    cabalExists <- doesFileExist "/tmp/hole-fixture/hole-fixture.cabal"
    holeExists  <- doesFileExist "/tmp/hole-fixture/src/Hole.hs"
    if not (cabalExists && holeExists)
      then pure True  -- no fixture, skip
      else do
        sess <- startGhcSession pd
        (_ok, diags) <- ApiSession.loadForTarget sess TargetLibrary ApiSession.Deferred
        killGhcSession sess
        -- Full Wave-2 hole pipeline: capture -> render -> parse.
        -- A non-empty holes list with the expected file proves the
        -- hook captured the warning, the renderer produced a valid
        -- GHCi-style header, and parseTypedHoles extracted the hole.
        let rendered = renderGhciStyle diags
            holes    = parseTypedHoles rendered
        pure $ not (null holes)
             && any (("Hole.hs" `T.isSuffixOf`) . thFile) holes

-- | Regression for the FlowArbitrary e2e failure: after
-- 'invalidateStanzaFlags' (which the server fires after every
-- 'ghc_deps add'), the NEXT 'loadForTarget' must re-bootstrap
-- cabal flags AND successfully compile a module that references
-- the newly-added dependency. Before the fix, the captured argv
-- still held a stale @-hide-all-packages@ AFTER the
-- @-package-id@ tokens, which under GHC-API flag-parsing resets
-- the visible-package set and surfaces as
-- @cannot satisfy -package-id QckChck-...@.
testLoadAfterDepsAdd :: IO Bool
testLoadAfterDepsAdd = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("arb-repro-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  let pdE = mkProjectDir dir
  case pdE of
    Left _   -> do removePathForcibly dir; pure False
    Right pd -> do
      let srcDir = dir </> "src"
      createDirectoryIfMissing True srcDir
      -- 1. Scaffold with base only (no QuickCheck yet).
      TIO.writeFile (dir </> "arb-repro.cabal") $ T.unlines
        [ "cabal-version: 2.4"
        , "name: arb-repro"
        , "version: 0.1.0.0"
        , ""
        , "library"
        , "  hs-source-dirs:   src"
        , "  exposed-modules:  Shapes, ShapesGen"
        , "  build-depends:    base"
        , "  default-language: Haskell2010"
        ]
      TIO.writeFile (dir </> "cabal.project") "packages: .\n"
      TIO.writeFile (srcDir </> "Shapes.hs") $ T.unlines
        [ "{-# LANGUAGE DerivingStrategies #-}"
        , "module Shapes (Status (..)) where"
        , "data Status = Ok | Err String deriving stock (Eq, Show)"
        ]
      TIO.writeFile (srcDir </> "ShapesGen.hs") $ T.unlines
        [ "{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}"
        , "module ShapesGen () where"
        , "import Shapes"
        , "import Test.QuickCheck"
        , "instance Arbitrary Status where"
        , "  arbitrary = oneof [ pure Ok, Err <$> arbitrary ]"
        ]
      sess <- startGhcSession pd
      -- 2. Mutate .cabal to add QuickCheck (simulates ghc_deps add).
      TIO.writeFile (dir </> "arb-repro.cabal") $ T.unlines
        [ "cabal-version: 2.4"
        , "name: arb-repro"
        , "version: 0.1.0.0"
        , ""
        , "library"
        , "  hs-source-dirs:   src"
        , "  exposed-modules:  Shapes, ShapesGen"
        , "  build-depends:    base, QuickCheck"
        , "  default-language: Haskell2010"
        ]
      ApiSession.invalidateStanzaFlags sess
      -- 3. Load via the full in-process path.
      (ok, diags) <- ApiSession.loadForTarget sess TargetLibrary ApiSession.Strict
      killGhcSession sess
      let satisfy =
            any (T.isInfixOf "cannot satisfy -package-id" . geMessage) diags
      when (not ok || satisfy) $ do
        putStrLn "  -- testLoadAfterDepsAdd diagnostics --"
        mapM_ (putStrLn . ("    " <>) . T.unpack . geMessage) diags
      removePathForcibly dir
      pure (ok && not satisfy)
