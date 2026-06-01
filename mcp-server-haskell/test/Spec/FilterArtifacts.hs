-- | Unit tests for artifact-filter helpers, mtime invalidation, AddModules
-- JSON-array/string parsing, and CheckModule warnings-block flag.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.FilterArtifacts
  ( testFilterArtifactsDropsWithPeer
  , testFilterArtifactsKeepsLone
  , testFilterArtifactsEmpty
  , testMtimeInvalidation
  , testAddModulesJsonArrayString
  , testAddModulesPlainStringStillWorks
  , testCheckModuleWarningsBlockDefault
  , testCheckModuleWarningsBlockFalse
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Tool.AddModules as AddModules
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import qualified HaskellFlows.Tool.Load as LoadTool
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (withTempProject)
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import qualified HaskellFlows.Ghc.ApiSession as ApiSession

-- | Issue #57: when GHC's deferred-pass emits a real diagnostic
-- (typed hole) AND a "GHC-58427 ... is not loaded" follow-up,
-- 'filterArtifacts' must drop the artifact. The agent then sees
-- one error, not two.
testFilterArtifactsDropsWithPeer :: IO Bool
testFilterArtifactsDropsWithPeer =
  let hole = GhcError
        { geFile     = "src/Demo.hs"
        , geLine     = 24
        , geColumn   = 35
        , geSeverity = SevError
        , geCode     = Nothing
        , geMessage  = "[GHC-88464] • Found hole: _holeArg :: [a]"
        }
      artifact = GhcError
        { geFile     = ""
        , geLine     = 0
        , geColumn   = 0
        , geSeverity = SevError
        , geCode     = Nothing
        , geMessage  = "<interactive>:1:1: error: [GHC-58427]\n    \
                       \attempting to use module 'Foo' which is not loaded"
        }
      out = ApiSession.filterArtifacts [hole, artifact]
  in pure (out == [hole])

-- | Issue #57: when the GHC-58427 entry is the ONLY diagnostic,
-- it stays — that case is a real \"module not in graph\"
-- situation and the agent should see the message.
testFilterArtifactsKeepsLone :: IO Bool
testFilterArtifactsKeepsLone =
  let lone = GhcError
        { geFile     = ""
        , geLine     = 0
        , geColumn   = 0
        , geSeverity = SevError
        , geCode     = Nothing
        , geMessage  = "<interactive>:1:1: error: [GHC-58427] not loaded"
        }
      out = ApiSession.filterArtifacts [lone]
  in pure (out == [lone])

-- | Issue #57: empty input is a no-op (no false drops).
testFilterArtifactsEmpty :: IO Bool
testFilterArtifactsEmpty =
  pure (null (ApiSession.filterArtifacts []))

testMtimeInvalidation :: IO Bool
testMtimeInvalidation = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("mtime-inv-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> "demo.cabal") $ T.unlines
    [ "cabal-version: 2.4"
    , "name: demo"
    , "version: 0.1.0.0"
    , ""
    , "library"
    , "  build-depends: base"
    , "  default-language: Haskell2010"
    ]
  TIO.writeFile (dir </> "cabal.project") "packages: .\n"
  case mkProjectDir dir of
    Left _ -> do removePathForcibly dir; pure False
    Right pd -> do
      sess <- startGhcSession pd
      before <- ApiSession.readCabalMtimeForTest sess
      ApiSession.ensureStanzaFlags sess
      afterFirst <- ApiSession.readCabalMtimeForTest sess
      -- macOS fs mtime has 1-sec resolution; sleep past it to
      -- guarantee a strictly-advanced mtime on the next write.
      threadDelay 1_100_000
      TIO.writeFile (dir </> "demo.cabal") $ T.unlines
        [ "cabal-version: 2.4"
        , "name: demo"
        , "version: 0.2.0.0"
        , ""
        , "library"
        , "  build-depends: base, containers"
        , "  default-language: Haskell2010"
        ]
      ApiSession.ensureStanzaFlags sess
      afterTouch <- ApiSession.readCabalMtimeForTest sess
      killGhcSession sess
      removePathForcibly dir
      pure
        ( isNothing before
        && isJust afterFirst
        && isJust afterTouch
        && afterFirst < afterTouch
        )

--------------------------------------------------------------------------------
-- BUG-PLUS-08: add_modules unwraps stringified JSON arrays
--------------------------------------------------------------------------------

-- | The real trap: a client-side wrapper stringifies a JSON
-- array before dispatch, so the server receives
-- @{"modules": "[\"A\", \"B\"]"}@ — a String whose content is a
-- rendered array. Earlier versions comma-split on the outer
-- string and kept the @[@, @]@, @\"@ characters as part of the
-- "module names", creating files like @src/[\"A\".hs@ on disk.
-- Post-fix, 'parseModuleList' recognises the JSON-array shape,
-- unwraps it via 'eitherDecodeStrict', and recurses into the
-- Array branch — the caller observes the same result either way.
testAddModulesJsonArrayString :: IO Bool
testAddModulesJsonArrayString =
  let stringified = A.object
        [ "modules" A..= ("[\"Expr.Syntax\", \"Expr.Eval\"]" :: Text) ]
      quotedNoSpaces = A.object
        [ "modules" A..= ("[\"Expr.Syntax\",\"Expr.Eval\"]" :: Text) ]
      indented = A.object
        [ "modules" A..= ("  [ \"Expr.Syntax\" , \"Expr.Eval\" ] " :: Text) ]
      ok v = case A.fromJSON v of
        A.Success (AddModules.AddModulesArgs xs _) ->
          xs == ["Expr.Syntax", "Expr.Eval"]
        _ -> False
  in pure (ok stringified && ok quotedNoSpaces && ok indented)

-- | The pre-BUG-PLUS-08 fallback must still work for plain
-- strings — @"A, B"@ and @"A B"@ continue to normalise to
-- @[\"A\", \"B\"]@. Guards against the aeson-first path
-- regressing the commonplace case.
testAddModulesPlainStringStillWorks :: IO Bool
testAddModulesPlainStringStillWorks =
  let csv   = A.object [ "modules" A..= ("A, B" :: Text) ]
      ws    = A.object [ "modules" A..= ("A B"  :: Text) ]
      mixed = A.object [ "modules" A..= ("A,\tB\nC" :: Text) ]
      ok v expected = case A.fromJSON v of
        A.Success (AddModules.AddModulesArgs xs _) -> xs == expected
        _ -> False
  in pure (ok csv ["A","B"] && ok ws ["A","B"] && ok mixed ["A","B","C"])

--------------------------------------------------------------------------------
-- BUG-PLUS-mediocre-1: warnings_block flag on ghc_check_module
--------------------------------------------------------------------------------

-- | CheckArgs.warnings_block defaults to True (back-compat
-- with the pre-fix pre-push-gate strictness).
testCheckModuleWarningsBlockDefault :: IO Bool
testCheckModuleWarningsBlockDefault =
  let payload = A.object [ "module_path" A..= ("src/Foo.hs" :: Text) ]
  in case A.fromJSON payload of
       A.Success args -> pure (CheckModule.caWarningsBlock args)
       _              -> pure False

-- | Passing @warnings_block: false@ flips the gate: the field
-- surfaces on the parsed args, and the handler uses it to stop
-- warnings from turning overall into False when compile + holes
-- + properties are green.
testCheckModuleWarningsBlockFalse :: IO Bool
testCheckModuleWarningsBlockFalse =
  let payload = A.object
        [ "module_path"    A..= ("src/Foo.hs" :: Text)
        , "warnings_block" A..= False
        ]
  in case A.fromJSON payload of
       A.Success args -> pure (not (CheckModule.caWarningsBlock args))
       _              -> pure False
