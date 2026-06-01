-- | Unit tests for 'Tool.ValidateCabal' (#90 Phase B): envelope shape,
-- warning/error classification, and backwards-compat issues array.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.ValidateCabal
  ( testValidateCabalClean
  , testValidateCabalWarnings
  , testValidateCabalErrors
  , testValidateCabalBackcompatIssues
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.ValidateCabal as ValidateCabalTool

import Spec.Helpers (decodeToolResult, runToolEnvelope)

runValidateCabalIn :: Text -> IO (Either String Env.ToolResponse)
runValidateCabalIn cabalBody = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-validate-test"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> "test-pkg.cabal") cabalBody
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir for tmp")
    Right pd -> do
      tr <- ValidateCabalTool.handle pd (A.object [])
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | Cabal fixture with no obvious cabal-check warnings or errors.
-- Whatever cabal-check actually says depends on the cabal/ghc
-- version installed — we don't pin a status here, only the
-- *contract* (status reflects errors / warnings counts faithfully).
minimalCabalBody :: Text
minimalCabalBody = T.unlines
  [ "cabal-version:      3.0"
  , "name:               test-pkg"
  , "version:            0.1.0.0"
  , "synopsis:           a test fixture for #90 phase B validation"
  , "description:        a longer description that exceeds the synopsis "
    <> "in length so cabal-check does not warn about it being shorter."
  , "category:           Testing"
  , "license:            BSD-3-Clause"
  , "author:             test"
  , "maintainer:         test@example.com"
  , "build-type:         Simple"
  , ""
  , "library"
  , "    default-language: GHC2024"
  , "    build-depends:    base >= 4.20 && < 5"
  ]

-- | Cabal fixture that intentionally triggers the duplicate-dep
-- heuristic. The dup is *guaranteed* to be a warning regardless of
-- cabal version. cabal-check may add more — we don't pin specifics.
duplicateDepCabalBody :: Text
duplicateDepCabalBody = T.unlines
  [ "cabal-version:      3.0"
  , "name:               test-pkg"
  , "version:            0.1.0.0"
  , "synopsis:           dup-dep fixture for #90 phase B validation"
  , "description:        a longer description that exceeds the synopsis "
    <> "in length so cabal-check does not warn about it being shorter."
  , "category:           Testing"
  , "license:            BSD-3-Clause"
  , "author:             test"
  , "maintainer:         test@example.com"
  , "build-type:         Simple"
  , ""
  , "library"
  , "    default-language: GHC2024"
  , "    build-depends:    base >= 4.20 && < 5, base"  -- intentional duplicate
  ]

-- | Phase B contract: when 'cabal check' returns no errors, the
-- envelope status is 'ok' (no warnings) or 'partial' (warnings only).
-- Status MUST NOT be 'failed' if there are no errors. Anchors the
-- (errors == 0) ⇒ (status ∈ {ok, partial}) implication.
testValidateCabalClean :: IO Bool
testValidateCabalClean = do
  decoded <- runValidateCabalIn minimalCabalBody
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        case AKM.lookup (AKey.fromText "errors") payload of
          Just (A.Number 0) ->
            -- 0 errors ⇒ status='ok' (#119: 'partial' was retired for
            -- warnings-only results; all 0-error outcomes are now 'ok')
            Env.reStatus env == Env.StatusOk
          Just (A.Number _) ->
            -- non-zero errors ⇒ status='failed' (the other branch)
            Env.reStatus env == Env.StatusFailed
          _ -> False
      _ -> False
    Left _ -> False

-- | Phase B + #119 contract: a cabal fixture with the duplicate-dep
-- heuristic warning *plus* zero cabal-check errors produces
-- status='ok' (not 'partial' — #119 fix) with envelope-warnings
-- populated. If cabal-check happens to also raise errors on this
-- fixture, status shifts to 'failed' — accept either, but assert
-- the structured 'warnings' array is populated whenever issues exist.
testValidateCabalWarnings :: IO Bool
testValidateCabalWarnings = do
  decoded <- runValidateCabalIn duplicateDepCabalBody
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        case ( AKM.lookup (AKey.fromText "errors")   payload
             , AKM.lookup (AKey.fromText "warnings") payload
             ) of
          (Just (A.Number 0), Just (A.Number w))
            | w > 0 ->
                -- #119: warnings-only → status='ok', not 'partial'.
                Env.reStatus env == Env.StatusOk
                  && not (null (Env.reWarnings env))
            | otherwise -> Env.reStatus env == Env.StatusOk
          (Just (A.Number e), _)
            | e > 0 -> Env.reStatus env == Env.StatusFailed
          _ -> False
      _ -> False
    Left _ -> False

-- | Phase B: a project dir with no .cabal file at all produces
-- status='failed' with the envelope's
-- 'error.kind=module_path_does_not_exist'. The earlier code path
-- emitted 'success: false' with a free-form error string;
-- post-Phase-B the error is structured.
testValidateCabalErrors :: IO Bool
testValidateCabalErrors = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-validate-no-cabal"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  -- No .cabal file in dir.
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir for tmp")
    Right pd -> do
      tr <- ValidateCabalTool.handle pd (A.object [])
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure $ case result of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.ModulePathDoesNotExist
    _ -> False

-- | Phase B back-compat: the legacy 'issues' array must continue
-- to live under 'result' so any consumer keying on it during the
-- migration window keeps working. Since the failed-status path
-- carries the structured info inside 'error' instead of 'result',
-- we drive the success/partial path (a clean cabal) here so 'result'
-- is guaranteed to exist.
testValidateCabalBackcompatIssues :: IO Bool
testValidateCabalBackcompatIssues = do
  decoded <- runValidateCabalIn duplicateDepCabalBody
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        case AKM.lookup (AKey.fromText "issues") payload of
          Just (A.Array _) -> True
          _                -> False
      Nothing ->
        -- failed path is also acceptable for this test if
        -- cabal-check raised errors — the contract is just that
        -- a successful path keeps the issues array
        Env.reStatus env == Env.StatusFailed
      _ -> False
    Left _ -> False
