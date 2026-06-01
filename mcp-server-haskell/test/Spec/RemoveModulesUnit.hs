-- | Unit tests for 'Tool.RemoveModules' cabal-text manipulation,
-- 'Tool.Gate' error-capture, and the nextStep hint for remove_modules.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.RemoveModulesUnit
  ( testRemoveModulesRegistered
  , testRemoveModulesStripsCabal
  , testRemoveModulesIdempotent
  , testRemoveModulesPreservesFields
  , testRemoveModulesOtherModules
  , testRemoveModulesOtherModulesIdempotent
  , testRemoveModulesBothSections
  , testRemoveModulesNotFoundField
  , testGateRunStepCatchesExceptions
  , testGateCabalStepBracket
  , testNextStepRemoveModules
  ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Control.Exception (SomeException, try)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Server (allToolNameTexts)
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.RemoveModules as RM
import HaskellFlows.Mcp.Progress (noopSink)
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (withTempProject, decodeToolResult)

testRemoveModulesRegistered :: IO Bool
testRemoveModulesRegistered = pure $
  "ghc_modules" `elem` allToolNameTexts

-- | Core behaviour: the exposed-modules entry for the named
-- module disappears; the rest of the block survives.
testRemoveModulesStripsCabal :: IO Bool
testRemoveModulesStripsCabal =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Expr.Syntax"
        , "                    Expr.Old"
        , "                    Expr.Eval"
        , "  build-depends:    base"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Expr.Old"]
  in pure $ removed == ["Expr.Old"]
         && T.isInfixOf "Expr.Syntax" newCabal
         && T.isInfixOf "Expr.Eval"   newCabal
         && not ("Expr.Old" `T.isInfixOf` newCabal)

-- | Removing a module that is not registered is a silent no-op:
-- no write, empty removed-list, body unchanged.
testRemoveModulesIdempotent :: IO Bool
testRemoveModulesIdempotent =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Expr.Syntax"
        , "  build-depends:    base"
        ]
      (newCabal, removed) =
        RM.removeModulesFromBody cabal ["Expr.NeverExisted"]
  in pure (null removed && newCabal == cabal)

-- | Removing must not disturb other fields (build-depends,
-- test-suite stanza, etc). Full-file regression guard.
testRemoveModulesPreservesFields :: IO Bool
testRemoveModulesPreservesFields =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Keep.This"
        , "                    Drop.This"
        , "  build-depends:    base"
        , ""
        , "test-suite expr-test"
        , "  main-is:    Spec.hs"
        , "  build-depends: base, QuickCheck"
        ]
      (newCabal, _) = RM.removeModulesFromBody cabal ["Drop.This"]
  in pure $ T.isInfixOf "library"                newCabal
         && T.isInfixOf "Keep.This"              newCabal
         && T.isInfixOf "build-depends:    base" newCabal
         && T.isInfixOf "test-suite expr-test"   newCabal
         && T.isInfixOf "QuickCheck"             newCabal

-- | #157: modules listed under @other-modules:@ are removed just
-- like @exposed-modules:@ entries. Before the fix only
-- @exposed-modules@ was scanned, so 'other-modules' entries
-- silently fell through with an empty 'cabal_removed'.
testRemoveModulesOtherModules :: IO Bool
testRemoveModulesOtherModules =
  let cabal = T.unlines
        [ "library"
        , "  other-modules:  Internal.Helper"
        , "                  Internal.Drop"
        , "  build-depends:  base"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Internal.Drop"]
  in pure $ removed == ["Internal.Drop"]
         && T.isInfixOf "Internal.Helper" newCabal
         && not ("Internal.Drop" `T.isInfixOf` newCabal)

-- | #157: removing a module that does not appear in @other-modules@
-- is still a no-op (same idempotency guarantee as exposed-modules).
testRemoveModulesOtherModulesIdempotent :: IO Bool
testRemoveModulesOtherModulesIdempotent =
  let cabal = T.unlines
        [ "library"
        , "  other-modules:  Internal.Helper"
        , "  build-depends:  base"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Internal.NeverExisted"]
  in pure (null removed && newCabal == cabal)

-- | #157: when a module appears in @exposed-modules@ of one stanza
-- and @other-modules@ of another, both occurrences are removed and
-- both names appear in the returned list.
testRemoveModulesBothSections :: IO Bool
testRemoveModulesBothSections =
  let cabal = T.unlines
        [ "library"
        , "  exposed-modules:  Shared.Core"
        , "                    Shared.Util"
        , "  build-depends:    base"
        , ""
        , "test-suite pkg-test"
        , "  other-modules:  Shared.Util"
        , "  build-depends:  base, QuickCheck"
        ]
      (newCabal, removed) = RM.removeModulesFromBody cabal ["Shared.Util"]
  in pure $ "Shared.Util" `elem` removed
         && T.isInfixOf "Shared.Core"     newCabal
         && not ("Shared.Util" `T.isInfixOf` newCabal)

-- | Issue #248: 'ghc_modules remove' must surface a 'not_found' list
-- when a requested module does not appear in any cabal section,
-- rather than silently returning an empty 'cabal_removed' list.
testRemoveModulesNotFoundField :: IO Bool
testRemoveModulesNotFoundField = do
  tmp <- getTemporaryDirectory
  let dir      = tmp </> "haskell-flows-rm-notfound"
      cabalFile = dir </> "pkg.cabal"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  TIO.writeFile cabalFile $ T.unlines
    [ "cabal-version: 3.0"
    , "name:          pkg"
    , "version:       0.1.0.0"
    , "library"
    , "  exposed-modules:  Existing.Module"
    , "  build-depends:    base"
    , "  default-language: Haskell2010"
    ]
  result <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      let args = A.object
            [ "modules"      A..= (["Never.Existed"] :: [Text])
            , "delete_files" A..= False
            ]
      tr <- RM.handle pd args
      pure $ case decodeToolResult tr of
        Right env ->
             Env.reStatus env == Env.StatusOk
          && case Env.reResult env of
               Just (A.Object obj) ->
                 case AKM.lookup (AKey.fromText "not_found") obj of
                   Just (A.Array arr) ->
                     A.String "Never.Existed" `elem` arr
                   _                  -> False
               _ -> False
        Left _ -> False
  removePathForcibly dir
  pure result

-- | BUG-01 — static source check that 'runStep' catches
-- synchronous exceptions from a step's body. If someone removes
-- the 'try' wrap, a step that throws would escape runStep,
-- propagate past runTool's outer try as a connection close, and
-- reproduce F-22 (the dogfood crash that killed the MCP
-- mid-session).
testGateRunStepCatchesExceptions :: IO Bool
testGateRunStepCatchesExceptions = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Gate.hs"
  pure $ T.isInfixOf "timeout budget (try body)"  src
      && T.isInfixOf "Left (e :: SomeException)" src
      && T.isInfixOf "\"exception\" .= T.pack (show e)" src

-- | BUG-01 + Issue #75 — 'cabalStep' must drain stdout AND
-- stderr without deadlocking on a full pipe buffer.
--
-- The pre-#75 implementation forked two threads doing
-- @hGetContents h >>= putMVar v@. Because @hGetContents@ is
-- lazy, the forks deposited thunks into the MVars without
-- actually draining the OS pipes. When cabal wrote more than
-- ~64 KiB (a noisy build error, a -Wall storm), the writer
-- blocked, @waitForProcess@ blocked, and the whole gate hung
-- past its 5-minute timeout in a way that corrupted the MCP
-- transport — the agent saw \"Connection closed\" instead of a
-- structured TimedOut step.
--
-- The fix delegates to @readCreateProcessWithExitCode@, which
-- uses strict bytestring drains for both pipes internally. This
-- test pins the new invariant: the manual fork-and-MVar pattern
-- is gone, replaced by the canonical helper.
testGateCabalStepBracket :: IO Bool
testGateCabalStepBracket = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Gate.hs"
  pure $ T.isInfixOf "readCreateProcessWithExitCode" src
      && not (T.isInfixOf "forkIO (hGetContents" src)
      && not (T.isInfixOf "(_, Just hOut, Just hErr, ph) <- createProcess" src)

-- | #94 Phase B nextStep coverage: 'ghc_modules' on a remove-shaped
-- success suggests the project-wide check + reload chain so any
-- dangling import surfaces immediately.  Both add and remove route
-- through the same chain (the post-condition is the same).
testNextStepRemoveModules :: IO Bool
testNextStepRemoveModules =
  let payload = A.object
        [ "success"      .= True
        , "cabal_removed".= (["Foo.Old"] :: [Text])
        ]
  in case suggestNext GhcModules True payload of
       Just ns ->
         pure $ nsTool ns == GhcCheckProject
             && case nsChain ns of
                  Just steps ->
                       any ((== GhcCheckProject) . csTool) steps
                    && any ((== GhcLoad)         . csTool) steps
                  Nothing -> False
       Nothing -> pure False
