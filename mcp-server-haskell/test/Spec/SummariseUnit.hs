-- | Unit tests for ghc_gate error summarisation and ghc_check_module
-- hole-reason filtering, compile-ok-with-holes, and refactor
-- pre-existing-holes exclusion.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.SummariseUnit
  ( testCheckModuleHolesReasonCount
  , testCheckModuleHoleOnlyCompileOk
  , testCheckModuleRealErrorsExcludesHoles
  , testRefactorPreExistingHolesExcluded
  , testCheckModuleUsesSpecificLoader
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.Gate as Gate
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import qualified HaskellFlows.Tool.Refactor as RefactorTool
import HaskellFlows.Mcp.Progress (noopSink)
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (withTempProject)

--------------------------------------------------------------------------------
-- Issue #135 — summariseMeasurementErrors truncation + dedup
--------------------------------------------------------------------------------

-- | #135: a single short error is presented as "First error (1/1): …"
-- with no omit note and total length well under 600 chars.
testCheckModuleHolesReasonCount :: IO Bool

testCheckModuleHolesReasonCount =
  -- Pass 2 dummy holes (unit values — holes param is polymorphic [a])
  let result = CheckModule.renderResult
                 "src/Scratch.hs"
                 True   -- compileOk
                 []     -- errors
                 []     -- warnings
                 [(), ()] -- 2 holes
                 []     -- regressions
                 0      -- totalProps
                 0      -- loadFailed
                 True   -- warnings_block
  in pure $ case Env.reResult result of
       Just (A.Object r) ->
         case AKM.lookup "gates" r of
           Just (A.Object g) ->
             case AKM.lookup "holes" g of
               Just (A.Object holesGate) ->
                 AKM.lookup "ok" holesGate == Just (A.Bool False)
                 && case AKM.lookup "reason" holesGate of
                      Just (A.String rr) -> "2 typed hole(s)" `T.isInfixOf` rr
                      _                  -> False
               _ -> False
           _ -> False
       _ -> False

testCheckModuleHoleOnlyCompileOk :: IO Bool

testCheckModuleHoleOnlyCompileOk = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckModule.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "isHoleErr" code
      && T.isInfixOf "GHC-88464" code
      && T.isInfixOf "ownHoleOnly" code
      && T.isInfixOf "compileOk = null errors" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | #108: 'errors' must exclude diagnostics where 'geCode == Just "GHC-88464"'.

testCheckModuleRealErrorsExcludesHoles :: IO Bool

testCheckModuleRealErrorsExcludesHoles = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckModule.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  -- Must filter with 'not (isHoleErr d)' or equivalent.
  pure $ T.isInfixOf "not (isHoleErr d)" code
       || T.isInfixOf "not (isHoleErr" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

-- | #108: 'commitResultWithDiff' in Refactor.hs must exclude holes from
-- pre_existing_errors by filtering on 'GHC-88464'.

testRefactorPreExistingHolesExcluded :: IO Bool

testRefactorPreExistingHolesExcluded = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Refactor.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "isHoleErr" code
      && T.isInfixOf "GHC-88464" code
      && T.isInfixOf "not (isHoleErr d)" code
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln

--------------------------------------------------------------------------------
-- Issue #188 — check_module uses loadSpecificFileForTarget
--------------------------------------------------------------------------------

-- | #188: 'ghc_check_module' used to call 'loadForTarget' which scans all
-- .hs files via 'enumerateHaskellSources', causing 'strictOk=False' when
-- any unregistered broken file existed in src/ even if the checked module
-- was clean. Fix: use 'loadSpecificFileForTarget' so only the requested
-- file is compiled.

testCheckModuleUsesSpecificLoader :: IO Bool

testCheckModuleUsesSpecificLoader = do
  src <- TIO.readFile "src/HaskellFlows/Tool/CheckModule.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "loadSpecificFileForTarget" code
      -- loadForTarget must NOT appear as a function call (it can appear
      -- in comments or error message strings, but not as the actual call)
      && not (T.isInfixOf "loadForTarget ghcSess tgt Strict" code)
      && not (T.isInfixOf "loadForTarget ghcSess tgt Deferred" code)
  where
    isDocLine ln = "--" `T.isPrefixOf` T.stripStart ln
