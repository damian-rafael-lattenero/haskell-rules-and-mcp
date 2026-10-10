-- | Cross-tool traversal-guard tests (#100C) and logging audit-path checks.
-- Each verifies that a tool refuses a path-traversal input at the
-- handle level with status=refused kind=path_traversal.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.TraversalGuards
  ( testApplyExportsRejectsTraversal
  , testFixWarningRejectsTraversal
  , testRefactorRejectsTraversal
  , testLoggingRedactionPolicy
  , testLoggingTraceIdGeneration
  , testLoggingAuditPathAbsentByDefault
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.Logging as Logging
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.ApplyExports as ApplyExports
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Tool.Refactor as RefactorTool

import Spec.Helpers (isTraversalRefused)
import Spec.ToolEnvFixture (pdEnv)

import Data.Text (Text)
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM

import System.Environment (unsetEnv)
import Data.Maybe (isNothing)

-- | Assert that the response is status='refused', kind='path_traversal'.

-- | #100C: 'ghc_apply_exports' must refuse traversal paths.
-- No GhcSession needed — 'mkModulePath' guard fires before any filesystem access.
testApplyExportsRejectsTraversal :: IO Bool

testApplyExportsRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text)
            , "exports"     A..= ([] :: [Text])
            ]
      tr <- ApplyExports.handle (pdEnv pd) args
      pure (isTraversalRefused (Right tr))

-- | #100C: 'ghc_fix_warning' must refuse traversal paths.

testFixWarningRejectsTraversal :: IO Bool

testFixWarningRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text)
            , "line"        A..= (1 :: Int)
            , "code"        A..= ("-Wunused-imports" :: Text)
            ]
      tr <- FixWarning.handle (pdEnv pd) args
      pure (isTraversalRefused (Right tr))

-- | #100C: 'ghc_check_module' must refuse traversal paths.
-- 'mkModulePath' fires before the GhcSession or Store are touched.



testRefactorRejectsTraversal :: IO Bool

testRefactorRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "action"           A..= ("rename_local" :: Text)
            , "module_path"      A..= ("../../etc/passwd" :: Text)
            , "old_name"         A..= ("foo" :: Text)
            , "new_name"         A..= ("bar" :: Text)
            , "scope_line_start" A..= (1 :: Int)
            , "scope_line_end"   A..= (10 :: Int)
            ]
      tr <- RefactorTool.runHandle undefined pd undefined args
      pure (isTraversalRefused (Right tr))

-- ---------------------------------------------------------------------------
-- Issue #98 Phase B · structured logging unit tests
-- ---------------------------------------------------------------------------

-- | 'redactArgs' must truncate string values longer than 'maxArgStringLen'
-- (40 chars) to exactly 40 chars + the Unicode ellipsis "…", while leaving
-- short strings, numbers, and bools verbatim.

testLoggingRedactionPolicy :: IO Bool

testLoggingRedactionPolicy = do
  let longStr  = T.replicate 50 "x"   -- 50 chars, will be truncated
      shortStr = T.replicate 20 "y"   -- 20 chars, kept verbatim
      args = A.object
        [ "long"  A..= longStr
        , "short" A..= shortStr
        , "num"   A..= (42 :: Int)
        , "flag"  A..= True
        ]
  case Logging.redactArgs args of
    A.Object km ->
      let longOk  = case AKM.lookup (AKey.fromString "long") km of
                      Just (A.String t) ->
                        -- Exactly maxArgStringLen chars + one "…" code point
                        T.length t == Logging.maxArgStringLen + 1
                        && T.last t == '\8230'   -- U+2026 HORIZONTAL ELLIPSIS
                      _ -> False
          shortOk = case AKM.lookup (AKey.fromString "short") km of
                      Just (A.String t) -> t == shortStr
                      _                 -> False
          numOk   = case AKM.lookup (AKey.fromString "num") km of
                      Just (A.Number _) -> True
                      _                 -> False
          flagOk  = case AKM.lookup (AKey.fromString "flag") km of
                      Just (A.Bool True) -> True
                      _                  -> False
      in pure (longOk && shortOk && numOk && flagOk)
    _ -> pure False

-- | 'newLogContext' must produce a 'LogContext' whose 'lcTraceId' is
-- exactly 6 characters long and consists solely of lowercase hex digits.

testLoggingTraceIdGeneration :: IO Bool

testLoggingTraceIdGeneration = do
  ctx <- Logging.newLogContext "ghc_test"
  let tid = Logging.lcTraceId ctx
  pure ( T.length tid == 6
      && T.all (\c -> c `elem` ("0123456789abcdef" :: String)) tid
      )

-- | When 'HASKELL_FLOWS_AUDIT' is not set, 'lcAuditPath' must be 'Nothing'.

testLoggingAuditPathAbsentByDefault :: IO Bool

testLoggingAuditPathAbsentByDefault = do
  -- Ensure the env var is absent for this test.
  unsetEnv "HASKELL_FLOWS_AUDIT"
  ctx <- Logging.newLogContext "ghc_test"
  pure (isNothing (Logging.lcAuditPath ctx))
