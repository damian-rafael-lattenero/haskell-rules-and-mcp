-- | Unit tests for 'Tool.Hoogle' and 'Tool.AddImport' envelope shapes
-- and guard cases.
--
-- The session-bound Info/AddImport handlers died with the ApiSession
-- backend (W6.8); their resolution behavior is covered end-to-end by
-- the e2e suite against the ghcide route. These tests pin the
-- session-free guard paths that still live in 'HoogleTool.handle' and
-- 'AddImportTool.runHandle'.
module Spec.InfoHoogle
  ( testHoogleRejectsEmpty
  , testHoogleUnavailable
  , testAddImportUnavailable
  , testAddImportRejectsMissingArg
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Maybe (isJust)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Control.Exception (bracket_)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Tool.Env (ToolEnv (..))
import qualified HaskellFlows.Tool.Hoogle as HoogleTool
import qualified HaskellFlows.Tool.AddImport as AddImportTool

import Spec.ToolEnvFixture (stubEnv)

-- | Phase B helper: drive 'HoogleTool.handle'. Hoogle is not
-- session-bound — the stub env is sufficient.
runHoogle :: A.Value -> IO (Either String Env.ToolResponse)
runHoogle args = do
  tr <- HoogleTool.handle stubEnv args
  pure (Right tr)

-- | Drive 'AddImportTool.runHandle' with a stub inject function. The
-- tests here short-circuit before any injection happens (hoogle
-- missing, parse error), so the stub is never called.
runAddImport :: A.Value -> IO (Either String Env.ToolResponse)
runAddImport args = do
  tr <- AddImportTool.runHandle
          (teLimits stubEnv)
          (\_ -> pure (False, "stub inject"))
          args
  pure (Right tr)

-- | An empty hoogle query → status='refused' with
-- kind='empty_input' + field='query'.
testHoogleRejectsEmpty :: IO Bool
testHoogleRejectsEmpty = do
  decoded <- runHoogle (A.object [ "query" A..= ("" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.EmptyInput
            && Env.eeField err == Just "query"
    _ -> False

-- | When the hoogle binary isn't on PATH, the status is
-- 'unavailable' (NOT 'failed'). Distinct discriminator: an
-- environment-binary issue is structurally different from a
-- runtime failure. The test scrubs PATH around the call to
-- guarantee the missing-binary code path fires regardless of
-- the host's actual hoogle install.
testHoogleUnavailable :: IO Bool
testHoogleUnavailable = do
  origPath <- lookupEnv "PATH"
  let scrubbed = "/var/empty-haskell-flows-no-hoogle"
  decoded <- bracket_
    (setEnv "PATH" scrubbed)
    (case origPath of
       Just p  -> setEnv "PATH" p
       Nothing -> unsetEnv "PATH")
    (runHoogle (A.object [ "query" A..= ("filter" :: T.Text) ]))
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusUnavailable
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.BinaryUnavailable
            && isJust (Env.eeRemediation err)
    _ -> False

-- | ghc_add_import shares the unavailable contract with hoogle_search.
testAddImportUnavailable :: IO Bool
testAddImportUnavailable = do
  origPath <- lookupEnv "PATH"
  let scrubbed = "/var/empty-haskell-flows-no-hoogle"
  decoded <- bracket_
    (setEnv "PATH" scrubbed)
    (case origPath of
       Just p  -> setEnv "PATH" p
       Nothing -> unsetEnv "PATH")
    (runAddImport (A.object [ "name" A..= ("fromMaybe" :: T.Text) ]))
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusUnavailable
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.BinaryUnavailable
    _ -> False

-- | Empty args (missing 'name') → status='failed' with
-- error.kind='missing_arg'.
testAddImportRejectsMissingArg :: IO Bool
testAddImportRejectsMissingArg = do
  decoded <- runAddImport (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.MissingArg
    _ -> False
