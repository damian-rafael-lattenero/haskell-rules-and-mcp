-- | Unit tests for 'Tool.AddImport' missing-hoogle handling and the
-- add-modules path helper.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.AddImportUnit
  ( testAddImportMissingHoogle
  , testAddModulesPath
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing)
import System.Environment (lookupEnv, setEnv, unsetEnv)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..))
import HaskellFlows.Config (defaultLimits)
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.AddModules as AddModules


testAddImportMissingHoogle :: IO Bool

testAddImportMissingHoogle = do
  origPath <- lookupEnv "PATH"
  setEnv "PATH" "/nonexistent/path-for-test-only"
  -- Fails at the hoogle-availability gate, before any injection.
  let args = A.object [ "name" A..= ("fromMaybe" :: T.Text) ]
  result <- AddImport.runHandle defaultLimits
              (\_ -> pure (False, "stub inject")) args
  -- Restore PATH so other tests aren't affected.
  case origPath of
    Just p  -> setEnv "PATH" p
    Nothing -> unsetEnv "PATH"
  -- Issue #90 Phase D step 2: branch on status and structured error.kind directly.
  pure $ Env.reStatus result == Env.StatusUnavailable
      && case Env.reError result of
           Just err ->
             let msgOk = "hoogle" `T.isInfixOf` T.toLower (Env.eeMessage err)
                 remOk = case Env.eeRemediation err of
                   Just _  -> True
                   Nothing -> False
             in msgOk && remOk
           Nothing -> False

-- W6.8: the addImportToSession unit tests died with the legacy
-- session handler — the injection contract (validate + record into
-- the ghcide accumulator) is pinned end-to-end by the W6.7
-- ghc_edit(action=import) scenario.

-- | Issue #53: nextStep dispatch on a ghc_add_import payload
-- with @count: 0@ must return 'Nothing' (no \"reload to confirm\"
-- nudge), since nothing was added.

testAddModulesPath :: IO Bool

testAddModulesPath = pure $
     AddModules.moduleToPath "Expr.Syntax"  == "src/Expr/Syntax.hs"
  && AddModules.moduleToPath "Main"         == "src/Main.hs"
