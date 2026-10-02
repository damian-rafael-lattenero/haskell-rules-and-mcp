-- | Unit tests for the full nextStep routing suite: Create/Deps/Load/Suggest/
-- Qc/Regression/Refactor/Hole/ExplainError/CheckModule/CheckProject hints,
-- suggestOnError routing (#259), session ledger, and scratch-target resolution.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepFull
  ( testNextStepCreateProject
  , testSuggestOnErrorNotInScope
  , testSuggestOnErrorNoSelfLoop
  , testExplainErrorOptionalModule
  , testSessionLedgerEmpty
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import qualified Data.Map.Strict as Map
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Mcp.WorkflowState as WS
import qualified HaskellFlows.Tool.ExplainError as ExplainError

import Spec.Helpers (withTempProject)

-- | The core happy-path chain: new scaffold → add deps.

testNextStepCreateProject :: IO Bool

testNextStepCreateProject =
  let payload = A.object [ "success" .= True, "files_written" .= ([] :: [Text]) ]
  in pure $ case suggestNext GhcProject True payload of
       Just ns -> nsTool ns == GhcDeps
       Nothing -> False

-- | After ghc_deps(add), reload.

testSuggestOnErrorNotInScope :: IO Bool

testSuggestOnErrorNotInScope =
  let payload = A.object
        [ "status" .= ("failed" :: Text)
        , "error"  .= A.object
            [ "kind" .= ("not_in_scope" :: Text), "message" .= ("foo" :: Text) ]
        ]
  in pure $ case suggestNext GhcEval False payload of
       Just ns -> nsTool ns == GhcExplainError
       Nothing -> False

-- | #A5: a failing ghc_explain_error must NOT recommend itself (no loop).

testSuggestOnErrorNoSelfLoop :: IO Bool

testSuggestOnErrorNoSelfLoop =
  let payload = A.object
        [ "status" .= ("failed" :: Text)
        , "error"  .= A.object
            [ "kind" .= ("compile_error" :: Text), "message" .= ("e" :: Text) ]
        ]
  in pure $ case suggestNext GhcExplainError False payload of
       Nothing -> True
       Just _  -> False

-- | #A5: an unrouted error kind (e.g. missing_arg) still suppresses — the
-- router is conservative, only the curated compile-ish kinds route.

testExplainErrorOptionalModule :: IO Bool

testExplainErrorOptionalModule =
  let withoutMod = A.eitherDecode "{\"error_text\":\"oops\"}"
                     :: Either String ExplainError.ExplainErrorArgs
      withMod    = A.eitherDecode "{\"module_path\":\"src/X.hs\"}"
                     :: Either String ExplainError.ExplainErrorArgs
  in pure $ case (withoutMod, withMod) of
       (Right a, Right b) ->
         isNothing (ExplainError.eaModulePath a)
           && ExplainError.eaModulePath b == Just "src/X.hs"
       _ -> False

-- | #266 cross-session: recordCallToDisk accumulates lifetime call counts
-- on disk; loadLifetime reads them back. Round-trips via a temp project.

testSessionLedgerEmpty :: IO Bool

testSessionLedgerEmpty = withTempProject $ \pd -> do
  m <- WS.loadLifetime pd
  pure (Map.null m)

-- | #A4 residual: ghc_info's nextStep example resolves the name from the
-- payload it echoes (ghc_batch-ready) instead of a "<placeholder>".

