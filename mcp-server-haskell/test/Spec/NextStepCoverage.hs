-- | Unit tests for nextStep routing: info/doc/goto no-match routes,
-- coverage exhaustiveness, action coverage, suppress guards, and
-- JSON-inject splices.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepCoverage
  ( testNextStepInfoNoMatchIsHoogle
  , testNextStepDocNoMatchIsHoogle
  , testNextStepGotoNoMatchIsGhcLoad
  , testNextStepInfoFoundIsDoc
  , testNextStepCoverageExhaustive
  , testNextStepActionCoverage
  , testNextStepSuppressIfTrue
  , testNextStepSuppressIfFalse
  , testNextStepSuppressOnDegraded
  , testNextStepSuppressOnZero
  , testNextStepSuppressOnZeroPass
  , testInjectSplices
  , testInjectSkipsNonJson
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (isNothing, isJust)
import qualified Data.Set as Set
import Control.Monad (unless)
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Mcp.ToolName (allToolNames, ToolName (..))
-- injectNextStep is re-exported from HaskellFlows.Mcp.NextStep (already imported above)

-- | #185: ghc_info on a name not found (status=no_match) must route to
-- hoogle_search, not ghc_doc (which will also no_match on the same name).
testNextStepInfoNoMatchIsHoogle :: IO Bool
testNextStepInfoNoMatchIsHoogle =
  let payload = A.object [ "status" .= ("no_match" :: Text), "name" .= ("unknownXYZ" :: Text) ]
  in pure $ case suggestNext GhcInfo True payload of
       Just ns -> nsTool ns == HoogleSearch
       Nothing -> False

-- | #185: ghc_doc on a name not found (status=no_match) must route to
-- hoogle_search, not ghc_browse (which expects a module, not a symbol).
testNextStepDocNoMatchIsHoogle :: IO Bool
testNextStepDocNoMatchIsHoogle =
  let payload = A.object [ "status" .= ("no_match" :: Text), "name" .= ("unknownXYZ" :: Text) ]
  in pure $ case suggestNext GhcDoc True payload of
       Just ns -> nsTool ns == HoogleSearch
       Nothing -> False

-- | #251: ghc_goto on a name not found (status=no_match) must route to
-- ghc_load (so the user loads the module containing the symbol), not
-- hoogle_search (which searches Hackage and is wrong for project-local names).
testNextStepGotoNoMatchIsGhcLoad :: IO Bool
testNextStepGotoNoMatchIsGhcLoad =
  let payload = A.object [ "status" .= ("no_match" :: Text), "name" .= ("unknownXYZ" :: Text) ]
  in pure $ case suggestNext GhcGoto True payload of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False

-- | #185: ghc_info on a name FOUND (status=ok) must still route to ghc_doc,
-- not hoogle_search — the no_match branch must not fire on success.
testNextStepInfoFoundIsDoc :: IO Bool
testNextStepInfoFoundIsDoc =
  let payload = A.object [ "status" .= ("ok" :: Text), "name" .= ("Data.List.sort" :: Text) ]
  in pure $ case suggestNext GhcInfo True payload of
       Just ns -> nsTool ns == GhcDoc
       Nothing -> False

-- | PR-3 exhaustivity: every tool except the 2 anti-loop exemptions
-- (GhcWorkflow, GhcBatch) returns 'Just' for a canonical success
-- payload. Adding a new ToolName constructor without filling in a
-- dispatch arm fails this test (as well as the -Wincomplete-patterns
-- warning on 'dispatch').
--
-- The canonical payload per tool exercises the success path: tools
-- that suppress on missing fields get rich payloads; everything else
-- falls back to {status:"ok"}.
testNextStepCoverageExhaustive :: IO Bool
testNextStepCoverageExhaustive = do
  let exempt :: Set.Set ToolName
      exempt = Set.fromList [GhcWorkflow, GhcBatch]
      missing =
        [ n | n <- allToolNames
            , n `Set.notMember` exempt
            , isNothing (suggestNext n True (canonicalPayload n)) ]
  unless (null missing) $
    putStrLn ("nextStep coverage gap: " <> show missing)
  pure (null missing)
  where
    -- Default success envelope; tools that need richer discriminators
    -- override below.
    defaultPayload :: Value
    defaultPayload = A.object [ "status" .= ("ok" :: Text) ]

    canonicalPayload :: ToolName -> Value
    canonicalPayload = \case
      -- count-gated suggestions: provide a non-zero count.
      GhcLint        -> A.object [ "count" .= (3 :: Int) ]
      GhcAddImport   -> A.object [ "count" .= (3 :: Int) ]
      GhcComplete    -> A.object [ "count" .= (3 :: Int) ]
      HoogleSearch   -> A.object [ "count" .= (3 :: Int) ]
      -- shape-gated dispatchers: feed the right discriminator.
      GhcLoad        -> A.object [ "warnings" .= ([] :: [Value])
                                 , "errors"   .= ([] :: [Value]) ]
      GhcDeps        -> A.object [ "action" .= ("add" :: Text) ]
      GhcQuickCheck  -> A.object [ "state"  .= ("passed" :: Text) ]
      GhcRefactor    -> A.object [ "action" .= ("rename_local" :: Text) ]
      GhcPropertyStore -> A.object [ "action" .= ("list" :: Text) ]
      GhcProject     -> A.object [ "scaffolded" .= True ]
      GhcGate        -> A.object [ "status" .= ("ok" :: Text) ]
      -- #253: ghc_scratch action-discriminated by payload shape. Use the
      -- write/show single-entry shape (carries 'id').
      GhcScratch     -> A.object [ "id" .= ("scratch-1" :: Text)
                                 , "kind" .= ("hypothesis" :: Text) ]
      -- everything else: bare success envelope is enough.
      _              -> defaultPayload

-- | Action-discriminated coverage: ghc_property_store, ghc_modules,
-- ghc_project, and ghc_deps each branch on 'action'. This test
-- exercises every action and confirms a Just result, catching the
-- "we forgot to wire one branch" regression.
testNextStepActionCoverage :: IO Bool
testNextStepActionCoverage = pure $
  all justOf
    [ suggestNext GhcPropertyStore True (A.object [ "action" .= ("list" :: Text) ])
    , suggestNext GhcPropertyStore True (A.object [ "action" .= ("run"  :: Text) ])
    -- export branch carries 'files_written' instead of 'action'
    , suggestNext GhcPropertyStore True
        (A.object [ "files_written" .= (["test/Spec.hs"] :: [Text]) ])
    -- audit branch carries 'findings' instead of 'action'
    , suggestNext GhcPropertyStore True
        (A.object [ "findings" .= ([] :: [Value]) ])
    -- ghc_deps every action.
    , suggestNext GhcDeps True (A.object [ "action" .= ("add"     :: Text) ])
    , suggestNext GhcDeps True (A.object [ "action" .= ("remove"  :: Text) ])
    , suggestNext GhcDeps True (A.object [ "action" .= ("explain" :: Text) ])
    -- ghc_project: each branch keys off a different shape field.
    , suggestNext GhcProject True (A.object [ "scaffolded" .= True ])  -- switch
    , suggestNext GhcProject True (A.object [ "host"       .= ("claude" :: Text) ])  -- bootstrap
    , suggestNext GhcProject True (A.object [ "errors"     .= (1 :: Int) ])  -- validate w/ errors
    , suggestNext GhcProject True (A.object [])  -- fallthrough = create
    ]
  where
    justOf (Just _) = True
    justOf Nothing  = False

-- Issue #95 Phase A: suppression rule unit tests --------------------------------

-- | suppressIf suppresses when the predicate returns True.
testNextStepSuppressIfTrue :: IO Bool
testNextStepSuppressIfTrue =
  let ns   = NextStep { nsTool = GhcLoad, nsWhy = "w", nsExample = Nothing, nsChain = Nothing, nsDogfood = Nothing }
      ctx  = NextStep.RecommendCtx { NextStep.rcTool = GhcLoad, NextStep.rcStatus = "ok", NextStep.rcPayload = A.object [] }
      rule = const True
  in pure (isNothing (NextStep.suppressIf rule ctx (Just ns)))

-- | suppressIf passes through when the predicate returns False.
testNextStepSuppressIfFalse :: IO Bool
testNextStepSuppressIfFalse =
  let ns   = NextStep { nsTool = GhcLoad, nsWhy = "w", nsExample = Nothing, nsChain = Nothing, nsDogfood = Nothing }
      ctx  = NextStep.RecommendCtx { NextStep.rcTool = GhcLoad, NextStep.rcStatus = "ok", NextStep.rcPayload = A.object [] }
      rule = const False
  in pure (isJust (NextStep.suppressIf rule ctx (Just ns)))

-- | suppressOnDegraded returns True (suppress) for "failed" status.
testNextStepSuppressOnDegraded :: IO Bool
testNextStepSuppressOnDegraded =
  let ctx = NextStep.RecommendCtx { NextStep.rcTool = GhcLoad
                                  , NextStep.rcStatus = "failed"
                                  , NextStep.rcPayload = A.object []
                                  }
  in pure (NextStep.suppressOnDegraded ctx)

-- | suppressOnZero suppresses when count field is zero.
testNextStepSuppressOnZero :: IO Bool
testNextStepSuppressOnZero =
  let ctx = NextStep.RecommendCtx { NextStep.rcTool = GhcAddImport
                                  , NextStep.rcStatus = "ok"
                                  , NextStep.rcPayload = A.object ["count" A..= (0 :: Int)]
                                  }
  in pure (NextStep.suppressOnZero "count" ctx)

-- | suppressOnZero passes when count field is nonzero.
testNextStepSuppressOnZeroPass :: IO Bool
testNextStepSuppressOnZeroPass =
  let ctx = NextStep.RecommendCtx { NextStep.rcTool = GhcAddImport
                                  , NextStep.rcStatus = "ok"
                                  , NextStep.rcPayload = A.object ["count" A..= (3 :: Int)]
                                  }
  in pure (not (NextStep.suppressOnZero "count" ctx))

-- | injectNextStep splices the nextStep into the first TextContent
-- block's JSON payload.
testInjectSplices :: IO Bool
testInjectSplices =
  let body = A.object [ "success" .= True, "data" .= (42 :: Int) ]
      txt  = TL.toStrict (TLE.decodeUtf8 (A.encode body))
      tr   = ToolResult { trContent = [ TextContent txt ], trIsError = False }
      ns   = NextStep { nsTool = GhcLoad, nsWhy = "because"
                      , nsExample = Nothing, nsChain = Nothing
                      , nsDogfood = Nothing }
      tr'  = injectNextStep ns tr
  in case trContent tr' of
       [TextContent t] -> pure $
         T.isInfixOf "\"nextStep\"" t
           && T.isInfixOf "\"ghc_load\"" t
           && T.isInfixOf "\"data\":42" t
           -- original field preserved
       _ -> pure False

-- | injectNextStep must NOT corrupt non-JSON payloads.
testInjectSkipsNonJson :: IO Bool
testInjectSkipsNonJson =
  let raw = "this is not json"
      tr  = ToolResult { trContent = [ TextContent raw ], trIsError = False }
      ns  = NextStep { nsTool = GhcLoad, nsWhy = "x"
                     , nsExample = Nothing, nsChain = Nothing
                     , nsDogfood = Nothing }
      tr' = injectNextStep ns tr
  in case trContent tr' of
       [TextContent t] -> pure (t == raw)  -- unchanged
       _ -> pure False
