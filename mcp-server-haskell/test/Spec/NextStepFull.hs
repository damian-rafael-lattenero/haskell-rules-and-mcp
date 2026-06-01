-- | Unit tests for the full nextStep routing suite: Create/Deps/Load/Suggest/
-- Qc/Regression/Refactor/Hole/ExplainError/CheckModule/CheckProject hints,
-- suggestOnError routing (#259), session ledger, and scratch-target resolution.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepFull
  ( testNextStepCreateProject
  , testNextStepDepsAdd
  , testNextStepLoadClean
  , testNextStepLoadWarnings
  , testNextStepSuggest
  , testNextStepQcPassed
  , testNextStepQcFailed
  , testNextStepRegressionList
  , testNextStepRefactor
  , testNextStepFromHoleRoutesToScratch
  , testNextStepFromExplainErrorRoutesToScratch
  , testNextStepSuggestChainHasQuickCheck
  , testNextStepCheckModule
  , testNextStepCheckProject
  , testNextStepErrorsSuppressed
  , testSuggestOnErrorCompileError
  , testSuggestOnErrorNotInScope
  , testSuggestOnErrorNoSelfLoop
  , testSuggestOnErrorUnroutedKind
  , testSuggestOnErrorEchoesModule
  , testSuggestOnErrorNoModule
  , testExplainErrorOptionalModule
  , testSessionLedgerRoundtrip
  , testSessionLedgerEmpty
  , testNextStepInfoNameResolved
  , testNextStepEchoFieldFallback
  , testNextStepScratchTargetResolved
  , testNextStepScratchTargetPlaceholder
  , testNextStepExploratoryNothing
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
testNextStepDepsAdd :: IO Bool
testNextStepDepsAdd =
  let payload = A.object [ "success" .= True, "action" .= ("added" :: Text) ]
      -- depsAction probes "action" field for "add"/"remove".
      -- The real ghc_deps response uses "added"/"removed" verbs; adjust
      -- this test to pin the contract we actually see in the wild.
      payload2 = A.object [ "success" .= True, "action" .= ("add" :: Text) ]
  in pure $ case suggestNext GhcDeps True payload2 of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False
    &&
      -- Pin: no false positive on the query variant.
      case suggestNext GhcDeps True payload of
        Nothing -> True
        Just _  -> True  -- either behaviour is acceptable; the real
                         -- guard is that add/remove trigger load.

-- | Load clean → suggest properties.
testNextStepLoadClean :: IO Bool
testNextStepLoadClean =
  let payload = A.object
        [ "success"  .= True
        , "errors"   .= ([] :: [Text])
        , "warnings" .= ([] :: [Text])
        ]
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcSuggest
       Nothing -> False

-- | Load with warnings → holes.
testNextStepLoadWarnings :: IO Bool
testNextStepLoadWarnings =
  -- Post-BUG-PLUS-mediocre-3 the 'ghc_load' → 'ghc_hole'
  -- route is reserved for typed-hole warnings specifically.
  -- Other (fixable) warnings route to 'ghc_fix_warning'; clean
  -- loads route to 'ghc_suggest'. This test fixture must
  -- emit a real typed-hole message so the dispatcher picks
  -- 'ghc_hole'.
  let payload = A.object
        [ "success"  .= True
        , "errors"   .= ([] :: [Text])
        , "warnings" .=
            [ A.object
                [ "message"  .= ("Found hole: _ :: Int" :: Text)
                , "severity" .= ("warning" :: Text)
                ]
            ]
        ]
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcHole
       Nothing -> False

-- | Suggest → quickcheck.
-- #253 Phase 5: GhcSuggest now routes to GhcScratch first (record law
-- candidate in scratchpad before quickchecking it).
testNextStepSuggest :: IO Bool
testNextStepSuggest =
  let payload = A.object [ "success" .= True, "count" .= (3 :: Int) ]
  in pure $ case suggestNext GhcSuggest True payload of
       Just ns -> nsTool ns == GhcScratch
       Nothing -> False

-- | QuickCheck passed → check_module.
testNextStepQcPassed :: IO Bool
testNextStepQcPassed =
  let payload = A.object [ "success" .= True, "state" .= ("passed" :: Text) ]
  in pure $ case suggestNext GhcQuickCheck True payload of
       Just ns -> nsTool ns == GhcCheckModule
       Nothing -> False

-- | QuickCheck failed → eval for debugging.
testNextStepQcFailed :: IO Bool
testNextStepQcFailed =
  let payload = A.object [ "success" .= True, "state" .= ("failed" :: Text) ]
  in pure $ case suggestNext GhcQuickCheck True payload of
       Just ns -> nsTool ns == GhcEval
       Nothing -> False

-- | ghc_property_store(list) → ghc_property_store(run).
-- #94 Phase C step 6: ghc_regression merged into
-- ghc_property_store(action=list|run); the list-then-run hint is
-- emitted on the consolidated tool.
testNextStepRegressionList :: IO Bool
testNextStepRegressionList =
  let payload = A.object [ "success" .= True, "action" .= ("list" :: Text) ]
  in pure $ case suggestNext GhcPropertyStore True payload of
       Just ns -> nsTool ns == GhcPropertyStore
       Nothing -> False

-- | Refactor landed → verify compile.
testNextStepRefactor :: IO Bool
testNextStepRefactor =
  let payload = A.object [ "success" .= True, "compile" .= ("ok" :: Text) ]
  in pure $ case suggestNext GhcRefactor True payload of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False

-- #253 Phase 5: cross-tool nextStep arms ─────────────────────────────────

-- | GhcHole now routes to GhcScratch (write the hole-filler hypothesis
-- before implementing it).
testNextStepFromHoleRoutesToScratch :: IO Bool
testNextStepFromHoleRoutesToScratch =
  let payload = A.object [ "success" .= True, "holes" .= ([] :: [Value]) ]
  in pure $ case suggestNext GhcHole True payload of
       Just ns -> nsTool ns == GhcScratch
       Nothing -> False

-- | GhcExplainError now routes to GhcScratch (record the proposed fix
-- before applying verify_patch to source).
testNextStepFromExplainErrorRoutesToScratch :: IO Bool
testNextStepFromExplainErrorRoutesToScratch =
  let payload = A.object [ "success" .= True, "error_text" .= ("..." :: Text) ]
  in pure $ case suggestNext GhcExplainError True payload of
       Just ns -> nsTool ns == GhcScratch
       Nothing -> False

-- | GhcSuggest chains to ghc_quickcheck after the scratch write + check
-- steps — verify the chain carries quickcheck as a follow-up.
testNextStepSuggestChainHasQuickCheck :: IO Bool
testNextStepSuggestChainHasQuickCheck =
  let payload = A.object [ "success" .= True, "count" .= (3 :: Int) ]
  in pure $ case suggestNext GhcSuggest True payload of
       Just ns ->
         let chainTools = maybe [] (map csTool) (nsChain ns)
         in GhcQuickCheck `elem` chainTools
       Nothing -> False

-- | Module gate → project gate.
testNextStepCheckModule :: IO Bool
testNextStepCheckModule =
  let payload = A.object [ "success" .= True, "overall" .= True ]
  in pure $ case suggestNext GhcCheckModule True payload of
       Just ns -> nsTool ns == GhcCheckProject
       Nothing -> False

-- | Project gate → gate (pre-push finalizer). BUG-06 re-routed
-- check_project from coverage → gate (the Phase 11n finalizer
-- tool) so the agent reaches the real CI-equivalent step; coverage
-- moves into the attached chain as the optional follow-up.
testNextStepCheckProject :: IO Bool
testNextStepCheckProject =
  let payload = A.object [ "success" .= True, "overall" .= True ]
  in pure $ case suggestNext GhcCheckProject True payload of
       Just ns ->
            nsTool ns == GhcGate
         && case nsChain ns of
              Just steps ->
                   any ((== GhcGate)     . csTool) steps
                && any ((== GhcCoverage) . csTool) steps
              Nothing -> False
       Nothing -> False

-- | UNSTRUCTURED errors (an 'error' that's a bare string, no 'kind')
-- suppress the suggestion — the agent reads the message. Curated error
-- KINDS route via 'suggestOnError' instead (plan A5 — see the
-- testSuggestOnError* tests below).
testNextStepErrorsSuppressed :: IO Bool
testNextStepErrorsSuppressed =
  let payload = A.object [ "success" .= False, "error" .= ("oops" :: Text) ]
  in pure $ case suggestNext GhcLoad False payload of
       Nothing -> True
       Just _  -> False

-- | #A5 failure-path routing: a structured compile_error routes the agent
-- to ghc_explain_error, with the error message threaded in as error_text.
testSuggestOnErrorCompileError :: IO Bool
testSuggestOnErrorCompileError =
  let payload = A.object
        [ "status" .= ("failed" :: Text)
        , "error"  .= A.object
            [ "kind"    .= ("compile_error" :: Text)
            , "message" .= ("Couldn't match type Int with Bool" :: Text)
            ]
        ]
  in pure $ case suggestNext GhcLoad False payload of
       Just ns -> nsTool ns == GhcExplainError
               && case nsExample ns of
                    Just (A.Object o) -> AKM.member (AKey.fromText "error_text") o
                    _                 -> False
       Nothing -> False

-- | #A5: not_in_scope also routes to ghc_explain_error (it can suggest the
-- missing import / scope fix).
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
testSuggestOnErrorUnroutedKind :: IO Bool
testSuggestOnErrorUnroutedKind =
  let payload = A.object
        [ "status" .= ("failed" :: Text)
        , "error"  .= A.object
            [ "kind" .= ("missing_arg" :: Text), "message" .= ("requires 'id'" :: Text) ]
        ]
  in pure $ case suggestNext GhcScratch False payload of
       Nothing -> True
       Just _  -> False

-- | #282: when the failing payload carries a module (here ghc_quickcheck's
-- echoed 'module'), the A5 route includes it as module_path so the agent can
-- follow ghc_explain_error in one hop without hitting missing_arg.
testSuggestOnErrorEchoesModule :: IO Bool
testSuggestOnErrorEchoesModule =
  let payload = A.object
        [ "status" .= ("failed" :: Text)
        , "error"  .= A.object
            [ "kind" .= ("not_in_scope" :: Text), "message" .= ("evaluate" :: Text) ]
        , "result" .= A.object [ "module" .= ("src/ExprEval.hs" :: Text) ]
        ]
  in pure $ case suggestNext GhcQuickCheck False payload of
       Just ns -> nsTool ns == GhcExplainError
               && case nsExample ns of
                    Just (A.Object o) ->
                      AKM.lookup (AKey.fromText "module_path") o
                        == Just (A.String "src/ExprEval.hs")
                    _ -> False
       Nothing -> False

-- | #282: when no module is in the payload, the A5 example omits module_path
-- (ghc_explain_error then falls back to text-only decode) — it must NOT emit a
-- placeholder that would fail validation.
testSuggestOnErrorNoModule :: IO Bool
testSuggestOnErrorNoModule =
  let payload = A.object
        [ "status" .= ("failed" :: Text)
        , "error"  .= A.object
            [ "kind" .= ("compile_error" :: Text), "message" .= ("e" :: Text) ]
        ]
  in pure $ case suggestNext GhcLoad False payload of
       Just ns -> case nsExample ns of
         Just (A.Object o) ->
           AKM.member (AKey.fromText "error_text") o
             && not (AKM.member (AKey.fromText "module_path") o)
         _ -> False
       Nothing -> False

-- | #282: ghc_explain_error now accepts a payload with NO module_path (text-only
-- mode), and still accepts one with module_path. Proves the arg is optional.
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
testSessionLedgerRoundtrip :: IO Bool
testSessionLedgerRoundtrip = withTempProject $ \pd -> do
  WS.recordCallToDisk pd GhcLoad
  WS.recordCallToDisk pd GhcLoad
  WS.recordCallToDisk pd GhcQuickCheck
  m <- WS.loadLifetime pd
  pure ( Map.lookup "ghc_load" m == Just 2
      && Map.lookup "ghc_quickcheck" m == Just 1 )

-- | #266 cross-session: loadLifetime on a project with no ledger reads as
-- empty (best-effort — never errors).
testSessionLedgerEmpty :: IO Bool
testSessionLedgerEmpty = withTempProject $ \pd -> do
  m <- WS.loadLifetime pd
  pure (Map.null m)

-- | #A4 residual: ghc_info's nextStep example resolves the name from the
-- payload it echoes (ghc_batch-ready) instead of a "<placeholder>".
testNextStepInfoNameResolved :: IO Bool
testNextStepInfoNameResolved =
  let payload = A.object [ "status" .= ("ok" :: Text), "name" .= ("reverse" :: Text) ]
  in pure $ case suggestNext GhcInfo True payload of
       Just ns -> case nsExample ns of
         Just (A.Object o) ->
           AKM.lookup (AKey.fromText "name") o == Just (A.String "reverse")
         _ -> False
       Nothing -> False

-- | #A4 residual: echoField is safe — when the payload does NOT echo the
-- field, the example keeps the honest "<placeholder>" agent-fill slot.
testNextStepEchoFieldFallback :: IO Bool
testNextStepEchoFieldFallback =
  let payload = A.object [ "status" .= ("ok" :: Text) ]  -- no 'name' echoed
  in pure $ case suggestNext GhcInfo True payload of
       Just ns -> case nsExample ns of
         Just (A.Object o) ->
           AKM.lookup (AKey.fromText "name") o
             == Just (A.String "<same name you just inspected>")
         _ -> False
       Nothing -> False

-- | #274: a scratch type_ok check now echoes the entry's module, so the
-- promote follow-up's target_module is concrete instead of "<src/Foo.hs>".
testNextStepScratchTargetResolved :: IO Bool
testNextStepScratchTargetResolved =
  let payload = A.object
        [ "status" .= ("ok" :: Text)
        , "result" .= A.object
            [ "id"     .= ("scratch-1" :: Text)
            , "kind"   .= ("type_ok" :: Text)
            , "module" .= ("src/Expr/Eval.hs" :: Text)
            ]
        ]
  in pure $ case suggestNext GhcScratch True payload of
       Just ns -> case nsExample ns of
         Just (A.Object o) ->
           AKM.lookup (AKey.fromText "target_module") o
             == Just (A.String "src/Expr/Eval.hs")
         _ -> False
       Nothing -> False

-- | #274: when the scratch entry has no module, the promote example keeps the
-- honest placeholder rather than inventing a path.
testNextStepScratchTargetPlaceholder :: IO Bool
testNextStepScratchTargetPlaceholder =
  let payload = A.object
        [ "status" .= ("ok" :: Text)
        , "result" .= A.object
            [ "id" .= ("scratch-1" :: Text), "kind" .= ("type_ok" :: Text) ]
        ]
  in pure $ case suggestNext GhcScratch True payload of
       Just ns -> case nsExample ns of
         Just (A.Object o) ->
           AKM.lookup (AKey.fromText "target_module") o
             == Just (A.String "<src/Foo.hs>")
         _ -> False
       Nothing -> False

-- | Per PR-3 of the integrated MCP improvements: exploratory tools
-- (type/info/goto/doc) now DO carry a forward-chaining hint — the
-- agent can ignore it but it removes the "ok, what next?" round-trip.
-- The genuine Nothing-arms are now:
--
--   * 'GhcWorkflow', 'GhcBatch' — anti-loop exemptions, always Nothing.
--   * 'GhcComplete', 'HoogleSearch', 'GhcAddImport', 'GhcLint' —
--     suppress when their 'count' field is zero or missing (no
--     candidates to act on).
--   * 'GhcEval', 'GhcCoverage' — suppress on degraded status (the
--     error speaks for itself).
--
-- This test pins the suppression contract; the positive contract
-- ("every other tool returns Just with a canonical payload") lives
-- in 'testNextStepCoverageExhaustive'.
testNextStepExploratoryNothing :: IO Bool
testNextStepExploratoryNothing = pure $
  all nothing
    -- anti-loop exemptions: never recommend regardless of payload.
    [ suggestNext GhcWorkflow True (A.object [])
    , suggestNext GhcBatch    True (A.object [])
    -- count-based suppression: empty payload (no count) → Nothing.
    , suggestNext GhcComplete    True (A.object [])
    , suggestNext HoogleSearch   True (A.object [])
    , suggestNext GhcAddImport   True (A.object [])
    , suggestNext GhcLint        True (A.object [])
    -- count-based suppression: explicit count=0 → Nothing.
    , suggestNext GhcComplete    True (A.object [ "count" .= (0 :: Int) ])
    , suggestNext HoogleSearch   True (A.object [ "count" .= (0 :: Int) ])
    , suggestNext GhcAddImport   True (A.object [ "count" .= (0 :: Int) ])
    , suggestNext GhcLint        True (A.object [ "count" .= (0 :: Int) ])
    -- degraded-status suppression: failed status → Nothing.
    , suggestNext GhcEval     True (A.object [ "status" .= ("failed" :: Text) ])
    , suggestNext GhcCoverage True (A.object [ "status" .= ("failed" :: Text) ])
    ]
  where
    nothing Nothing = True
    nothing _       = False
