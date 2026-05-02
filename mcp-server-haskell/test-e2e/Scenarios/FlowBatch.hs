-- | Flow: composition via @ghc_batch@.
--
-- Exercises the tool that runs N sub-calls sequentially in a
-- single request. This is the primitive that the multi-step
-- 'nextStep.chain' field is designed to hand off to (BUG-22).
--
-- Two passes:
--
--   (1) Happy composition — three actions (add_modules → deps
--       → workflow) run in sequence; aggregated result reports
--       total = 3, ok = 3, failed = 0.
--   (2) Fail-fast short-circuit — deliberately-broken dep
--       (invalid package name) as action #2; with default
--       fail_fast=true, action #3 must be reported as skipped.
--
-- Tools exercised:
--
--   ghc_batch  (composition primitive)
-- Indirectly every tool that appears as a sub-action.
module Scenarios.FlowBatch
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- runFlow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  ----------------------------------------------------------------
  -- setup: just scaffold so the sub-actions have something to
  -- operate on.
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("batch-demo" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) Happy composition: 3 legit actions in order.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_batch(3 happy actions)"
  let happyActions =
        [ object
            [ "tool" .= ("ghc_modules" :: Text)
            , "args" .= object
                [ "action"  .= ("add" :: Text)
                , "modules" .= (["Foo", "Bar"] :: [Text])
                ]
            ]
        , object
            [ "tool" .= ("ghc_deps" :: Text)
            , "args" .= object
                [ "action"  .= ("add" :: Text)
                , "package" .= ("text" :: Text)
                , "stanza"  .= ("library" :: Text)
                ]
            ]
        , object
            [ "tool" .= ("ghc_workflow" :: Text)
            , "args" .= object [ "action" .= ("status" :: Text) ]
            ]
        ]
  happyR <- Client.callTool c GhcBatch
              (object [ "actions" .= happyActions ])
  c1 <- liveCheck $ checkJsonField
          "happy · overall success"
          happyR "success" (Bool True)
  c2 <- liveCheck $ checkJsonFieldMatches
          "happy · total == 3"
          happyR "total" (\v -> v == Number 3)
          "expected 3 actions total"
  c3 <- liveCheck $ checkJsonFieldMatches
          "happy · ok == 3"
          happyR "ok" (\v -> v == Number 3)
          "all 3 sub-actions should report success"
  c4 <- liveCheck $ checkJsonFieldMatches
          "happy · failed == 0"
          happyR "failed" (\v -> v == Number 0)
          "no sub-action should fail"
  c5 <- liveCheck $ checkJsonFieldMatches
          "happy · skipped == 0"
          happyR "skipped" (\v -> v == Number 0)
          "no sub-action should be skipped"
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (2) Fail-fast with OBSERVABLE side effects.
  --
  -- The earlier version of this scenario fired (status → bad-dep →
  -- help) and asserted the batch's own counters (ok/failed/skipped).
  -- That only proves the TOOL can count. A tool bug where the
  -- fail_fast flag is silently ignored and action #3 DOES run would
  -- still report ok=1, failed=1, skipped=1 if the emitter is buggy
  -- in the same direction.
  --
  -- To close that hole we chain THREE 'ghc_deps(add)' actions with
  -- distinct packages. Action #2 is deliberately invalid (validator
  -- refuses). With fail_fast=true, action #3 (bytestring) must
  -- NEVER run — the oracle is the .cabal on disk:
  --
  --    build_depends must contain 'text'       (action #1 ran)
  --    build_depends must NOT contain 'bytestring' (action #3 skipped)
  --
  -- That's an observable fact, independent of the batch's own
  -- counters. If the tool lies about counters the filesystem still
  -- tells the truth.
  ----------------------------------------------------------------
  t2 <- stepHeader 3
          "ghc_batch(fail_fast=true) · filesystem oracle"
  -- Use packages DISTINCT from the happy-case batch above so
  -- action #1 genuinely inserts (happy case already added 'text';
  -- re-adding it here returns "No change: already at desired
  -- state" which reads as failure, not success — that is the
  -- correct ghc_deps behaviour but would be a false negative
  -- for the fail-fast oracle).
  let ffActions =
        [ object  -- action 1: genuinely new dep → success
            [ "tool" .= ("ghc_deps" :: Text)
            , "args" .= object
                [ "action"  .= ("add" :: Text)
                , "package" .= ("aeson" :: Text)
                , "stanza"  .= ("library" :: Text)
                ]
            ]
        , object  -- action 2: boundary-rejected (validator)
            [ "tool" .= ("ghc_deps" :: Text)
            , "args" .= object
                [ "action"  .= ("add" :: Text)
                , "package" .= ("has spaces and ! is invalid" :: Text)
                , "stanza"  .= ("library" :: Text)
                ]
            ]
        , object  -- action 3: MUST be skipped by fail_fast
            [ "tool" .= ("ghc_deps" :: Text)
            , "args" .= object
                [ "action"  .= ("add" :: Text)
                , "package" .= ("bytestring" :: Text)
                , "stanza"  .= ("library" :: Text)
                ]
            ]
        ]
  ffR <- Client.callTool c GhcBatch (object
    [ "actions"   .= ffActions
    , "fail_fast" .= True
    ])
  -- Issue #90: post-envelope, ghc_batch with a mixed outcome
-- (ok + failed) emits status='partial' rather than success=false.
-- Pre-#90 the legacy projection mapped partial → success: true,
-- so the original test was already inaccurate; assert the
-- post-#90 status discriminator directly.
  c6 <- liveCheck $ checkJsonField
          "fail_fast · status = 'partial' (mixed outcome)"
          ffR "status" (String "partial")
  c7 <- liveCheck $ checkJsonFieldMatches
          "fail_fast · ok == 1 (first action only)"
          ffR "ok" (\v -> v == Number 1)
          "with fail_fast, only the first (good) action runs to success"
  c8 <- liveCheck $ checkJsonFieldMatches
          "fail_fast · failed ≥ 1"
          ffR "failed" (numberAtLeast 1)
          "the broken deps action must be counted as failed"
  c9 <- liveCheck $ checkJsonFieldMatches
          "fail_fast · skipped ≥ 1"
          ffR "skipped" (numberAtLeast 1)
          "the third action must be skipped once fail_fast trips"
  c10 <- liveCheck $ checkJsonFieldMatches
          "fail_fast · 'fail_fast' flag echoed"
          ffR "fail_fast" (\v -> v == Bool True)
          "response should echo fail_fast=true so consumers know the mode"
  stepFooter 3 t2

  -- The REAL oracle: what does the filesystem say?
  -- F-08: ghc_deps(action="list") without a stanza now emits
  -- @stanzas: {library: [...], "test-suite:NAME": [...]}@; the
  -- legacy @build_depends@ array is only emitted when a stanza
  -- selector is supplied.  Read both shapes for forward compat.
  t3 <- stepHeader 4 "filesystem oracle · ls build_depends after batch"
  ls <- Client.callTool c GhcDeps
          (object [ "action" .= ("list" :: Text) ])
  let deps = case lookupField "build_depends" ls of
        Just (Array xs) -> [ p | String p <- toListVec xs ]
        _ -> case lookupField "stanzas" ls of
          Just (Object o) ->
            [ p
            | (_, Array xs) <- KeyMap.toList o
            , String p <- toListVec xs
            ]
          _ -> []
      hasAeson = any ("aeson" `textInfix`) deps
      hasBS    = any ("bytestring" `textInfix`) deps
  c11 <- liveCheck $ Check
    { cName   = "action #1 landed · 'aeson' is in build_depends"
    , cOk     = hasAeson
    , cDetail = "If 'aeson' is missing, even the FIRST successful \
                \batch action didn't persist — the batch short-circuit \
                \also rolled back earlier successes, or ghc_deps is \
                \broken. build_depends=" <> T.pack (show deps)
    }
  c12 <- liveCheck $ Check
    { cName   = "action #3 SKIPPED · 'bytestring' NOT in build_depends"
    , cOk     = not hasBS
    , cDetail = "If 'bytestring' IS in build_depends the third action \
                \ran despite fail_fast=true and a failed middle action. \
                \The counters in ffR may look right (tool cheats) but \
                \the filesystem doesn't lie. build_depends="
                <> T.pack (show deps)
    }
  stepFooter 4 t3

  pure [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

numberAtLeast :: Int -> Value -> Bool
numberAtLeast n (Number x) = n <= (round x :: Int)
numberAtLeast _ _          = False

toListVec :: V.Vector a -> [a]
toListVec = V.toList

textInfix :: Text -> Text -> Bool
textInfix = T.isInfixOf
