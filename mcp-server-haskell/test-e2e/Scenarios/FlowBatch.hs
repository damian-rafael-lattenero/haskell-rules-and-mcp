-- | Flow: composition via @ghci_batch@.
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
--   ghci_batch  (composition primitive)
-- Indirectly every tool that appears as a sub-action.
module Scenarios.FlowBatch
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

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
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("batch-demo" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) Happy composition: 3 legit actions in order.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghci_batch(3 happy actions)"
  let happyActions =
        [ object
            [ "tool" .= ("ghci_add_modules" :: Text)
            , "args" .= object
                [ "modules" .= (["Foo", "Bar"] :: [Text]) ]
            ]
        , object
            [ "tool" .= ("ghci_deps" :: Text)
            , "args" .= object
                [ "action"  .= ("add" :: Text)
                , "package" .= ("text" :: Text)
                , "stanza"  .= ("library" :: Text)
                ]
            ]
        , object
            [ "tool" .= ("ghci_workflow" :: Text)
            , "args" .= object [ "action" .= ("status" :: Text) ]
            ]
        ]
  happyR <- Client.callTool c "ghci_batch"
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
  -- (2) Fail-fast: second action is broken (invalid package
  -- name rejects at the boundary validator). With the default
  -- fail_fast=true, action #3 must surface as skipped.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghci_batch(fail_fast=true on bad middle action)"
  let ffActions =
        [ object
            [ "tool" .= ("ghci_workflow" :: Text)
            , "args" .= object [ "action" .= ("status" :: Text) ]
            ]
        , object
            [ "tool" .= ("ghci_deps" :: Text)
            , "args" .= object
                [ "action"  .= ("add" :: Text)
                , "package" .= ("this has spaces, clearly invalid!" :: Text)
                , "stanza"  .= ("library" :: Text)
                ]
            ]
        , object
            [ "tool" .= ("ghci_workflow" :: Text)
            , "args" .= object [ "action" .= ("help" :: Text) ]
            ]
        ]
  ffR <- Client.callTool c "ghci_batch" (object
    [ "actions"   .= ffActions
    , "fail_fast" .= True
    ])
  c6 <- liveCheck $ checkJsonField
          "fail_fast · overall success is false"
          ffR "success" (Bool False)
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

  pure [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

numberAtLeast :: Int -> Value -> Bool
numberAtLeast n (Number x) = n <= (round x :: Int)
numberAtLeast _ _          = False

_unused :: KeyMap.KeyMap Value -> Key.Key
_unused _ = Key.fromText ""
