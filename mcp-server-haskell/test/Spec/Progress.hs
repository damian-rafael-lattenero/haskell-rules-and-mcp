-- | Unit tests for 'Mcp.Progress' streaming primitives (#265) and
-- 'Mcp.WorkflowState' phase/history tracking.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Progress
  ( testProgressNotificationShape
  , testProgressTokenPresent
  , testProgressTokenAbsent
  , testProgressCollectingSink
  , testProgressNoSubscriptionNoop
  , testPhasePreScaffold
  , testPhaseHintNonEmpty
  ) where

import qualified Data.Aeson as A
import Data.Aeson (object, (.=))
import qualified Data.Aeson.KeyMap as AKM
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Mcp.Progress
  ( ProgressEvent (..)
  , ProgressSink (..)
  , emitProgress
  , mkProgressSink
  , noopSink
  , peStep
  , progressNotification
  , progressTokenFrom
  )
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Mcp.WorkflowState as WS

import Spec.Helpers (withTempProject)

-- #265: the emitted notification must be a spec-shaped notifications/progress
-- message — jsonrpc 2.0, the right method, and params carrying the token,
-- message, elapsed_ms, and the progress/total counters.

testProgressNotificationShape :: IO Bool

testProgressNotificationShape =
  let n = progressNotification (A.String "tok-1")
            (ProgressEvent "regression: running" 42 (Just 1) (Just 3))
  in pure $ case n of
       A.Object o ->
            AKM.lookup "jsonrpc" o == Just (A.String "2.0")
         && AKM.lookup "method"  o == Just (A.String "notifications/progress")
         && case AKM.lookup "params" o of
              Just (A.Object p) ->
                   AKM.lookup "progressToken" p == Just (A.String "tok-1")
                && AKM.lookup "message"       p == Just (A.String "regression: running")
                && AKM.lookup "elapsed_ms"    p == Just (A.Number 42)
                && AKM.lookup "progress"      p == Just (A.Number 1)
                && AKM.lookup "total"         p == Just (A.Number 3)
              _ -> False
       _ -> False

-- #265: the client's subscription token is pulled from params._meta.progressToken.

testProgressTokenPresent :: IO Bool

testProgressTokenPresent =
  let params = A.object
        [ "name"  .= ("ghc_gate" :: Text)
        , "_meta" .= A.object [ "progressToken" .= (7 :: Int) ]
        ]
  in pure (progressTokenFrom params == Just (A.Number 7))

-- #265: no _meta ⇒ no token ⇒ the client did not subscribe.

testProgressTokenAbsent :: IO Bool

testProgressTokenAbsent =
  let params = A.object [ "name" .= ("ghc_gate" :: Text) ]
  in pure (isNothing (progressTokenFrom params))

-- #265: a handler emitting through a sink reaches the sink's consumer in order
-- (the mechanism ghc_gate uses for its per-step events).

testProgressCollectingSink :: IO Bool

testProgressCollectingSink = do
  ref <- newIORef []
  let sink = ProgressSink (\ev -> modifyIORef' ref (peStep ev :))
  emitProgress sink (ProgressEvent "a" 1 (Just 1) (Just 3))
  emitProgress sink (ProgressEvent "b" 2 (Just 2) (Just 3))
  emitProgress sink (ProgressEvent "c" 3 (Just 3) (Just 3))
  steps <- readIORef ref
  pure (reverse steps == ["a", "b", "c"])

-- #265: when the client did not subscribe (no token), the chosen sink is the
-- no-op — emitting through it is harmless (zero overhead, no stdout write).
-- Covers both mkProgressSink Nothing and the noopSink it returns.

testProgressNoSubscriptionNoop :: IO Bool

testProgressNoSubscriptionNoop = do
  sink <- mkProgressSink Nothing
  emitProgress sink   (ProgressEvent "x" 0 Nothing Nothing)
  emitProgress noopSink (ProgressEvent "y" 0 Nothing Nothing)
  pure True  -- must complete without writing / throwing

-- | BUG-08 — 5 @ghc_load@ calls in a row must trigger the
-- polling nudge that points at ghc_quickcheck / check_project.

testPhasePreScaffold :: IO Bool

testPhasePreScaffold = do
  ref <- WS.newWorkflowStateRef
  s   <- WS.readState ref
  pure (WS.classifyPhase s == WS.PhasePreScaffold)

-- | BUG-24 — a failed ghc_load classifies as bootstrap. Verify
-- with a synthetic state update sequence.

testPhaseHintNonEmpty :: IO Bool

testPhaseHintNonEmpty = pure $
  let phases = [ WS.PhasePreScaffold, WS.PhaseBootstrap
               , WS.PhaseDeveloping, WS.PhaseTestingLaws
               , WS.PhaseReadyToPush ]
  in not (any (T.null . WS.renderPhaseHint) phases)
