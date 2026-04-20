-- | @WorkflowState@ — light session-level tracker the Server
-- updates after every successful tool call. @ghci_workflow@'s
-- @help@ action reads this state to render context-aware advice
-- ("you edited 4 times since last load, recompile"; "regression
-- has N saved properties, consider running it") instead of a
-- static string.
--
-- Intentionally narrow: 5 counters + a sliding tool history. The
-- TS MCP had per-module state; we keep per-session state for now
-- and grow to per-module when a real use case demands it. State
-- is held in an 'MVar' at 'Server' construction and updated via
-- 'trackTool' — no thread-unsafe read-modify-write.
module HaskellFlows.Mcp.WorkflowState
  ( WorkflowState (..)
  , WorkflowStateRef
  , newWorkflowStateRef
  , trackTool
  , readState
  , renderHelp
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T

-- | Observable session counters + sliding history. Kept small so
-- JSON serialisation is cheap on every @ghci_workflow(status)@
-- call.
data WorkflowState = WorkflowState
  { wsToolCalls          :: !Int
  , wsEditsSinceLastLoad :: !Int
  , wsLastLoadSuccess    :: !(Maybe Bool)
  , wsLastLoadWarnings   :: !Int
  , wsPassedProperties   :: !Int
  , wsToolHistory        :: ![Text]   -- ^ most recent first, bounded at 'historyLimit'
  }
  deriving stock (Eq, Show)

instance ToJSON WorkflowState where
  toJSON s = object
    [ "toolCalls"          .= wsToolCalls s
    , "editsSinceLastLoad" .= wsEditsSinceLastLoad s
    , "lastLoadSuccess"    .= wsLastLoadSuccess s
    , "lastLoadWarnings"   .= wsLastLoadWarnings s
    , "passedProperties"   .= wsPassedProperties s
    , "toolHistory"        .= wsToolHistory s
    ]

-- | Handle used by the Server layer to mutate the state from
-- concurrent tool handlers safely.
newtype WorkflowStateRef = WorkflowStateRef (MVar WorkflowState)

newWorkflowStateRef :: IO WorkflowStateRef
newWorkflowStateRef = WorkflowStateRef <$> newMVar initial
  where
    initial = WorkflowState
      { wsToolCalls          = 0
      , wsEditsSinceLastLoad = 0
      , wsLastLoadSuccess    = Nothing
      , wsLastLoadWarnings   = 0
      , wsPassedProperties   = 0
      , wsToolHistory        = []
      }

historyLimit :: Int
historyLimit = 20

-- | Update the state based on a just-finished tool invocation.
-- The payload is the tool's result JSON; we look at well-known
-- fields to derive counters. Unknown payloads only bump the
-- tool-call counter + history — we never fail.
trackTool :: WorkflowStateRef -> Text -> Bool -> Value -> IO ()
trackTool (WorkflowStateRef ref) toolName ok payload =
  modifyMVar_ ref $ \s ->
    let calls  = wsToolCalls s + 1
        hist   = take historyLimit (toolName : wsToolHistory s)
        base   = s { wsToolCalls = calls, wsToolHistory = hist }
    in pure (applyToolUpdate base toolName ok payload)

applyToolUpdate :: WorkflowState -> Text -> Bool -> Value -> WorkflowState
applyToolUpdate s toolName ok payload = case toolName of
  "ghci_load" | ok ->
    s { wsEditsSinceLastLoad = 0
      , wsLastLoadSuccess    = Just True
      , wsLastLoadWarnings   = warningCount payload
      }
  "ghci_load" ->
    s { wsLastLoadSuccess = Just False }
  "ghci_refactor" ->
    s { wsEditsSinceLastLoad = wsEditsSinceLastLoad s + 1 }
  "ghci_quickcheck" | ok, isPassed payload ->
    s { wsPassedProperties = wsPassedProperties s + 1 }
  _ -> s

--------------------------------------------------------------------------------
-- payload probes
--------------------------------------------------------------------------------

warningCount :: Value -> Int
warningCount (Object o) = case KeyMap.lookup (Key.fromText "warnings") o of
  Just (Array a) -> length a
  _              -> 0
warningCount _ = 0

isPassed :: Value -> Bool
isPassed (Object o) = case KeyMap.lookup (Key.fromText "state") o of
  Just (String s) -> s == "passed"
  _               -> False
isPassed _ = False

--------------------------------------------------------------------------------
-- state-aware help
--------------------------------------------------------------------------------

readState :: WorkflowStateRef -> IO WorkflowState
readState (WorkflowStateRef ref) = readMVar ref

-- | Turn the state into a short human-readable nudge list. Empty
-- list means "nothing urgent".
renderHelp :: WorkflowState -> [Text]
renderHelp s = concat
  [ [ "You have " <> tshow (wsEditsSinceLastLoad s)
      <> " edits since the last ghci_load — recompile to see fresh \
      \diagnostics."
    | wsEditsSinceLastLoad s >= 3
    ]
  , [ "Last ghci_load reported " <> tshow (wsLastLoadWarnings s)
      <> " warnings — fix or ghci_fix_warning before moving on."
    | wsLastLoadWarnings s > 0
    ]
  , [ "Last ghci_load failed — inspect errors before doing anything else."
    | wsLastLoadSuccess s == Just False
    ]
  , [ "You've persisted " <> tshow (wsPassedProperties s) <> " \
      \passing properties in this session. Consider \
      \ghci_regression(action=\"run\") to confirm none regressed, \
      \then ghci_quickcheck_export to materialise them as a \
      \test/Spec.hs."
    | wsPassedProperties s >= 3
    ]
  ]
  where
    tshow :: Int -> Text
    tshow = T.pack . show
