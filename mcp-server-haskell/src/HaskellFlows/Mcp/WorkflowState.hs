-- | @WorkflowState@ — light session-level tracker the Server
-- updates after every successful tool call. @ghc_workflow@'s
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
    -- * BUG-24 — phase classifier
  , SessionPhase (..)
  , classifyPhase
  , renderPhaseHint
    -- * BUG-08 — history-pattern nudges (exported for testing)
  , historyNudges
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Observable session counters + sliding history. Kept small so
-- JSON serialisation is cheap on every @ghc_workflow(status)@
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
  "ghc_load" | ok ->
    s { wsEditsSinceLastLoad = 0
      , wsLastLoadSuccess    = Just True
      , wsLastLoadWarnings   = warningCount payload
      }
  "ghc_load" ->
    s { wsLastLoadSuccess = Just False }
  "ghc_refactor" ->
    s { wsEditsSinceLastLoad = wsEditsSinceLastLoad s + 1 }
  "ghc_quickcheck" | ok, isPassed payload ->
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
--
-- BUG-08: now also inspects 'wsToolHistory' to catch *patterns*
-- the scalar counters miss — e.g. N consecutive @ghc_load@
-- calls (polling without progress), a @ghc_suggest@ that was
-- never followed by @ghc_quickcheck@, a @ghc_refactor@ that
-- was never re-loaded.
renderHelp :: WorkflowState -> [Text]
renderHelp s = concat
  [ -- Counter-based nudges (pre-BUG-08 behaviour).
    [ "You have " <> tshow (wsEditsSinceLastLoad s)
      <> " edits since the last ghc_load — recompile to see fresh \
      \diagnostics."
    | wsEditsSinceLastLoad s >= 3
    ]
  , [ "Last ghc_load reported " <> tshow (wsLastLoadWarnings s)
      <> " warnings — fix or ghc_fix_warning before moving on."
    | wsLastLoadWarnings s > 0
    ]
  , [ "Last ghc_load failed — inspect errors before doing anything else."
    | wsLastLoadSuccess s == Just False
    ]
  , [ "You've persisted " <> tshow (wsPassedProperties s) <> " \
      \passing properties in this session. Consider \
      \ghc_regression(action=\"run\") to confirm none regressed, \
      \then ghc_quickcheck_export to materialise them as a \
      \test/Spec.hs."
    | wsPassedProperties s >= 3
    ]
    -- BUG-08: history-pattern nudges.
  , historyNudges (wsToolHistory s)
  ]
  where
    tshow :: Int -> Text
    tshow = T.pack . show

-- | Pattern-based nudges derived from the sliding tool-call
-- history. Each case inspects a small prefix (typically 3..5
-- entries) and looks for a known anti-pattern or an obvious
-- missed follow-up.
historyNudges :: [Text] -> [Text]
historyNudges hist = concat
  [ -- 5 consecutive ghc_load calls → the agent is polling
    -- rather than editing. Suggest a flakiness / stability
    -- check instead of another reload.
    [ "The last 5 tool calls were all ghc_load — you're polling \
      \rather than progressing. Try ghc_determinism on a recent \
      \property for flakiness, or ghc_check_project to surface \
      \module-level gates you can knock out in parallel."
    | length recent5 >= 5, all (== "ghc_load") recent5
    ]
    -- ghc_suggest recent, no ghc_quickcheck since.
  , [ "You ran ghc_suggest but haven't tried any of the proposals \
      \with ghc_quickcheck yet. Pick the highest-confidence law \
      \and feed it in — passes auto-persist to the regression store."
    | "ghc_suggest" `elem` recent3, "ghc_quickcheck" `notElem` recent3
    ]
    -- Last tool was ghc_refactor and there's been no load since.
  , [ "Last tool was ghc_refactor. The refactor was snapshot-and-\
      \compile-verified, but a fresh ghc_load(diagnostics=true) \
      \catches any new holes or warnings the rename surfaced."
    | case hist of
        ("ghc_refactor" : rest) -> "ghc_load" `notElem` take 2 rest
        _                        -> False
    ]
  ]
  where
    recent3 = take 3 hist
    recent5 = take 5 hist

-- | BUG-24: session phase classifier. Given the counters, decide
-- which "phase" the project is in and return a hint tailored to
-- that phase. Phases are coarse but cover the dominant flows —
-- the agent gets a pointer at the most probable next *flow*, not
-- just the next tool.
data SessionPhase
  = PhasePreScaffold       -- ^ No successful load yet; likely pre-scaffold.
  | PhaseBootstrap         -- ^ Scaffold done, first load needs deps + modules.
  | PhaseDeveloping        -- ^ Modules loaded, iterating on code.
  | PhaseTestingLaws       -- ^ Iterating on properties.
  | PhaseReadyToPush       -- ^ Several passing properties; ready for gate.
  deriving stock (Eq, Show)

-- | Classify the session based purely on the counters. Deliberately
-- coarse; never throws.
classifyPhase :: WorkflowState -> SessionPhase
classifyPhase s
  | isNothing (wsLastLoadSuccess s)
      && wsToolCalls s < 3              = PhasePreScaffold
  | wsLastLoadSuccess s == Just False   = PhaseBootstrap
  | wsPassedProperties s >= 3           = PhaseReadyToPush
  | "ghc_quickcheck" `elem` recent3
      || "ghc_suggest"   `elem` recent3 = PhaseTestingLaws
  | otherwise                           = PhaseDeveloping
  where
    recent3 = take 3 (wsToolHistory s)

-- | Render a phase-specific follow-up. Returned as a short
-- paragraph so the @ghc_workflow(help)@ view can concatenate it
-- next to the counter-based nudges.
renderPhaseHint :: SessionPhase -> Text
renderPhaseHint p = case p of
  PhasePreScaffold ->
    "Phase: pre-scaffold. If this is a new project, start with \
    \ghc_create_project(name=...); if you already have one, \
    \ghc_load(module_path=\"src/<Entry>.hs\") boots GHCi and \
    \gives you the cleanest error surface."
  PhaseBootstrap ->
    "Phase: bootstrap. The last load failed — likely a missing \
    \dependency or an unregistered module. Chain ghc_deps(add,...) \
    \+ ghc_add_modules(modules=[...]) + ghc_load; ghc_batch can \
    \run the three as one round-trip."
  PhaseDeveloping ->
    "Phase: developing. Modules load clean. ghc_suggest on a \
    \recently-edited binding gives you QuickCheck candidates; \
    \names that hint at normalisation (simplify / normalize / fold) \
    \automatically bump the evaluator-preservation law to High \
    \confidence if a paired interpreter is a sibling."
  PhaseTestingLaws ->
    "Phase: testing laws. Feed the highest-confidence proposal from \
    \ghc_suggest into ghc_quickcheck; every pass auto-persists \
    \to .haskell-flows/properties.json. Use ghc_determinism to \
    \check stability before adding to the regression suite."
  PhaseReadyToPush ->
    "Phase: ready to push. ghc_regression(action=\"run\") replays \
    \the full set; ghc_quickcheck_export materialises them as \
    \test/Spec.hs; ghc_gate runs regression + cabal test + cabal \
    \build in one call — if green, push is safe."
