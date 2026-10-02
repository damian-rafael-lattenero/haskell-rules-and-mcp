-- | Unit tests for logging audit-path, Bench.Budget parsing, Runner
-- warmup-discard, and nextStep Gate chain quality/length/golden dispatch.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.BudgetGate
  ( testLoggingAuditPathPresentWhenEnabled
  , testBudgetParsesCleanly
  , testNextStepGateDWhyQuality
  , testNextStepGateEChainLength
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import System.Environment (setEnv, unsetEnv)

import qualified HaskellFlows.Bench.Budget as Budget
import qualified HaskellFlows.Mcp.Logging as Logging
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..), allToolNames)

import Data.Aeson (Value, object, (.=))
import Data.Maybe (catMaybes, isJust)
import qualified Data.List as List
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

testLoggingAuditPathPresentWhenEnabled :: IO Bool

testLoggingAuditPathPresentWhenEnabled = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hf-audit-test"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  setEnv "HASKELL_FLOWS_AUDIT" "1"
  setEnv "HASKELL_PROJECT_DIR" dir
  ctx <- Logging.newLogContext "ghc_test"
  unsetEnv "HASKELL_FLOWS_AUDIT"
  unsetEnv "HASKELL_PROJECT_DIR"
  removePathForcibly dir
  pure $ case Logging.lcAuditPath ctx of
    Nothing   -> False
    Just path -> ".haskell-flows/audit.jsonl" `List.isSuffixOf` path

------------------------------------------------------------------------
-- Issue #96 Phase A — performance budget scaffold
------------------------------------------------------------------------

-- | Every constructor in 'ToolName' must have an entry in 'Budget.allBudgets'.
-- Catches gaps introduced when a new tool is added to 'ToolName' without
-- a corresponding budget row.

testBudgetParsesCleanly :: IO Bool

testBudgetParsesCleanly =
  pure $ all (isJust . Budget.lookupBudget) allToolNames

-- | No budget value is 0 ms — a zero p50 or p95 would always pass and
-- would be useless as a regression gate.

allDispatchedHints :: [NextStep]

allDispatchedHints = catMaybes $
  -- Default payload — covers the bulk of tools (most have a single
  -- dispatch branch that ignores payload content).
  [ suggestNext t True (object []) | t <- allToolNames ]
  ++
  -- Per-tool payload variants that select alternate branches:
  [ suggestNext GhcGate         True (object ["status" .= ("ok"     :: Text)])
  -- #94 Phase C: determinism (runs>=2) merged into quickcheck.
  , suggestNext GhcQuickCheck   True (object ["runs"   .= (3 :: Int), "status" .= ("ok" :: Text)])
  , suggestNext GhcPropertyStore True (object ["action" .= ("run"    :: Text)])
  , suggestNext GhcPropertyStore True (object ["action" .= ("list"   :: Text)])
  , suggestNext GhcPropertyStore True (object ["files_written" .= (["test/Spec.hs"] :: [Text])])
  , suggestNext GhcPropertyStore True (object ["findings" .= ([] :: [Value])])
  , suggestNext GhcDeps         True (object ["action" .= ("add"    :: Text)])
  , suggestNext GhcDeps         True (object ["action" .= ("remove" :: Text)])
  , suggestNext GhcAddImport    True (object ["count"  .= (3 :: Int)])
  , suggestNext GhcQuickCheck   True (object ["state"  .= ("passed" :: Text)])
  , suggestNext GhcQuickCheck   True (object ["state"  .= ("failed" :: Text)])
  -- GhcLoad: typed-hole path
  , suggestNext GhcLoad True
      (object ["warnings" .=
        [object ["message" .= ("typed hole: _ :: Int" :: Text)]]])
  -- GhcLoad: fixable-warning path (non-hole warning)
  , suggestNext GhcLoad True
      (object ["warnings" .=
        [object ["message" .= ("unused import" :: Text)]]])
  -- GhcProject(action=switch): empty directory (no cabal file → scaffold)
  , suggestNext GhcProject True (object ["scaffolded" .= False])
  -- GhcProject(action=validate): cabal errors present
  , suggestNext GhcProject True (object ["errors" .= (3 :: Int)])
  ]

-- | Gate D: every 'nsWhy' string must be at least 10 characters long
-- and must end with a period ".".  A short or unpunctuated 'why' string
-- is not actionable — it gives the agent too little context to act on.

testNextStepGateDWhyQuality :: IO Bool

testNextStepGateDWhyQuality = pure $
  all checkWhy allDispatchedHints
  where
    checkWhy ns =
      T.length (nsWhy ns) >= 10
      && T.isSuffixOf "." (T.strip (nsWhy ns))

-- | Gate E: every 'nsChain' list (when present) must contain at most 4
-- steps.  Chains longer than 4 steps are overwhelming — the agent should
-- batch large workflows rather than prescribe them up front.

testNextStepGateEChainLength :: IO Bool

testNextStepGateEChainLength = pure $
  all checkChain allDispatchedHints
  where
    checkChain ns = case nsChain ns of
      Nothing -> True
      Just c  -> length c <= 4

------------------------------------------------------------------------
-- Issue #95 Phase C — golden dispatch snapshot
------------------------------------------------------------------------

-- | Golden table: @(description, source tool, payload, expected next tool)@.
-- Captures the dispatch table's behaviour for every meaningful (tool, payload)
-- combination.  A diff against this table signals a deliberate suppression-rule
-- change and must be reviewed before landing.

type GoldenRow = (String, ToolName, Value, Maybe ToolName)

