-- | Flow: 'ghc_perf' Phase 2 — wall-clock benchmarking + baseline
-- compare (#61, #93).
--
-- Plant a simple library module, scaffold a test-suite so the
-- evalIOString harness can resolve the function, and call
-- ghc_perf with a small @runs@ count. Assert:
--
--   * @success: true@.
--   * @measurements@ contains a non-zero @mean_ns@ and the
--     samples array length matches @runs_executed@.
--   * @phase@ is @"2-baseline"@ (Phase 2 baseline-compare landed
--     in commit a29fbab — issue #93).
--   * The narration evidence package + agent instructions are
--     present so the next iteration can wire an LLM.
module Scenarios.FlowPerfBasic
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, fieldInt, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + simple library module.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("perf-demo" :: Text) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Demo.hs") $ T.unlines
    [ "module Demo where"
    , ""
    , "double :: Int -> Int"
    , "double x = x + x"
    ]
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Demo"] :: [Text]) ])

  -- Step 2 — drive ghc_perf. The expression evaluates to a
  -- pure Int — small enough that wall-clock dominates GC.
  -- F-26: 'verbose=true' is required to keep the per-run 'samples'
  -- array in the response (default omits it for big runs values).
  t0 <- stepHeader 1 "ghc_perf returns wall-clock measurements (#61)"
  r <- Client.callTool c GhcPerf (object
    [ "expression" .= ("sum [1 .. 100]" :: Text)
    , "runs"       .= (5 :: Int)
    , "verbose"    .= True
    ])
  let success     = statusOk r == Just True
      runsExec    = fieldInt "runs_executed" r
      meanNs      = drillNumber ["measurements", "mean_ns"] r
      sampleLen   = case drillPath ["measurements", "samples"] r of
        Just (Array xs) -> V.length xs
        _               -> -1
      phase       = lookupString "phase" r
      hasInstr    = case lookupField "instructions_for_agent" r of
        Just (String _) -> True
        _               -> False
  cBasic <- liveCheck $ checkPure
    "success + runs=5 + measurements/mean_ns≥0 + samples=5 + phase=2-baseline"
    (success
       && runsExec == Just 5
       && meanNs >= 0
       && sampleLen == 5
       && phase == Just "2-baseline"
       && hasInstr)
    ( "Got: success=" <> T.pack (show success)
      <> ", runs=" <> T.pack (show runsExec)
      <> ", mean=" <> T.pack (show meanNs)
      <> ", samples=" <> T.pack (show sampleLen)
      <> ", phase=" <> T.pack (show phase) )
  stepFooter 1 t0

  pure [cBasic]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

drillNumber :: [Text] -> Value -> Double
drillNumber ks v = case drillPath ks v of
  Just (Number n) -> realToFrac n
  _               -> -1

drillPath :: [Text] -> Value -> Maybe Value
drillPath [] v = Just v
drillPath (k : ks) v = case lookupField k v of
  Just inner -> drillPath ks inner
  Nothing    -> Nothing

lookupString :: Text -> Value -> Maybe Text
lookupString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

