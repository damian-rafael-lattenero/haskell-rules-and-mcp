-- | Flow: @ghc_workflow(help|next)@ — state-aware guidance.
--
-- Drives the server through a sequence of state-changing tool
-- calls, then asks @ghc_workflow(help)@ + @ghc_workflow(next)@
-- for a state-aware nudge. Pins that:
--
--   * 'help' carries a non-empty @steps@ list + a 'phase' field
--     (BUG-24 phase classifier).
--   * 'next' returns a single concrete tool name.
--   * After ≥3 passing quickcheck calls, the 'help' payload's
--     'stateHints' mentions 'regression' (BUG-08's history-aware
--     nudge — "you've persisted N passing properties, consider
--     running the regression").
module Scenarios.FlowWorkflowHelp
  ( runFlow
  ) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

calcSrc :: Text
calcSrc = T.unlines
  [ "module Calc where"
  , ""
  , "double :: Int -> Int"
  , "double x = x * 2"
  , ""
  , "triple :: Int -> Int"
  , "triple x = x * 3"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- help at t=0 (no state yet → phase = PhasePreScaffold)
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "help at t=0 — pre-scaffold phase"
  r0 <- Client.callTool c GhcWorkflow (object [ "action" .= ("help" :: Text) ])
  c1 <- liveCheck $ checkJsonFieldMatches
          "help has a 'steps' array"
          r0 "steps" isNonEmptyArray
          "steps[] should list the first-call recipe"
  c2 <- liveCheck $ checkJsonFieldMatches
          "help reports phase = PhasePreScaffold"
          r0 "phase" (stringIs "PhasePreScaffold")
          "with no successful load, phase must be PhasePreScaffold (BUG-24)"
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- advance state: scaffold + load + add 3 passing properties
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "advance state: scaffold + load + 3 passing props"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("workflow-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Calc"] :: [Text]) ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  -- 3 distinct passing properties to cross the wsPassedProperties
  -- >= 3 threshold in WorkflowState.renderHelp.
  let props =
        [ "\\(x :: Int) -> double x == x + x"
        , "\\(x :: Int) -> triple x == x * 3"
        , "\\(x :: Int) -> double (double x) == triple x + x"
        ]
  forM_ props $ \p ->
    Client.callTool c GhcQuickCheck (object
      [ "property" .= (p :: Text)
      , "module"   .= ("src/Calc.hs" :: Text)
      ])
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- help at t=N (≥3 passing props → state hint mentions regression)
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "help after ≥3 passing props — history nudge"
  rN <- Client.callTool c GhcWorkflow (object [ "action" .= ("help" :: Text) ])
  c3 <- liveCheck $ checkJsonFieldMatches
          "help now reports a non-preScaffold phase"
          rN "phase" (not . stringIs "PhasePreScaffold")
          "after successful loads + quickchecks, phase must advance"
  c4 <- liveCheck $ checkJsonFieldMatches
          "help surfaces 'stateHints' once thresholds trip (BUG-08)"
          rN "stateHints" (hintsMention "regression")
          "with 3+ passing properties, WorkflowState.renderHelp emits a \
          \'consider ghc_regression(action=\"run\")' nudge"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- next — single tool recommendation
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "workflow(next) — single next tool"
  rNext <- Client.callTool c GhcWorkflow (object [ "action" .= ("next" :: Text) ])
  c5 <- liveCheck $ checkJsonFieldMatches
          "next · payload carries a 'tool' string"
          rNext "tool" isString
          "next should be a concrete tool name, not a structured object"
  c6 <- liveCheck $ checkJsonFieldMatches
          "next · payload carries a 'why' rationale"
          rNext "why" isString
          "every nextStep-shaped recommendation must justify itself"
  stepFooter 4 t3

  pure [c1, c2, c3, c4, c5, c6]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

isNonEmptyArray :: Value -> Bool
isNonEmptyArray (Array a) = not (V.null a)
isNonEmptyArray _         = False

isString :: Value -> Bool
isString (String _) = True
isString _          = False

stringIs :: Text -> Value -> Bool
stringIs t (String s) = s == t
stringIs _ _          = False

-- | Does the 'stateHints' array contain a string entry that
-- mentions the needle?
hintsMention :: Text -> Value -> Bool
hintsMention needle (Array a) =
  any (\case String s -> needle `T.isInfixOf` s
             _        -> False)
      (V.toList a)
hintsMention _ _ = False
