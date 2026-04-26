-- | Flow: @ghc_coverage@ — @cabal test --enable-coverage@ + HPC parse.
--
-- SLOW: spawns a full cabal test cycle; runtime ~30 s on a warm
-- cache, longer on cold. Skip-friendly: the tool itself has a
-- 5-min internal cap and returns a structured result even when
-- cabal bails.
--
-- Scenario covers three paths:
--
--   (1) Happy path — passing Spec.hs → success + metrics populated
--       on most platforms (HPC occasionally fails to find the
--       coverage dir on constrained CI runners, so we accept a
--       graceful-failure hint too).
--   (2) Failing test suite — Spec.hs with @exitFailure@ → the tool
--       must surface success=false, NOT swallow the failure or
--       claim green. This is the real oracle: a previous version of
--       the scenario only pinned structural shape and would have
--       passed even if the tool silently reported success on a
--       failing cabal test. Planted-defect approach.
--   (3) Empty project — no Spec.hs at all → cabal test has nothing
--       to run. The tool must respond structurally (success=false
--       with hint/error), not crash or hang.
module Scenarios.FlowCoverage
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonFieldMatches
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

calcSrc :: Text
calcSrc =
  "module Calc (double) where\n\
  \\n\
  \double :: Int -> Int\n\
  \double x = x * 2\n"

specSrcPassing :: Text
specSrcPassing = T.unlines
  [ "module Main where"
  , ""
  , "import Calc (double)"
  , "import System.Exit (exitSuccess, exitFailure)"
  , ""
  , "main :: IO ()"
  , "main = if double 21 == 42 then exitSuccess else exitFailure"
  ]

-- | Deliberately-failing spec. The planted defect: double 21 is
-- 42, so @== 43@ is false, so we hit exitFailure. Used by step (2)
-- to prove the tool surfaces a failing cabal test as success=false.
specSrcFailing :: Text
specSrcFailing = T.unlines
  [ "module Main where"
  , ""
  , "import Calc (double)"
  , "import System.Exit (exitSuccess, exitFailure)"
  , ""
  , "main :: IO ()"
  , "main = if double 21 == 43 then exitSuccess else exitFailure"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup — scaffold + module + trivial test
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Calc + Spec.hs"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("coverage-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Calc"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  createDirectoryIfMissing True (projectDir </> "test")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  TIO.writeFile (projectDir </> "test" </> "Spec.hs") specSrcPassing
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) Happy path — cabal test with a passing Spec.
  --
  -- Coverage is brittle on some CI runners (cabal flags pick the
  -- wrong package component, HPC dir heuristics need full
  -- dist-newstyle); we accept either "metrics[] populated" OR
  -- "success=false + hint" as the happy outcome. What we do NOT
  -- accept is structural-only success — the previous oracle
  -- ("success is a Bool") passed on literally any response shape.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "happy · passing Spec.hs"
  r <- Client.callTool c GhcCoverage (object [])
  c1 <- liveCheck $ checkJsonFieldMatches
          "coverage returns a structured payload"
          r "success" (\case Bool _ -> True; _ -> False)
          "success must be a Bool; either metrics[] is populated \
          \or a 'hint' explains why HPC didn't come together"
  c2 <- liveCheck $ checkPure
          "coverage · payload carries 'metrics' / 'hint' / 'error'"
          (hasField "metrics" r || hasField "hint" r || hasField "error" r)
          "the response should carry at least one of metrics[], a \
          \graceful-failure hint, or an error string"
  -- Real oracle: if success=true, metrics[] MUST have content.
  -- The previous scenario would report green on an empty metrics
  -- array as long as the shape was right.
  let happySuccess = fieldBool "success" r == Just True
      metricsArr = case lookupField "metrics" r of
                     Just (Array xs) -> xs
                     _               -> V.empty
  c3 <- liveCheck $ checkPure
          "happy · success=true iff metrics[] non-empty"
          (not happySuccess || not (V.null metricsArr))
          ("A success=true coverage call with no metrics is the \
           \shape that would have silently slipped past the old \
           \structural oracle. metrics.length=" <>
           T.pack (show (V.length metricsArr)))
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (2) Failing test suite — deliberate defect: the Spec now
  -- returns exitFailure. If the tool tried to hide this (e.g.
  -- swallowing the nonzero exit, reporting metrics anyway) we'd
  -- never know. The oracle: success MUST be false here.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "failing · cabal test exits nonzero"
  TIO.writeFile (projectDir </> "test" </> "Spec.hs") specSrcFailing
  r2 <- Client.callTool c GhcCoverage (object [])
  let failSuccess = fieldBool "success" r2
  c4 <- liveCheck $ checkPure
          "failing · success=false (cabal test's exitFailure surfaced)"
          (failSuccess == Just False)
          ("A failing test suite must surface as success=false, not \
           \be silently swallowed. Got success=" <>
           T.pack (show failSuccess) <> ". Raw: " <> renderShort r2)
  c5 <- liveCheck $ checkPure
          "failing · response carries diagnostic (error / hint / exit_code)"
          ( hasField "error" r2
            || hasField "hint" r2
            || hasField "exit_code" r2
            || hasField "stderr" r2
          )
          ("A failure payload must name the failure mode — not just \
           \a bare success=false with no diagnostic. The LLM agent \
           \needs something to route on. Raw: " <> renderShort r2)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (3) Empty project — no test suite on disk. cabal test has
  -- nothing to run. The tool MUST return a structured response;
  -- it must NOT crash, hang, or report green.
  --
  -- Implementation detail: removing Spec.hs from disk is enough;
  -- cabal will either fail to build the test-suite stanza or fail
  -- to find Main. Both flow into the same "cabal test nonzero"
  -- response shape our tool should handle.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "empty · no Spec.hs on disk"
  let specPath = projectDir </> "test" </> "Spec.hs"
  -- Replace with a file that won't compile (references undefined
  -- module) — indistinguishable from "no test suite" at the
  -- cabal-test exit-code layer, and easier to produce reliably
  -- than juggling cabal file surgery.
  TIO.writeFile specPath
    "module Main where\nimport DoesNotExist\nmain = undefined\n"
  r3 <- Client.callTool c GhcCoverage (object [])
  c6 <- liveCheck $ checkPure
          "empty · structured response on unbuildable test-suite"
          (case fieldBool "success" r3 of
             Just _  -> True   -- any success Bool is structured
             Nothing -> False)
          ("The tool must always return a structured response with \
           \a Bool 'success', even when cabal test can't build the \
           \test-suite. Raw: " <> renderShort r3)
  c7 <- liveCheck $ checkPure
          "empty · success is NOT claimed when the test-suite is broken"
          (fieldBool "success" r3 /= Just True)
          ("Build-broken test-suite reported success=true. The tool \
           \is papering over a real failure. Raw: " <> renderShort r3)
  stepFooter 4 t3

  pure [c1, c2, c3, c4, c5, c6, c7]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

hasField :: Text -> Value -> Bool
hasField k (Object o) = KeyMap.member (Key.fromText k) o
hasField _ _          = False

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (Bool b) -> Just b
  _             -> Nothing
fieldBool _ _ = Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

renderShort :: Value -> Text
renderShort v =
  let s = T.pack (show v)
  in if T.length s > 300 then T.take 300 s <> "…" else s

_v :: V.Vector Value
_v = V.empty
