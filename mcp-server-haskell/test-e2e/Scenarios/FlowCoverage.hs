-- | Flow: @ghci_coverage@ — @cabal test --enable-coverage@ + HPC parse.
--
-- SLOW: spawns a full cabal test cycle; runtime ~30 s on
-- a warm cache, longer on cold. Skip-friendly: the tool itself
-- has a 5-min internal cap and returns a structured result even
-- when cabal bails.
--
-- Flow: scaffold + add QuickCheck + write a trivial module +
-- a trivial Spec.hs that exercises it + ghci_coverage.
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

calcSrc :: Text
calcSrc =
  "module Calc (double) where\n\
  \\n\
  \double :: Int -> Int\n\
  \double x = x * 2\n"

specSrc :: Text
specSrc = T.unlines
  [ "module Main where"
  , ""
  , "import Calc (double)"
  , "import System.Exit (exitSuccess, exitFailure)"
  , ""
  , "main :: IO ()"
  , "main = if double 21 == 42 then exitSuccess else exitFailure"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup — scaffold + module + trivial test
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Calc + Spec.hs"
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("coverage-demo" :: Text) ])
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Calc"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  createDirectoryIfMissing True (projectDir </> "test")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  TIO.writeFile (projectDir </> "test" </> "Spec.hs") specSrc
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- ghci_coverage — runs cabal test --enable-coverage
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghci_coverage (slow, runs cabal test)"
  r <- Client.callTool c "ghci_coverage" (object [])
  -- Coverage is brittle on some CI runners (cabal flags pick
  -- the wrong package component, HPC dir heuristics need full
  -- dist-newstyle); we pin the structured shape only.
  -- 'success' is true iff hpc report parsed AND at least one
  -- metric came out; false + hint is an acceptable path in
  -- constrained environments.
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
  stepFooter 2 t1

  pure [c1, c2]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

hasField :: Text -> Value -> Bool
hasField k (Object o) = KeyMap.member (Key.fromText k) o
hasField _ _          = False

_v :: V.Vector Value
_v = V.empty
