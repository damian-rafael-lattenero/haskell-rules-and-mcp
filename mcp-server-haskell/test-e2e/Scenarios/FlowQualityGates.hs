-- | Flow: quality gates — lint, format, check_module, check_project.
--
-- Sequences the four "is this ready to commit" probes against a
-- tiny project:
--
--   ghci_lint          (HLint: non-failing hints)
--   ghci_format        (formatter presence; graceful if missing)
--   ghci_check_module  (per-module gate rollup)
--   ghci_check_project (every module in the cabal)
module Scenarios.FlowQualityGates
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

calcSrc :: Text
calcSrc =
  "module Calc (greet, double) where\n\
  \\n\
  \greet :: String -> String\n\
  \greet n = \"Hello, \" ++ n\n\
  \\n\
  \double :: Int -> Int\n\
  \double x = x * 2\n"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Calc + load"
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("gates-demo" :: Text) ])
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Calc"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  _ <- Client.callTool c "ghci_load"
         (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- ghci_lint
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghci_lint on src/"
  lintR <- Client.callTool c "ghci_lint"
             (object [ "path" .= ("src/" :: Text) ])
  c1 <- liveCheck $ checkJsonField "lint success" lintR "success" (Bool True)
  c2 <- liveCheck $ checkJsonFieldMatches
          "lint · 'suggestions' array (possibly empty)"
          lintR "suggestions" isArray
          "hlint output parses into the 'suggestions' array"
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- ghci_format — we pass write=false so we just check parsing;
  -- if no formatter is on PATH the tool reports gracefully.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghci_format(src/Calc.hs) — check-only"
  fmtR <- Client.callTool c "ghci_format" (object
    [ "module_path" .= ("src/Calc.hs" :: Text)
    , "write"       .= False
    ])
  c3 <- liveCheck $ checkJsonFieldMatches
          "format returns a structured response"
          fmtR "success" (\case Bool _ -> True; _ -> False)
          "expected a boolean 'success' — true if formatted, or \
          \false-with-hint if no formatter is installed"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- ghci_check_module — per-module gate
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghci_check_module(src/Calc.hs)"
  cmR <- Client.callTool c "ghci_check_module"
           (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  c4 <- liveCheck $ checkJsonField "check_module success" cmR "success" (Bool True)
  c5 <- liveCheck $ checkJsonField "check_module overall=true"
                      cmR "overall" (Bool True)
  c6 <- liveCheck $ checkJsonFieldMatches
          "check_module · gates.compile.ok == true"
          cmR "gates" gateCompileOk
          "the compile gate must be green on a clean source"
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- ghci_check_project — every exposed-module
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "ghci_check_project"
  cpR <- Client.callTool c "ghci_check_project" (object [])
  c7 <- liveCheck $ checkJsonField "check_project success" cpR "success" (Bool True)
  c8 <- liveCheck $ checkJsonField "check_project overall=true"
                      cpR "overall" (Bool True)
  c9 <- liveCheck $ checkJsonFieldMatches
          "check_project · passed ≥ 1"
          cpR "passed" (numberAtLeast 1)
          "at least one module should have been checked"
  c10 <- liveCheck $ checkJsonField "check_project failed=0"
                      cpR "failed" (Number 0)
  stepFooter 5 t4

  pure [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

isArray :: Value -> Bool
isArray (Array _) = True
isArray _         = False

numberAtLeast :: Int -> Value -> Bool
numberAtLeast n (Number x) = n <= (round x :: Int)
numberAtLeast _ _          = False

-- | gates.compile is an object with an 'ok' boolean.
gateCompileOk :: Value -> Bool
gateCompileOk (Object o) = case KeyMap.lookup (Key.fromText "compile") o of
  Just (Object co) -> case KeyMap.lookup (Key.fromText "ok") co of
    Just (Bool True) -> True
    _                -> False
  _ -> False
gateCompileOk _ = False
