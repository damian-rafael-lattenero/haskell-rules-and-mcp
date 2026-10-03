-- | Flow: the F3 parity matrix, promoted from the throwaway probe
-- that caught every ghcide lifecycle bug (queue, dirtyKeys, FOI,
-- mi_top_env, multi-unit flags, mtime rescan).
--
-- Ten verb cases covering the four interactive tool families:
--
--   * ghc_check  — load / module / project
--   * ghc_eval   — pure expression + home-module scope
--   * ghc_inspect(type) — pure + home-module scope
--   * ghc_property(check) — pass / expected-fail / determinism
--
-- The assertions are BACKEND-NEUTRAL contracts (fields + states),
-- so the same scenario passes under the legacy backend and under
-- @HASKELL_FLOWS_BACKEND=ghcide@. When the ghcide backend is
-- active, each interactive response additionally must carry
-- @backend = "ghcide"@ — that pins the route to the real ghcide
-- engine and turns any silent fall-through to legacy into a red
-- check.
--
-- Fixture shape mirrors the proven /tmp/rmprobe matrix project:
-- library @Expr@ with @greet@, plus a QuickCheck-carrying
-- test-suite stanza.
module Scenarios.FlowGhcideMatrix
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import qualified E2E.Assert as Assert
import qualified E2E.Client as Client
import qualified E2E.Envelope as Env
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- flow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Assert.Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) scaffold: lib Expr + test-suite with QuickCheck
  ----------------------------------------------------------------
  t0 <- Assert.stepHeader 1 "scaffold (lib Expr + QC test-suite) + write Expr.hs"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("matrix-demo" :: Text) ])
  _ <- Client.callTool c GhcModule
         (object [ "action" .= ("add" :: Text), "modules" .= (["Expr"] :: [Text]) ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Expr.hs") exprSrc
  Assert.stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) the matrix, one check per case
  ----------------------------------------------------------------
  t1 <- Assert.stepHeader 2 "10-case verb matrix"
  backendGhcide <- (== Just "ghcide") <$> lookupEnv "HASKELL_FLOWS_BACKEND"

  -- ghc_check: load / module / project ----------------------------------
  rLoad <- Client.callTool c GhcCheck
             (object [ "action" .= ("load" :: Text)
                     , "module_path" .= ("src/Expr.hs" :: Text) ])
  c1 <- Assert.liveCheck $ Assert.checkJsonField
          "matrix · check load compiles clean" rLoad "success" (Bool True)

  rMod <- Client.callTool c GhcCheck
            (object [ "action" .= ("module" :: Text)
                    , "module_path" .= ("src/Expr.hs" :: Text) ])
  c2 <- Assert.liveCheck $ Assert.checkJsonField
          "matrix · check module compiles clean" rMod "success" (Bool True)

  rPrj <- Client.callTool c GhcCheck
            (object [ "action" .= ("project" :: Text) ])
  c3 <- Assert.liveCheck $ Assert.checkJsonFieldMatches
          "matrix · check project reports the module set" rPrj "summary"
          (\v -> any (`T.isInfixOf` render v) ["compile clean", "modules compile"])
          "summary should mention clean modules"

  -- ghc_eval: pure + home scope ----------------------------------------
  rEvalPure <- Client.callTool c GhcEval
                 (object [ "expression" .= ("1 + 2" :: Text) ])
  c4 <- Assert.liveCheck $ Assert.checkJsonField
          "matrix · eval pure (1 + 2 = 3)" rEvalPure "output" (String "3")

  rEvalHome <- Client.callTool c GhcEval
                 (object [ "expression" .= ("greet \"w\"" :: Text) ])
  c5 <- Assert.liveCheck $ Assert.checkJsonFieldMatches
          "matrix · eval home scope (greet in scope)" rEvalHome "output"
          (\v -> "hi w" `T.isInfixOf` render v)
          "output should contain the greet result"

  -- ghc_inspect(type): pure + home scope ---------------------------------
  rTyPure <- Client.callTool c GhcInspect
               (object [ "action" .= ("type" :: Text)
                       , "expression" .= ("map (+1)" :: Text) ])
  c6 <- Assert.liveCheck $ Assert.checkJsonFieldMatches
          "matrix · type pure (map (+1))" rTyPure "type"
          (\v -> "Num" `T.isInfixOf` render v && "->" `T.isInfixOf` render v)
          "type should be a Num function type"

  rTyHome <- Client.callTool c GhcInspect
               (object [ "action" .= ("type" :: Text)
                       , "expression" .= ("greet" :: Text) ])
  c7 <- Assert.liveCheck $ Assert.checkJsonField
          "matrix · type home scope (greet :: String -> String)" rTyHome "type"
          (String "String -> String")

  -- ghc_property: pass / expected-fail / determinism ---------------------
  let propPass  = "\\(x :: String) -> length (greet x) >= 0" :: Text
      propFail  = "\\(x :: String) -> length (greet x) < 0" :: Text
  rPropPass <- Client.callTool c GhcProperty (object
    [ "action" .= ("check" :: Text), "property" .= propPass
    , "module"  .= ("src/Expr.hs" :: Text) ])
  c8 <- Assert.liveCheck $ Assert.checkJsonField
          "matrix · property pass" rPropPass "state" (String "passed")

  rPropFail <- Client.callTool c GhcProperty (object
    [ "action" .= ("check" :: Text), "property" .= propFail
    , "module"  .= ("src/Expr.hs" :: Text) ])
  c9 <- Assert.liveCheck $ Assert.checkJsonFieldMatches
          "matrix · property expected-fail + counterexample" rPropFail "state"
          (\v -> render v == "failed")
          "state should be failed"
  c9b <- Assert.liveCheck $ Assert.checkJsonFieldMatches
          "matrix · property fail carries counterexample" rPropFail "counterexample"
          (\v -> not (T.null (T.strip (render v))))
          "counterexample should be non-empty"

  rPropDet <- Client.callTool c GhcProperty (object
    [ "action" .= ("check" :: Text), "property" .= propPass
    , "module"  .= ("src/Expr.hs" :: Text), "runs" .= (3 :: Int) ])
  c10 <- Assert.liveCheck $ Assert.checkJsonField
           "matrix · property determinism (runs=3 stable)" rPropDet "state" (String "passed")
  c10b <- Assert.liveCheck $ Assert.checkJsonField
            "matrix · property determinism stable=true" rPropDet "stable" (Bool True)

  -- ghcide route pin: every interactive response must be served by the
  -- ghcide engine when that backend is active — a missing tag means we
  -- silently fell through to legacy.
  backendChecks <-
    if backendGhcide
      then sequence
        [ pin "check load" rLoad, pin "check module" rMod
        , pin "eval pure" rEvalPure, pin "eval home" rEvalHome
        , pin "type pure" rTyPure, pin "type home" rTyHome
        , pin "prop pass" rPropPass, pin "prop fail" rPropFail
        , pin "prop determinism" rPropDet ]
      else pure []
  Assert.stepFooter 2 t1

  pure ([c1, c2, c3, c4, c5, c6, c7, c8, c9, c9b, c10, c10b] <> backendChecks)
  where
    pin lbl v = Assert.liveCheck $ Assert.checkJsonField
      ("matrix · ghcide route pin — " <> lbl) v "backend" (String "ghcide")
    render v = case v of
      String t -> t
      _        -> T.pack (show v)

exprSrc :: Text
exprSrc = T.unlines
  [ "module Expr where"
  , ""
  , "greet :: String -> String"
  , "greet n = \"hi \" <> n"
  ]
