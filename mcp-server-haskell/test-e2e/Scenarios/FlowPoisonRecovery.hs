-- | Flow: session poison-recovery — every session-dropping event
-- leaves the next ghcide call on a freshly booted session (W7).
--
-- Contract pinned
-- ---------------
-- Since ApiSession died (W6.8.3+4) the 'MVar (Maybe IdeSession)' is
-- the only backend slot, and three events recycle it:
--
--   * @ghc_modules(action=add)@ / @ghc_deps(add|remove)@ /
--     @ghc_project(action=create)@ call 'IdeBacked.dropIdeSession'
--     after mutating the @.cabal@ (the cradle must re-resolve);
--   * @ghc_project(action=switch)@ swaps the slot to 'Nothing';
--   * the 'runTool' exception / outer-timeout shield drops it.
--
-- A botched drop (slot left empty, or a half-dead session left in the
-- slot) wedges EVERY later ghcide call — the 2b78e6e infinite-block
-- class. This scenario recycles the slot three times end-to-end and
-- proves each successor call boots and answers:
--
--   boot A → deps add   → boot B (sees the new dep graph)
--         → deps remove → boot C (back to the old graph, still alive)
module Scenarios.FlowPoisonRecovery
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

usesMapSrc :: Text
usesMapSrc = T.unlines
  [ "module UsesMap where"
  , ""
  , "import qualified Data.Map.Strict as Map"
  , ""
  , "sizeOfEmpty :: Int"
  , "sizeOfEmpty = Map.size Map.empty"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + register a module that NEEDS containers so
  -- the dep graph is observable through check_module.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text)
                 , "name"   .= ("poison-recover" :: Text)
                 ])
  _ <- Client.callTool c GhcModule
         (object [ "action" .= ("add" :: Text)
                 , "modules" .= (["UsesMap"] :: [Text])
                 ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "UsesMap.hs") usesMapSrc

  -- Step 2 — baseline: session A boots and answers. containers is
  -- NOT in the dep graph yet.
  t0 <- stepHeader 1 "baseline · ghc_eval boots session A"
  base <- Client.callTool c GhcEval
            (object [ "expression" .= ("1 + 1" :: Text) ])
  cBase <- liveCheck $ checkPure
    "baseline ghc_eval(1+1) succeeds"
    (statusOk base == Just True)
    ("Expected baseline eval to succeed; got: " <> truncRender base)
  stepFooter 1 t0

  -- Step 3 — POISON EVENT #1: deps add mutates the .cabal and drops
  -- session A mid-life. The next call must boot session B and answer
  -- through it. (No dep-visibility negative control here: ghcide
  -- exposes GHC boot packages through the global package db even
  -- when undeclared, so a containers-importing module compiles
  -- either way — the pinned contract is the recycle, not the graph.)
  t2 <- stepHeader 2 "recovery #1 · deps add drops A, boot B answers"
  addR <- Client.callTool c GhcDeps
            (object [ "action" .= ("add" :: Text)
                    , "package" .= ("containers" :: Text)
                    ])
  cAdd <- liveCheck $ checkPure
    "ghc_deps(add containers) → status=ok"
    (statusOk addR == Just True)
    ("Got: " <> truncRender addR)
  stepFooter 3 t2

  t3 <- stepHeader 3 "boot B · check_module(UsesMap) compiles on the fresh session"
  withDeps <- Client.callTool c GhcCheck
                (object [ "action" .= ("module" :: Text)
                        , "module_path" .= ("src/UsesMap.hs" :: Text)
                        ])
  cWithDeps <- liveCheck $ checkPure
    "check_module(UsesMap) succeeds on the fresh session (containers resolved)"
    (statusOk withDeps == Just True)
    ("Got: " <> truncRender withDeps)
  stepFooter 4 t3

  t4 <- stepHeader 4 "boot B eval · sizeOfEmpty answers through the fresh session"
  ev <- Client.callTool c GhcEval
          (object [ "expression" .= ("sizeOfEmpty" :: Text) ])
  let okEval = statusOk ev == Just True
            && case lookupField "output" ev of
                 Just (String o) -> "0" `T.isInfixOf` o
                 _               -> False
  cEval <- liveCheck $ checkPure
    "ghc_eval(sizeOfEmpty) → ok AND output contains 0"
    okEval
    ("Got: " <> truncRender ev)
  stepFooter 4 t4

  -- Step 4 — POISON EVENT #2: deps remove drops B. Boot C must come
  -- up clean (no wedge) even though the graph shrank back.
  t5 <- stepHeader 5 "recovery #2 · deps remove drops B, boot C still answers"
  rmR <- Client.callTool c GhcDeps
           (object [ "action" .= ("remove" :: Text)
                   , "package" .= ("containers" :: Text)
                   ])
  cRm <- liveCheck $ checkPure
    "ghc_deps(remove containers) → status=ok"
    (statusOk rmR == Just True)
    ("Got: " <> truncRender rmR)
  stepFooter 5 t5

  t6 <- stepHeader 6 "boot C · ghc_eval still answers after two recycles"
  final <- Client.callTool c GhcEval
             (object [ "expression" .= ("40 + 2" :: Text) ])
  let okFinal = statusOk final == Just True
             && case lookupField "output" final of
                  Just (String o) -> "42" `T.isInfixOf` o
                  _               -> False
  cFinal <- liveCheck $ checkPure
    "ghc_eval(40+2) → ok AND output contains 42 (no wedge)"
    okFinal
    ("Got: " <> truncRender final)
  stepFooter 6 t6

  pure [cBase, cAdd, cWithDeps, cEval, cRm, cFinal]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
