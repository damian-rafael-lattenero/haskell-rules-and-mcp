-- | Flow: multi-unit lifecycle — a project that grows from
-- lib+test to lib+test+exe mid-session keeps every unit servable
-- after one session recycle (W7).
--
-- Contract pinned
-- ---------------
-- ghcide resolves units through the cradle, and the cradle reads the
-- @.cabal@ at boot. A unit added WHILE a session is live is invisible
-- to that session by design — the fix contract (W6.8.3) is that
-- @ghc_modules(action=add)@ and @ghc_project(action=switch)@ drop the
-- slot, so the next call boots a session whose cradle sees all three
-- units.
--
-- Sequence:
--   create (lib+test) → boot A → add MU.Extra + append exe stanza
--   → switch-to-self (drop) → boot B resolves lib + exe + test.
module Scenarios.FlowMultiUnitLifecycle
  ( runFlow
  ) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeExtension, (</>))

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

extraSrc :: Text
extraSrc = T.unlines
  [ "module MU.Extra where"
  , ""
  , "extraAnswer :: Int"
  , "extraAnswer = 42"
  ]

exeMainSrc :: Text
exeMainSrc = T.unlines
  [ "module Main where"
  , ""
  , "import MU.Extra (extraAnswer)"
  , ""
  , "main :: IO ()"
  , "main = print extraAnswer"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold: create writes a lib (src/<Mod>.hs) + a
  -- test-suite (test/Spec.hs). No executable.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text)
                 , "name"   .= ("multi-unit" :: Text)
                 ])

  -- Step 2 — baseline: session A boots against the 2-unit project.
  t0 <- stepHeader 1 "baseline · ghc_eval boots session A (lib+test)"
  base <- Client.callTool c GhcEval
            (object [ "expression" .= ("1 + 1" :: Text) ])
  cBase <- liveCheck $ checkPure
    "baseline ghc_eval(1+1) succeeds"
    (statusOk base == Just True)
    ("Expected baseline eval to succeed; got: " <> truncRender base)
  stepFooter 1 t0

  -- Step 3 — grow the project mid-session: register MU.Extra on the
  -- lib (drops A), write its source, write app/Main.hs, and append
  -- an executable stanza referencing the package itself.
  _ <- Client.callTool c GhcModule
         (object [ "action" .= ("add" :: Text)
                 , "modules" .= (["MU.Extra"] :: [Text])
                 ])
  createDirectoryIfMissing True (projectDir </> "src" </> "MU")
  TIO.writeFile (projectDir </> "src" </> "MU" </> "Extra.hs") extraSrc
  createDirectoryIfMissing True (projectDir </> "app")
  TIO.writeFile (projectDir </> "app" </> "Main.hs") exeMainSrc
  cabalPath <- findCabalFile projectDir
  origCabal <- TIO.readFile cabalPath
  -- mtime must strictly advance for the cradle + ghcide watchers.
  threadDelay 1_100_000
  TIO.writeFile cabalPath (origCabal <> exeStanza "multi-unit")

  -- Step 4 — recycle: switch-to-self serialises the quartet swap and
  -- drops the IdeSession (2b78e6e discipline: slot refilled first).
  t1 <- stepHeader 2 "recycle · ghc_project(switch to self) drops session A"
  swR <- Client.callTool c GhcProject
           (object [ "action" .= ("switch" :: Text)
                   , "path"   .= T.pack projectDir
                   ])
  cSwitch <- liveCheck $ checkPure
    "ghc_project(switch self) → status=ok"
    (statusOk swR == Just True)
    ("Got: " <> truncRender swR)
  stepFooter 2 t1

  -- Step 5 — boot B resolves the NEW lib module: the module added
  -- mid-session is eval-able after the recycle.
  t2 <- stepHeader 3 "boot B · eval reaches the mid-session module add"
  ev <- Client.callTool c GhcEval
          (object [ "expression" .= ("extraAnswer" :: Text) ])
  let okEval = statusOk ev == Just True
            && case lookupField "output" ev of
                 Just (String o) -> "42" `T.isInfixOf` o
                 _               -> False
  cEval <- liveCheck $ checkPure
    "ghc_eval(extraAnswer) → ok AND output contains 42"
    okEval
    ("Got: " <> truncRender ev)
  stepFooter 3 t2

  -- Step 6 — the executable unit compiles through check_module.
  t3 <- stepHeader 4 "exe unit · check_module(app/Main.hs) on the fresh session"
  exeChk <- Client.callTool c GhcCheck
              (object [ "action" .= ("module" :: Text)
                      , "module_path" .= ("app/Main.hs" :: Text)
                      ])
  cExe <- liveCheck $ checkPure
    "check_module(app/Main.hs) → status=ok (executable unit visible)"
    (statusOk exeChk == Just True)
    ("Got: " <> truncRender exeChk)
  stepFooter 4 t3

  -- Step 7 — the test unit still compiles after the growth: the
  -- scaffolded suite keeps resolving against the fattened lib.
  t4 <- stepHeader 5 "test unit · check_module(test/Spec.hs) still ok"
  tstChk <- Client.callTool c GhcCheck
              (object [ "action" .= ("module" :: Text)
                      , "module_path" .= ("test/Spec.hs" :: Text)
                      ])
  cTest <- liveCheck $ checkPure
    "check_module(test/Spec.hs) → status=ok (test unit intact)"
    (statusOk tstChk == Just True)
    ("Got: " <> truncRender tstChk)
  stepFooter 5 t4

  pure [cBase, cSwitch, cEval, cExe, cTest]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

exeStanza :: Text -> Text
exeStanza pkg = T.unlines
  [ ""
  , "executable " <> pkg
  , "    import:           shared"
  , "    main-is:          Main.hs"
  , "    hs-source-dirs:   app"
  , "    build-depends:    base >= 4.20 && < 5"
  , "                    , " <> pkg
  ]

findCabalFile :: FilePath -> IO FilePath
findCabalFile root = do
  entries <- listDirectory root
  case [ root </> e | e <- entries, takeExtension e == ".cabal" ] of
    (p : _) -> pure p
    []      -> error ("FlowMultiUnitLifecycle: no .cabal in " <> root)

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
