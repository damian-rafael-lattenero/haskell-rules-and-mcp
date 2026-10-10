-- | Flow: ghc_module(action="write_module") — whole-module authorship
-- under the promote discipline (W2). The dogfood E4 gap was that an
-- agent could only splice bindings (promote) but never author a full
-- module body via MCP. Pins:
--
--   * a REGISTERED module whose file is missing → write_module creates
--     it, compile-verifies, eval resolves (overwritten=false)
--   * overwriting an existing good module commits (overwritten=true)
--   * a type-broken body ROLLS BACK — disk keeps the good content and
--     the previous eval still works
--   * an UNREGISTERED path never lands: the cradle resolves by the
--     .cabal's module list, so the write is UNVERIFIABLE — it fails
--     with the component-honesty error and nothing is left on disk
--   * a DOTTED module name ("Extra.Math") derives its src/<name>.hs
--     path and lands verified (W3.3 real-agent finding: models pass
--     the name even when the schema says path)
--   * the project gate counts the written module green
module Scenarios.FlowWriteModule
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, removeFile)
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

-- | First body: one function, stub from action=add is replaced.
calcV1 :: Text
calcV1 = T.unlines
  [ "module Calc where"
  , ""
  , "double :: Int -> Int"
  , "double n = n * 2"
  ]

-- | Second body: extends v1 — proves overwrite of a good module.
calcV2 :: Text
calcV2 = T.unlines
  [ "module Calc where"
  , ""
  , "double :: Int -> Int"
  , "double n = n * 2"
  , ""
  , "triple :: Int -> Int"
  , "triple n = n * 3"
  ]

-- | Type-broken body (parses fine, fails the typecheck verify).
calcBroken :: Text
calcBroken = T.unlines
  [ "module Calc where"
  , ""
  , "double :: Int -> Int"
  , "double = \"not an Int\""
  ]

-- | Body for the dotted-name check.
extraMath :: Text
extraMath = T.unlines
  [ "module Extra.Math where"
  , ""
  , "quad :: Int -> Int"
  , "quad n = n * 4"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text)
                 , "name"   .= ("write-mod" :: Text)
                 ])
  _ <- Client.callTool c GhcModule
         (object [ "action" .= ("add" :: Text)
                 , "modules" .= (["Calc"] :: [Text])
                 ])
  -- Host-side setup only: drop the scaffolded stub so the FIRST
  -- write_module takes the create-new-file branch.
  let calcPath = projectDir </> "src" </> "Calc.hs"
  removeFile calcPath

  -- 1 · create: registered module, file present-but-empty → full body
  -- written, verified, eval resolves through it.
  t1 <- stepHeader 1 "write_module · new body on registered module"
  w1 <- Client.callTool c GhcModule
          (object [ "action"  .= ("write_module" :: Text)
                  , "module"  .= ("src/Calc.hs" :: Text)
                  , "code"    .= calcV1
                  ])
  ev1 <- Client.callTool c GhcEval
           (object [ "expression" .= ("double 21" :: Text) ])
  c1 <- liveCheck $ checkPure
    "write_module(new body) → ok, overwritten=false, eval double 21 = 42"
    (statusOk w1 == Just True
      && boolField False "overwritten" w1
      && statusOk ev1 == Just True && out42 ev1)
    ("w1: " <> render w1 <> " || ev1: " <> render ev1)
  stepFooter 1 t1

  -- 2 · overwrite: extended body commits on top of a good module.
  t2 <- stepHeader 2 "write_module · overwrite good module commits"
  w2 <- Client.callTool c GhcModule
          (object [ "action"  .= ("write_module" :: Text)
                  , "module"  .= ("src/Calc.hs" :: Text)
                  , "code"    .= calcV2
                  ])
  ev2 <- Client.callTool c GhcEval
           (object [ "expression" .= ("triple 14" :: Text) ])
  c2 <- liveCheck $ checkPure
    "write_module(overwrite) → ok, overwritten=true, eval triple 14 = 42"
    (statusOk w2 == Just True
      && boolField True "overwritten" w2
      && statusOk ev2 == Just True && out42 ev2)
    ("w2: " <> render w2 <> " || ev2: " <> render ev2)
  stepFooter 2 t2

  -- 3 · rollback: a type-broken body must NOT survive — disk keeps
  -- calcV2 and the pre-existing eval still works.
  t3 <- stepHeader 3 "write_module · type-broken body rolls back"
  w3 <- Client.callTool c GhcModule
          (object [ "action"  .= ("write_module" :: Text)
                  , "module"  .= ("src/Calc.hs" :: Text)
                  , "code"    .= calcBroken
                  ])
  diskAfter <- TIO.readFile calcPath
  ev3 <- Client.callTool c GhcEval
           (object [ "expression" .= ("double 21" :: Text) ])
  c3 <- liveCheck $ checkPure
    "broken write → failed + snapshot restored + disk keeps calcV2 + eval 42"
    (statusOk w3 == Just False
      && "snapshot restored" `T.isInfixOf` render w3
      && "triple" `T.isInfixOf` diskAfter
      && "not an Int" `T.isInfixOf` diskAfter == False
      && statusOk ev3 == Just True && out42 ev3)
    ("w3: " <> render w3 <> " || disk: " <> T.take 120 diskAfter)
  stepFooter 3 t3

  -- 4 · unregistered path: the cradle resolves by the .cabal's module
  -- LIST, so this write is unverifiable — it must FAIL with the
  -- component-honesty cause and leave NOTHING on disk.
  t4 <- stepHeader 4 "write_module · unregistered path never lands"
  w4 <- Client.callTool c GhcModule
          (object [ "action"  .= ("write_module" :: Text)
                  , "module"  .= ("src/Unregistered.hs" :: Text)
                  , "code"    .= calcV1
                  ])
  unregLeft <- doesFileExist (projectDir </> "src" </> "Unregistered.hs")
  c4 <- liveCheck $ checkPure
    "unregistered write → failed + component cause + no file left"
    (statusOk w4 == Just False
      && "component" `T.isInfixOf` render w4
      && not unregLeft)
    ("w4: " <> render w4 <> " || left on disk: " <> T.pack (show unregLeft))
  stepFooter 4 t4

  -- 5 · dotted module name: derives src/Extra/Math.hs, lands verified.
  t5 <- stepHeader 5 "write_module · dotted name derives src/<name>.hs"
  _ <- Client.callTool c GhcModule
         (object [ "action" .= ("add" :: Text)
                 , "modules" .= (["Extra.Math"] :: [Text])
                 ])
  w5 <- Client.callTool c GhcModule
          (object [ "action"  .= ("write_module" :: Text)
                  , "module"  .= ("Extra.Math" :: Text)
                  , "code"    .= extraMath
                  ])
  extraOnDisk <- doesFileExist (projectDir </> "src" </> "Extra" </> "Math.hs")
  ev5 <- Client.callTool c GhcEval
           (object [ "expression" .= ("Extra.Math.quad 10" :: Text) ])
  c5 <- liveCheck $ checkPure
    "dotted name → ok, landed at src/Extra/Math.hs, eval quad 10 = 40"
    (statusOk w5 == Just True
      && extraOnDisk
      && "src/Extra/Math.hs" `T.isInfixOf` render w5
      && statusOk ev5 == Just True && outHas "40" ev5)
    ("w5: " <> render w5 <> " || ev5: " <> render ev5)
  stepFooter 5 t5

  -- 6 · the project gate counts the authored modules green.
  t6 <- stepHeader 6 "check(project) · authored module counted green"
  cp <- Client.callTool c GhcCheck (object [ "action" .= ("project" :: Text) ])
  c6 <- liveCheck $ checkPure
    "check(project) after write_module → green"
    (statusOk cp == Just True)
    ("cp: " <> render cp)
  stepFooter 6 t6

  pure [c1, c2, c3, c4, c5, c6]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

boolField :: Bool -> Text -> Value -> Bool
boolField want key v = case lookupField key v of
  Just (Bool b) -> b == want
  _             -> False

out42 :: Value -> Bool
out42 v = outHas "42" v

outHas :: Text -> Value -> Bool
outHas needle v = case lookupField "output" v of
  Just (String s) -> needle `T.isInfixOf` s
  _               -> False

render :: Value -> Text
render v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
