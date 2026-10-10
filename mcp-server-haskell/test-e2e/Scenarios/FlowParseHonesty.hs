-- | Flow: parse-error honesty — a file that fails to PARSE must fail
-- the module gate AND the project gate. ghcide publishes parser
-- diagnostics under the GetParsedModule rule; TypeCheck only sees a
-- BadDependency (no diags of its own), so a check that reads only
-- TypeCheck/GetModSummary rule diags blesses parse-broken files
-- ("7/7 modules compile clean" while the session shows GHC-58481
-- parse errors) — the 2026-10-10 dogfood honesty bug.
module Scenarios.FlowParseHonesty
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

-- 'if then else' cannot parse on any GHC — a pure parser breakage,
-- no typechecking involved.
brokenSrc :: Text
brokenSrc = T.unlines
  [ "module ParseBroken where"
  , ""
  , "answer :: Int"
  , "answer = if then else"
  ]

fixedSrc :: Text
fixedSrc = T.unlines
  [ "module ParseBroken where"
  , ""
  , "answer :: Int"
  , "answer = 40 + 2"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text)
                 , "name"   .= ("parse-honesty" :: Text)
                 ])
  _ <- Client.callTool c GhcModule
         (object [ "action" .= ("add" :: Text)
                 , "modules" .= (["ParseBroken"] :: [Text])
                 ])
  TIO.writeFile (projectDir </> "src" </> "ParseBroken.hs") brokenSrc

  -- The module gate must SEE the parse error: overall=false and the
  -- errors array non-empty with a parser-flavoured message.
  t0 <- stepHeader 1 "check(module) · parse-broken file fails"
  cm <- Client.callTool c GhcCheck
          (object [ "action" .= ("module" :: Text)
                  , "module_path" .= ("src/ParseBroken.hs" :: Text)
                  ])
  let okModule = statusOk cm == Just False
  cModule <- liveCheck $ checkPure
    "check(module) on parse-broken → failed (not a clean green)"
    okModule
    (render cm)
  stepFooter 1 t0

  t1 <- stepHeader 2 "check(module) · errors carry the parser message"
  let errs = arrayValues "errors" cm
      mentionsParse = case errs of
        [] -> False
        _  -> any msgMentionsParse errs
  cParse <- liveCheck $ checkPure
    "check(module) · ≥1 error mentioning 'parse' (GHC-58481 family)"
    (statusOk cm == Just False && mentionsParse)
    (render cm)
  stepFooter 2 t1

  -- The project gate must flag it too — this is the exact dogfood
  -- lie: "N/N modules compile clean" over a parse-broken project.
  t2 <- stepHeader 3 "check(project) · parse-broken module counted"
  cp <- Client.callTool c GhcCheck (object [ "action" .= ("project" :: Text) ])
  cProject <- liveCheck $ checkPure
    "check(project) on parse-broken → failed + failed ≥ 1"
    (statusOk cp == Just False && countAtLeast 1 "failed" cp)
    (render cp)
  stepFooter 3 t2

  -- Recovery: host-fix the file, the project gate goes green again —
  -- the red above was the parse error, not a stuck gate.
  t3 <- stepHeader 4 "host-fix → check(project) green + eval 42"
  TIO.writeFile (projectDir </> "src" </> "ParseBroken.hs") fixedSrc
  cp2 <- Client.callTool c GhcCheck (object [ "action" .= ("project" :: Text) ])
  ev <- Client.callTool c GhcEval
          (object [ "expression" .= ("answer" :: Text) ])
  cRecovered <- liveCheck $ checkPure
    "fixed project → check(project) ok and eval(answer) = 42"
    (statusOk cp2 == Just True && statusOk ev == Just True && out42 ev)
    ("cp2: " <> render cp2 <> " || ev: " <> render ev)
  stepFooter 4 t3

  pure [cModule, cParse, cProject, cRecovered]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

arrayValues :: Text -> Value -> [Value]
arrayValues key v = case lookupField key v of
  Just (Array a) -> foldr (:) [] a
  _              -> []

countAtLeast :: Int -> Text -> Value -> Bool
countAtLeast n key v = case lookupField key v of
  Just (Number x) -> x >= fromIntegral n
  _               -> False

msgMentionsParse :: Value -> Bool
msgMentionsParse e = case lookupField "message" e of
  Just (String s) -> "parse" `T.isInfixOf` T.toLower s
  _               -> False

out42 :: Value -> Bool
out42 v = case lookupField "output" v of
  Just (String s) -> "42" `T.isInfixOf` s
  _               -> False

render :: Value -> Text
render v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
