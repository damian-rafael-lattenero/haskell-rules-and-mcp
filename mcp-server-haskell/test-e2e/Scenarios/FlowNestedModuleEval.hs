-- | Flow: nested-module eval — functions in @src/Sub/Nested.hs@ must
-- be evaluable exactly like flat @src/Flat.hs@ ones (dogfood
-- 2026-10-10: bare AND qualified eval of nested-module names failed
-- with "no project module provides the names" while flat modules
-- worked; the e2e corpus was all-flat so 410/410 never saw it).
module Scenarios.FlowNestedModuleEval
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

flatSrc :: Text
flatSrc = T.unlines
  [ "module FlatMod where"
  , ""
  , "flatAnswer :: Int"
  , "flatAnswer = 21"
  ]

nestedSrc :: Text
nestedSrc = T.unlines
  [ "module Sub.Nested where"
  , ""
  , "import FlatMod (flatAnswer)"
  , ""
  , "nestedAnswer :: Int"
  , "nestedAnswer = flatAnswer * 2"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Scaffold + register a flat module and a nested one that IMPORTS
  -- the flat one (intra-project dep — the dogfood shape).
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text)
                 , "name"   .= ("nested-eval" :: Text)
                 ])
  _ <- Client.callTool c GhcModule
         (object [ "action" .= ("add" :: Text)
                 , "modules" .= (["FlatMod", "Sub.Nested"] :: [Text])
                 ])
  createDirectoryIfMissing True (projectDir </> "src" </> "Sub")
  TIO.writeFile (projectDir </> "src" </> "FlatMod.hs") flatSrc
  TIO.writeFile (projectDir </> "src" </> "Sub" </> "Nested.hs") nestedSrc
  _ <- Client.callTool c GhcCheck
         (object [ "action" .= ("load" :: Text)
                 , "module_path" .= ("src/Sub/Nested.hs" :: Text)
                 ])
  -- The dogfood event: a .cabal edit DROPS the session mid-flight;
  -- the project check that follows boots a FRESH session, and every
  -- later eval rides it.
  _ <- Client.callTool c GhcDeps
         (object [ "action" .= ("add" :: Text)
                 , "package" .= ("containers" :: Text)
                 ])
  _ <- Client.callTool c GhcCheck
         (object [ "action" .= ("project" :: Text) ])

  -- Control: the flat module evals.
  t0 <- stepHeader 1 "control · flat module eval after session recycle"
  flat <- Client.callTool c GhcEval
            (object [ "expression" .= ("flatAnswer" :: Text) ])
  let okFlat = statusOk flat == Just True
            && outHas "21" flat
  cFlat <- liveCheck $ checkPure
    "eval(flatAnswer) → ok + 21 (control)"
    okFlat
    ("Got: " <> truncRender flat)
  stepFooter 1 t0

  -- The pin: the nested module must eval BARE exactly like the flat
  -- one — same anchor chain, same component, one more directory.
  t1 <- stepHeader 2 "pin · nested module eval (bare name)"
  nested <- Client.callTool c GhcEval
              (object [ "expression" .= ("nestedAnswer" :: Text) ])
  let okNested = statusOk nested == Just True
              && outHas "42" nested
  cNested <- liveCheck $ checkPure
    "eval(nestedAnswer) → ok + 42 (src/Sub/Nested.hs in the anchor chain)"
    okNested
    ("Got: " <> truncRender nested)
  stepFooter 2 t1

  -- And qualified — the hint's own fallback must work too.
  t2 <- stepHeader 3 "pin · nested module eval (qualified name)"
  qual <- Client.callTool c GhcEval
            (object [ "expression" .= ("Sub.Nested.nestedAnswer" :: Text) ])
  cQual <- liveCheck $ checkPure
    "eval(Sub.Nested.nestedAnswer) → ok"
    (statusOk qual == Just True)
    ("Got: " <> truncRender qual)
  stepFooter 3 t2

  pure [cFlat, cNested, cQual]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

outHas :: Text -> Value -> Bool
outHas needle v = case lookupField "output" v of
  Just (String s) -> needle `T.isInfixOf` s
  _               -> False

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
