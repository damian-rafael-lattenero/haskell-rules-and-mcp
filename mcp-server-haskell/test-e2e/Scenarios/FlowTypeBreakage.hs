-- | Flow: deliberately introduce a TYPE error and verify that
-- @ghc_check_module@ refuses to mark the module green.
--
-- This is the scenario that would have caught the F-08 regression
-- (where the @Deferred@ @:unset@ leaked across calls and every
-- compile-check silently deferred its type errors). It is the only
-- observable test for the invariant "@ghc_check_module@ fails a
-- module that does not type-check, period".
--
-- Real user flow:
--
--   1. Dev writes 'f :: Int -> Int' and implements 'f x = x + 1'.
--   2. @ghc_check_module@ returns overall=true. Good.
--   3. Dev edits the body to 'f x = show x' — now the body has type
--      'Int -> String', contradicting the declared signature.
--   4. Dev asks @ghc_check_module@ again — must be RED.
--
-- Wrong answers (bugs we want to catch):
--
--   (a) overall=true after the mismatch. This is the F-08 shape:
--       errors silently deferred, module reports clean.
--   (b) success=true with compile gate green. Same bug, different
--       field.
--   (c) exception leaks through the transport. Dispatcher is not
--       catching tool panics.
module Scenarios.FlowTypeBreakage
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
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
import E2E.Envelope (fieldBool, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

clean :: Text
clean = T.unlines
  [ "module Arith where"
  , ""
  , "f :: Int -> Int"
  , "f x = x + 1"
  ]

broken :: Text
broken = T.unlines
  [ "module Arith where"
  , ""
  , "f :: Int -> Int"
  , "f x = show x     -- INTENTIONAL TYPE ERROR: returns String, not Int"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) scaffold + module that type-checks
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Arith.hs (typechecks)"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("typebreak-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Arith"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Arith.hs") clean
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Arith.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) precondition — module gate is GREEN on the clean source
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "precondition · check_module is GREEN on clean source"
  rClean <- Client.callTool c GhcCheckModule
              (object [ "module_path" .= ("src/Arith.hs" :: Text) ])
  let cleanOverall = fieldBool "overall" rClean == Just True
  cPre <- liveCheck $ checkPure
    "precondition · check_module overall=true on clean source"
    cleanOverall
    ("The scenario is useless if the module was already red. Something \
     \is miswired. Raw: " <> truncRender rClean)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) introduce a type error and re-check
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "break the types: f :: Int -> Int but body returns String"
  TIO.writeFile (projectDir </> "src" </> "Arith.hs") broken

  rBroken <- Client.callTool c GhcCheckModule
               (object [ "module_path" .= ("src/Arith.hs" :: Text) ])

  let overallBroken = fieldBool "overall" rBroken
      compileGreen  = case lookupPath rBroken ["gates", "compile", "ok"] of
                        Just (Bool b) -> b
                        _             -> False

  cOverall <- liveCheck $ checkPure
    "type error detected · check_module overall=false"
    (overallBroken == Just False)
    ("THE OVERALL FLAG IS LYING. If overall=true on a source with a \
     \'Couldn't match expected type' error, the compile gate is \
     \silently deferred. Check Tool/CheckModule + Session.hs :load \
     \paths for a regression of F-08 (deferred-passes leak). \
     \Raw: " <> truncRender rBroken)

  cCompile <- liveCheck $ checkPure
    "type error detected · gates.compile.ok=false (per-gate shape)"
    (not compileGreen)
    "gates.compile.ok must be false when the module has an \
    \unresolved type error. If this is true, the overall flag \
    \above is also wrong by propagation."
  stepFooter 3 t2

  pure [cPre, cOverall, cCompile]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Walk a key path; auto-drills the first hop through @result@
-- so post-#90 envelope responses resolve.
lookupPath :: Value -> [Text] -> Maybe Value
lookupPath = foldl step . Just
  where
    step (Just o) k = lookupField k o
    step Nothing  _ = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
