-- | Flow: 'ghc_check_module' properties gate sees stored entries (#74).
--
-- Pre-#74, the properties gate compared the caller's @module_path@
-- (a relative path like @src/Foo/Bar.hs@) against the property
-- store's @module@ field (the Haskell module name @Foo.Bar@). The
-- two strings only matched by accident, so the gate reported
-- @total: 0@ for every module that actually had stored properties.
--
-- Post-#74, 'ghc_check_module' reads the source file's @module …
-- where@ header and accepts both shapes. This scenario asserts
-- that contract by:
--
--   1. scaffolding a project,
--   2. persisting a property under the canonical module-name shape
--      (the one 'ghc_quickcheck' produces), and
--   3. calling @check_module@ with the relative path — the
--      properties gate must surface @total: 1, passed: 1@.
module Scenarios.FlowCheckModuleProperties
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
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold so we have a module to gate.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("check-prop-demo" :: Text) ])

  -- Step 2 — plant a single passing property under the
  -- canonical module-name shape ("CheckPropDemo"). This is
  -- the exact shape 'ghc_quickcheck' would persist on first
  -- pass, so the test is end-to-end realistic.
  let storeDir = projectDir </> ".haskell-flows"
  createDirectoryIfMissing True storeDir
  TIO.writeFile (storeDir </> "properties.json")
    "[{\"expression\":\"\\\\x -> x == (x :: Int)\",\
    \\"module\":\"CheckPropDemo\",\"passed\":1,\"updated\":0}]"

  -- Step 3 — load the module so the gate has a fresh GhcSession.
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/CheckPropDemo.hs" :: Text) ])

  -- Step 4 — call check_module by relative path. Pre-#74 this
  -- would return total=0 for the properties gate. Post-#74 it
  -- must see the planted property and report total=1, passed=1.
  t0 <- stepHeader 1 "ghc_check_module sees module-name-shaped properties (#74)"
  rCheck <- Client.callTool c GhcCheckModule
              (object [ "module_path" .= ("src/CheckPropDemo.hs" :: Text) ])
  let total    = fieldIntPath ["gates", "properties", "total"]    rCheck
      passed   = fieldIntPath ["gates", "properties", "passed"]   rCheck
      status   = fieldTextPath ["gates", "properties", "status"]  rCheck
      ok       = fieldBoolPath ["gates", "properties", "ok"]      rCheck
      okShape  = total == 1
              && passed == 1
              && status == Just "pass"
              && ok == Just True
  cShape <- liveCheck $ checkPure
    "properties gate finds the stored entry and reports total=1, passed=1"
    okShape
    ("Got: " <> truncRender rCheck)
  stepFooter 1 t0

  pure [cShape]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBoolPath :: [Text] -> Value -> Maybe Bool
fieldBoolPath ks v = case lookupPath ks v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldIntPath :: [Text] -> Value -> Int
fieldIntPath ks v = case lookupPath ks v of
  Just (Number n) -> truncate (toRational n)
  _               -> -1

fieldTextPath :: [Text] -> Value -> Maybe Text
fieldTextPath ks v = case lookupPath ks v of
  Just (String s) -> Just s
  _               -> Nothing

-- | Walk a key path, auto-drilling through @result@ at the top
-- level (post-#90 envelope). Subsequent hops are direct.
lookupPath :: [Text] -> Value -> Maybe Value
lookupPath ks v = foldl step (Just v) ks
  where
    step Nothing _    = Nothing
    step (Just o) k   = lookupField k o

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
