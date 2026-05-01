-- | Flow: 'ghc_check_module' properties-gate carries a status
-- discriminator and the reason text matches the ok flag (#42).
--
-- Pre-fix behaviour
-- -----------------
-- @gates.properties.reason@ said \"1 stored properties pass\"
-- even when @ok@ was @false@, because the reason was computed
-- independently of whether any property had regressed. Agents
-- that pattern-match on @reason@ drew the wrong conclusion.
--
-- New contract
-- ------------
-- The gate carries:
--
--   * @status@:    \"empty\" | \"pass\" | \"regressed\" | \"skipped\"
--   * @total@:     stored property count.
--   * @passed@:    replays that returned 'QcPassed'.
--   * @regressed@: replays that returned a non-pass result.
--   * @skipped@:   replays whose module load failed (#51).
--   * @reason@:    free-text mirror of @status@.
--
-- @ok=false@ AND @reason@ \"...pass\" can no longer co-occur.
module Scenarios.FlowGatesProperties
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

-- | A library module that compiles cleanly. We only need it to
-- exist so check_module has something to point at.
demoSrc :: Text
demoSrc = T.unlines
  [ "module GateDemo where"
  , ""
  , "double :: Int -> Int"
  , "double x = x * 2"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + write GateDemo.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("gate-props-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["GateDemo"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "GateDemo.hs") demoSrc

  -- Step 2 — plant a property whose lambda references a name
  -- that doesn't exist in GateDemo. The replay's compile fails
  -- → load_failed → properties gate goes red with status=skipped.
  let storeFile = projectDir </> ".haskell-flows" </> "properties.json"
  createDirectoryIfMissing True (projectDir </> ".haskell-flows")
  TIO.writeFile storeFile
    "[{\"expression\":\"\\\\x -> nonexistentFn (x :: Int) == nonexistentFn x\",\
    \\"module\":\"src/GateDemo.hs\",\"passed\":1,\"updated\":0}]"

  -- Step 3 — invoke check_module. Properties gate must reflect
  -- the load-failure: ok=false, status=skipped, reason mentions
  -- load — NOT \"pass\".
  t0 <- stepHeader 1 "ghc_check_module reports skipped status (#42)"
  r <- Client.callTool c GhcCheckModule
         (object [ "module_path" .= ("src/GateDemo.hs" :: Text) ])
  let propsGate = drillField "gates" "properties" r
      okVal     = case propsGate of
                    Just (Object o) -> KeyMap.lookup (Key.fromText "ok") o
                    _               -> Nothing
      statusVal = case propsGate of
                    Just (Object o) -> KeyMap.lookup (Key.fromText "status") o
                    _               -> Nothing
      reasonVal = case propsGate of
                    Just (Object o) -> KeyMap.lookup (Key.fromText "reason") o
                    _               -> Nothing
      reasonOk  = case reasonVal of
                    Just (String s) ->
                         not ("stored properties pass" `T.isInfixOf` s)
                      && not (T.null s)
                    _               -> False
  cShape <- liveCheck $ checkPure
    "gates.properties has ok=false, status=skipped, reason ≠ 'pass'"
    (okVal == Just (Bool False)
      && statusVal == Just (String "skipped")
      && reasonOk)
    ( "Expected: ok=false, status='skipped', reason without 'pass'. \
      \Got: ok=" <> T.pack (show okVal)
      <> ", status=" <> T.pack (show statusVal)
      <> ", reason=" <> T.pack (show reasonVal) )
  stepFooter 1 t0

  pure [cShape]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

drillField :: Text -> Text -> Value -> Maybe Value
drillField outer inner v = case lookupField outer v of
  Just outerV -> lookupField inner outerV
  Nothing     -> Nothing

