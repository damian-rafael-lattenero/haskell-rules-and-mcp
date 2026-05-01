-- | Flow: @ghc_regression(run)@ classifies load-failures
-- distinctly from regressions (#51).
--
-- Pre-fix behaviour
-- -----------------
-- A persisted property whose recorded module no longer compiles
-- (typically because the module's contents changed since the
-- property was first captured) returned
-- @outcome: { state: "unparsed", raw: "" }@ and was tallied as a
-- regression. That conflated three states the agent needs to
-- separate:
--
--   * /Pass/        — replay ran, all 200 cases held.
--   * /Regression/  — replay ran, a counterexample was found.
--   * /Load failed/ — replay never ran; the recorded scope can
--                     no longer compile.
--
-- New contract
-- ------------
-- Properties whose load fails are surfaced under @"load_failed"@
-- with @outcome.state = "load_failed"@ and the captured stderr
-- in @outcome.error@. They do not contribute to @"regressions"@.
-- The summary line names both counts.
module Scenarios.FlowRegressionLoadFailure
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
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
  -- Step 1 — scaffold a tiny project. We don't need a working
  -- test-suite; we just need somewhere for ghc_regression to
  -- replay properties from.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("regload-demo" :: Text) ])

  -- Step 2 — write a property to the on-disk store whose lambda
  -- references a name that doesn't exist anywhere in the
  -- project. cabal-repl will load the file but the body fails
  -- with @"Variable not in scope: ghostFn"@. Pre-#51 this came
  -- back as a regression with raw="" — confusing.
  let storeDir = projectDir </> ".haskell-flows"
  createDirectoryIfMissing True storeDir
  TIO.writeFile (storeDir </> "properties.json")
    "[{\"expression\":\"\\\\x -> ghostFn (x :: Int) == ghostFn x\",\
    \\"module\":\"src/Main.hs\",\"passed\":1,\"updated\":0}]"

  -- Step 3 — run the regression. The property must NOT be
  -- counted as a regression; it must surface under load_failed.
  t0 <- stepHeader 1 "ghc_regression(run) classifies load failure (#51)"
  r <- Client.callTool c GhcPropertyStore
         (object [ "action" .= ("run" :: Text), "action" .= ("run" :: Text) ])
  let regressions     = arrayLen "regressions" r
      loadFailed      = arrayLen "load_failed" r
      noRegressions   = regressions == 0
      hasLoadFailures = loadFailed >= 1
  cClassify <- liveCheck $ checkPure
    "regressions=0, load_failed≥1 (was: regressions≥1, raw=\"\")"
    (noRegressions && hasLoadFailures)
    ( "Expected: regressions array empty, load_failed array non-empty. \
      \Got: regressions=" <> T.pack (show regressions)
      <> ", load_failed=" <> T.pack (show loadFailed)
      <> ". Raw: " <> truncRender r )
  stepFooter 1 t0

  -- Step 4 — the load_failed entry must carry a 'state:
  -- load_failed' outcome with a non-empty 'error' message
  -- (ghost identifier name should be visible).
  t1 <- stepHeader 2 "load_failed entry has state + error (#51)"
  let firstLF       = firstArrayElement "load_failed" r
      stateLF       = drillField "outcome" "state" firstLF
      errorLF       = drillField "outcome" "error" firstLF
      isLoadFailed  = stateLF == Just "load_failed"
      errorPresent  = case errorLF of
                        Just msg -> not (T.null (T.strip msg))
                        Nothing  -> False
  cShape <- liveCheck $ checkPure
    "outcome.state = 'load_failed' with non-empty 'error'"
    (isLoadFailed && errorPresent)
    ( "Expected outcome.state='load_failed', non-empty error. \
      \Got: state=" <> T.pack (show stateLF)
      <> ", error=" <> T.pack (show errorLF) )
  stepFooter 2 t1

  pure [cClassify, cShape]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

arrayLen :: Text -> Value -> Int
arrayLen k v = case lookupField k v of
  Just (Array xs) -> V.length xs
  _               -> -1

firstArrayElement :: Text -> Value -> Maybe Value
firstArrayElement k v = case lookupField k v of
  Just (Array xs) | not (V.null xs) -> Just (V.head xs)
  _                                  -> Nothing

drillField :: Text -> Text -> Maybe Value -> Maybe Text
drillField outer inner Nothing = Nothing
  where _ = (outer, inner)
drillField outer inner (Just v) = case lookupField outer v of
  Just inner' -> case lookupField inner inner' of
    Just (String t) -> Just t
    _               -> Nothing
  _ -> Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
