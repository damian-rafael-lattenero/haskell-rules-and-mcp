-- | Flow: 'ghc_deps_explain' Phase 1 — translates a cabal solver
-- dump into a structured conflict report (#63).
--
-- Phase 1 contract pinned here:
--
--   * @cabal_output@ supplied → tool parses without re-running
--     cabal. Faster e2e, deterministic input.
--   * Response carries a non-null @conflict@ object whose
--     @root_cause.package@ matches the deepest rejection in the
--     dump.
--   * Clean dump → @conflict: null@.
module Scenarios.FlowDepsExplain
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, statusIs, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

solverDump :: Text
solverDump = T.unlines
  [ "Resolving dependencies..."
  , "cabal: Could not resolve dependencies:"
  , "[__0] trying: my-project-0.1.0.0 (user goal)"
  , "[__1] next goal: aeson (dependency of my-project)"
  , "[__1] rejecting: aeson-2.2.3.0 (conflict: my-project => aeson < 2.0)"
  , "[__41] rejecting: aeson-2.1.2.1 (conflict: text >= 2.0 needed; text-1.2.5.0 installed)"
  , "[__41] backjump limit reached (currently 4000, change with --max-backjumps)."
  ]

cleanDump :: Text
cleanDump = T.unlines
  [ "Resolving dependencies..."
  , "Build profile: -w ghc-9.12.2 -O1"
  , "In order, the following will be built:"
  , " - my-project-0.1.0.0 (lib)"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 1 — supplied dump → conflict extracted.
  t0 <- stepHeader 1 "ghc_deps_explain extracts root_cause (#63)"
  rConflict <- Client.callTool c GhcDepsExplain
                 (object [ "cabal_output" .= solverDump ])
  let success = statusOk rConflict
      rootPkg = drillStr ["conflict", "root_cause", "package"] rConflict
      rootDepth = case drill ["conflict", "root_cause", "depth"] rConflict of
        Just (Number n) -> truncate (toRational n) :: Int
        _               -> -1
  cConflict <- liveCheck $ checkPure
    "supplied dump → success=true, root_cause.package='aeson-2.1.2.1', depth=41"
    (success == Just True
       && rootPkg == Just "aeson-2.1.2.1"
       && rootDepth == 41)
    ( "Expected: success=true, root_cause.package='aeson-2.1.2.1', depth=41. \
      \Got: success=" <> T.pack (show success)
      <> ", pkg=" <> T.pack (show rootPkg)
      <> ", depth=" <> T.pack (show rootDepth)
      <> ". Raw: " <> truncRender rConflict )
  stepFooter 1 t0

  -- Step 2 — clean dump → conflict: null.
  t1 <- stepHeader 2 "ghc_deps_explain returns null on clean dump (#63)"
  rClean <- Client.callTool c GhcDepsExplain
              (object [ "cabal_output" .= cleanDump ])
  -- Issue #90: 'no conflict found' is semantically status='no_match'
-- post-envelope (the tool was asked "is there a conflict?" and
-- the answer was no). Accept either 'ok' or 'no_match' — both
-- represent "explainer ran successfully, no conflict in input".
  let cleanOk = (statusOk rClean == Just True
                  || statusIs "no_match" rClean)
              && drill ["conflict"] rClean == Just Null
  cClean <- liveCheck $ checkPure
    "clean dump → ok|no_match, conflict=null"
    cleanOk
    ("Expected conflict=null. Got: " <> truncRender rClean)
  stepFooter 2 t1

  pure [cConflict, cClean]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

drill :: [Text] -> Value -> Maybe Value
drill [] v = Just v
drill (k : ks) v = case lookupField k v of
  Just inner -> drill ks inner
  Nothing    -> Nothing

drillStr :: [Text] -> Value -> Maybe Text
drillStr ks v = case drill ks v of
  Just (String s) -> Just s
  _               -> Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 800
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
