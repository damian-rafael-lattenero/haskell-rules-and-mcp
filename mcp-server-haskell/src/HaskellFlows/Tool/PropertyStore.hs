-- | @ghc_property_store@ — action-discriminated primitive that
-- subsumes the four legacy property-store tools:
--
--   * @action: \"list\"@   — 'HaskellFlows.Tool.Regression' (action=list)
--   * @action: \"run\"@    — 'HaskellFlows.Tool.Regression' (action=run)
--   * @action: \"export\"@ — 'HaskellFlows.Tool.QuickCheckExport'
--   * @action: \"audit\"@  — 'HaskellFlows.Tool.PropertyAudit'
--
-- Issue #94 Phase C step 6: the four per-verb tools are retired
-- outright and replaced by this single action-discriminated
-- primitive. This collapses four wire surfaces to one and aligns
-- with the previous mergers' pattern.
--
-- (History: a fifth sibling, HaskellFlows.Tool.PropertyLifecycle, had
-- the same shape as @action=list@ on the legacy 'ghc_regression'; it
-- became unreachable after this consolidation and was deleted in the
-- 2026-10 cleanup audit — zero importers, stale "tests still exercise
-- it" justification.)
--
-- #275: dispatch now lives HERE in 'handle' (next to the tool it
-- discriminates, consistent with ghc_deps / ghc_modules / ghc_workflow)
-- rather than in 'Server.dispatchPropertyStore'. The differing per-handler
-- dependencies ('Store', 'GhcSession', 'ProjectDir') are injected as
-- parameters, exactly as 'ghc_workflow' already threads its server state.
--
-- Schema is per-action @oneOf@-discriminated (issue #92): each
-- action declares its own required-field set (which, for these
-- four, is empty — 'action' is the only field).
module HaskellFlows.Tool.PropertyStore
  ( handle
  , renderStored
  , listResult
  ) where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef, readIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Data.PropertyStore (Store, StoredProperty (..), loadAll)
import HaskellFlows.Ghc.IdeSession (IdeSession)
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.Schema as Schema
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.IdeBacked qualified as IdeBacked
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExportTool
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Types (ProjectDir)

-- | #275: dispatch a @ghc_property_store@ call to the right delegate based on
-- the @action@ discriminator. Dependencies are injected: @ideRef@ lazily boots
-- the ghcide session (run / audit need it; list / export do not),
-- @storeRef@ + @pdRef@ are the server's refs. @list@ / @run@ keep the @action@
-- field (the run renderer parses it); @export@ / @audit@ strip it.
handle :: ToolEnv -> Value -> IO ToolResponse
handle env =
  runHandle
    (teIdeSessionRef env)
    (teStoreRef env)
    (teProjectDirRef env)

runHandle
  :: MVar (Maybe IdeSession)
  -> IORef Store
  -> IORef ProjectDir
  -> Value
  -> IO ToolResponse
runHandle ideRef storeRef pdRef rawArgs = case actionField rawArgs of
  Nothing ->
    pure (Env.mkRefused
        (Env.mkErrorEnvelope Env.MissingArg
          "ghc_property_store requires an 'action' field \
          \(one of 'list', 'run', 'export', 'audit')."))
  Just action -> case action of
    -- list/run previously rode the deleted subprocess Regression
    -- handler. run now replays through the ghcide session (the only
    -- backend); list is pure store introspection rendered locally.
    "list"   -> listStored
    "run"    -> IdeBacked.withIdeSession ideRef pdRef
                  (IdeBacked.handlePropertyRun pdRef storeRef)
    "export" -> do
      pd    <- readIORef pdRef
      store <- readIORef storeRef
      QcExportTool.handle store pd (stripAction rawArgs)
    "audit"  -> do
      -- W6.8.2: the probes run through the ghcide session via the
      -- injected probe-runner (IdeBacked.ideQcProbe) — the legacy
      -- GhcSession boot is gone.
      store <- readIORef storeRef
      IdeBacked.withIdeSession ideRef pdRef $ \s ->
        PropertyAuditTool.handle
          (PropertyAuditTool.AuditQueries
             { PropertyAuditTool.aqProbe = IdeBacked.ideQcProbe pdRef s })
          store
          (stripAction rawArgs)
    other ->
      pure (Env.mkRefused
          (Env.mkErrorEnvelope Env.Validation
            ("Unknown ghc_property_store action: '" <> other
             <> "' (expected 'list', 'run', 'export', or 'audit').")))
  where
    listStored = do
      store <- readIORef storeRef
      props <- loadAll store
      pure (Env.mkOk (listResult props))

-- | Pure list-view payload (shared with the unit tests).
listResult :: [StoredProperty] -> Value
listResult props =
  let nullCount = length (filter (isNothing . spModule) props)
      base =
        [ "action" .= ("list" :: Text)
        , "count" .= length props
        , "properties" .= map renderStored props
        ]
      extra
            | nullCount > 0 =
                [ "null_module_count" .= nullCount
                , "null_module_hint" .=
                    ( T.pack (show nullCount)
                        <> " properties have no recorded module path; re-run "
                        <> "them via ghc_property(action=check, module=\"src/X.hs\") "
                        <> "to improve replay reliability." ::
                        Text
                    )
                ]
            | otherwise = []
  in A.object (base <> extra)



-- | Peek at the @action@ string without committing to a FromJSON parser.
actionField :: Value -> Maybe Text
actionField (Object o) = case KeyMap.lookup "action" o of
  Just (String s) -> Just s
  _               -> Nothing
actionField _ = Nothing

-- | Drop @action@ before delegating to handlers that reject unknown fields.
stripAction :: Value -> Value
stripAction (Object o) = Object (KeyMap.delete "action" o)
stripAction v          = v


schema :: Value
schema = Schema.discriminatedSchema "action"
  [ Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "list"
      , Schema.sbDescription       =
          "Inspect every stored property — returns count + entries \
          \with expression, module, cumulative pass count, and \
          \last-updated POSIX time."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "run"
      , Schema.sbDescription       =
          "Replay every persisted QuickCheck property as a regression \
          \suite. Per-property pass/fail under 'replays', total \
          \regression count under 'regressions'."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "export"
      , Schema.sbDescription       =
          "Materialise test/Spec.hs from the persisted store. The \
          \emitted file is exactly what 'cabal test' will replay in \
          \CI; use this to seed a project's regression net. \
          \Safe by default: refuses to overwrite a file that was not \
          \previously generated by this tool. Pass force=true to \
          \bypass the guard."
      , Schema.sbProperties        =
          [ ( "output_path"
            , object [ "type" .= ("string" :: Text)
                     , "description" .=
                         ("Target file path relative to project root. \
                          \Defaults to test/Spec.hs." :: Text) ] )
          , ( "force"
            , object [ "type" .= ("boolean" :: Text)
                     , "description" .=
                         ("Overwrite even if the target file was not \
                          \generated by this tool. Default: false." :: Text) ] )
          ]
      , Schema.sbRequired          = []
      }
  , Schema.SchemaBranch
      { Schema.sbDiscriminantValue = "audit"
      , Schema.sbDescription       =
          "Pairwise contradiction probe across the persisted property \
          \set. Reports any pair of laws that disagree on a shared \
          \counter-example so the agent can prune or refine the \
          \weaker law."
      , Schema.sbProperties        = []
      , Schema.sbRequired          = []
      }
  ]

-- | One stored property rendered for the list view (shared with the
-- unit tests that pin the store's JSON shape).
renderStored :: StoredProperty -> Value
renderStored sp =
  let base =
        [ "expression" .= spExpression sp
        , "module" .= spModule sp
        , "passed" .= spPassed sp
        , "cases" .= spCases sp
        , "updated" .= spUpdated sp
        ]
      -- #238: per-property hint when the module path is missing so
      -- the caller knows how to fix replay reliability.
      moduleHint = case spModule sp of
        Nothing ->
          [ "module_hint" .=
              ("module path not recorded; re-run via ghc_property(action=\
               \check, module=\"src/X.hs\") to improve replay reliability" :: Text)
          ]
        Just _ -> []
  in A.object (base <> moduleHint)
