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
-- (Note: 'HaskellFlows.Tool.PropertyLifecycle' had the same
-- shape as @action=list@ on the legacy 'ghc_regression'; the
-- consolidated @list@ branch routes to 'Regression.handle' with
-- @action=list@ so the wire shape (including the @action@ field)
-- is byte-identical to the legacy 'ghc_regression(action=list)'
-- caller. 'PropertyLifecycle.handle' is no longer reachable through
-- this surface but is kept exported because some unit tests still
-- exercise it directly.)
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
  ( descriptor
  , handle
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (IORef, readIORef)
import Data.Text (Text)

import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.Schema as Schema
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool
import qualified HaskellFlows.Tool.QuickCheckExport as QcExportTool
import qualified HaskellFlows.Tool.Regression as RegressionTool
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Types (ProjectDir)

-- | #275: dispatch a @ghc_property_store@ call to the right delegate based on
-- the @action@ discriminator. Dependencies are injected: @startSession@ lazily
-- boots the GHC session (list / run / audit need it; export does not),
-- @storeRef@ + @pdRef@ are the server's refs. @list@ / @run@ keep the @action@
-- field (Regression parses it); @export@ / @audit@ strip it.
handle :: ToolEnv -> Value -> IO ToolResponse
handle env = runHandle (teSession env) (teStoreRef env) (teProjectDirRef env)

runHandle :: IO GhcSession -> IORef Store -> IORef ProjectDir -> Value -> IO ToolResponse
runHandle startSession storeRef pdRef rawArgs = case actionField rawArgs of
  Nothing ->
    pure (Env.mkRefused
        (Env.mkErrorEnvelope Env.MissingArg
          "ghc_property_store requires an 'action' field \
          \(one of 'list', 'run', 'export', 'audit')."))
  Just action -> case action of
    "list"   -> regression
    "run"    -> regression
    "export" -> do
      pd    <- readIORef pdRef
      store <- readIORef storeRef
      QcExportTool.handle store pd (stripAction rawArgs)
    "audit"  -> do
      sess  <- startSession
      store <- readIORef storeRef
      PropertyAuditTool.handle store sess (stripAction rawArgs)
    other ->
      pure (Env.mkRefused
          (Env.mkErrorEnvelope Env.Validation
            ("Unknown ghc_property_store action: '" <> other
             <> "' (expected 'list', 'run', 'export', or 'audit').")))
  where
    regression = do
      sess  <- startSession
      store <- readIORef storeRef
      RegressionTool.handle store sess rawArgs

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

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcPropertyStore
    , tdDescription =
        "PURPOSE: Inspect, replay, export, or audit the persisted property \
        \store. \
        \WHEN: action='list' (one entry per stored property); \
        \action='run' (replay all as a regression suite); \
        \action='export' (materialise test/Spec.hs); action='audit' \
        \(pairwise contradiction detector across stored laws). \
        \WHEN NOT: ghc_quickcheck to add a new property; ghc_gate for \
        \the full pre-push run. \
        \PREREQUISITES: a .haskell-flows/properties.json (populated by \
        \ghc_quickcheck passes). \
        \OUTPUT: per-action {count, properties|results|files_written|findings}. \
        \SEE ALSO: ghc_quickcheck, ghc_gate. \
        \(#94 Phase C step 6 successor to ghc_property_lifecycle + \
        \ghc_regression + ghc_quickcheck_export + ghc_property_audit.)"
    , tdInputSchema = schema
    }

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
