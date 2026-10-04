-- | @ghc_check@ — action-discriminated verification gate
-- (wave 2b consolidation).
--
-- Subsumes the load / check_module / check_project / lint verbs into a
-- single tool with an @action@ discriminator
-- (@load@ | @module@ | @project@ | @lint@).
--
-- Behaviour-preserving thin dispatcher: every branch forwards to the
-- existing internal handler after stripping the @action@ field, so the
-- response shape on the wire is unchanged.
module HaskellFlows.Tool.Check
  ( descriptor
  , handle
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Action as Act
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.Env (ToolEnv (..))
import qualified HaskellFlows.Tool.Lint as Lint

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcCheck
    , tdDescription =
        "PURPOSE: The verification gate — load a module, gate one \
        \module, gate the whole project, or lint. \
        \WHEN: after every write cycle; action='module' after editing \
        \one file; action='project' before wider chains; action='lint' \
        \matches CI hygiene; action='load' boots the session and \
        \returns the cleanest error surface for one module. \
        \WHEN NOT: pre-push finalizer — that is ghc_gate. \
        \PREREQUISITES: a scaffolded project (ghc_project). \
        \OUTPUT: each action returns the legacy verb's envelope. \
        \SEE ALSO: ghc_gate, ghc_edit. \
        \Wave 2b successor to ghc_load + ghc_check_module + \
        \ghc_check_project + ghc_lint."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= Act.actionEnumValues Act.checkSpec
                  , "description" .=
                      ("'load' boots the session on one module; 'module' \
                       \is the strict per-module gate; 'project' gates \
                       \every module; 'lint' runs hlint (matches CI)." :: Text)
                  ]
              , "module_path" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Relative module path (load/module/lint)." :: Text)
                  ]
              , "path" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("File or directory for lint." :: Text)
                  ]
              , "diagnostics" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("load: surface hole/warning diagnostics in the envelope." :: Text)
                  ]
              , "warnings_block" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("module/project: treat warnings as gate failures." :: Text)
                  ]
              , "fail_fast" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("project: stop at the first failing module." :: Text)
                  ]
              , "timeout_seconds" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("project: per-module gate budget." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = case parseEither (Act.parsePayloadAction Act.checkSpec) rawArgs of
  Left err     -> pure (refusal err)
  Right action -> do
    let inner = Act.stripActionField rawArgs
    case action of
      -- load/module/project execute in IdeBacked.handleCheck{Load,
      -- Module,Project} via Server routeIde (the only backend since
      -- the F1 strangler completed). These arms are unreachable
      -- backstops keeping the action table exhaustive.
      Act.CheckLoad    -> dispatchRegressionBackstop "load"
      Act.CheckModule  -> dispatchRegressionBackstop "module"
      Act.CheckProject -> dispatchRegressionBackstop "project"
      Act.CheckLint    -> Env.withResultAction "lint" <$> Lint.handle env inner
  where
    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))

-- | Unreachable-response for the three routeIde-served actions.
dispatchRegressionBackstop :: Text -> IO ToolResponse
dispatchRegressionBackstop act =
  pure
    ( Env.mkFailed
        ( Env.mkErrorEnvelope
            Env.InternalError
            ( "ghc_check(action=" <> act <> ") is served by the in-process "
                <> "ghcide route; reaching this handler is a dispatch regression"
            )
        )
    )
