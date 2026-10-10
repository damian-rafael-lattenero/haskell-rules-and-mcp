-- | @ghc_inspect@ — action-discriminated read-only introspection
-- (wave 2b consolidation).
--
-- Subsumes the thin read-only tools: ghc_type, ghc_hole, ghc_info,
-- ghc_browse, ghc_complete, ghc_goto, ghc_doc. Actions mirror the old
-- tool names.
module HaskellFlows.Tool.Inspect
  ( descriptor
  , handle
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Action as Act
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.Env (ToolEnv)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcInspect
    , tdDescription =
        "PURPOSE: Read-only introspection — types, holes, definitions, \
        \docs, completions. \
        \WHEN: action='type' (expression type), 'hole' (valid fits for a \
        \typed hole), 'info' (definition site + instances), 'browse' \
        \(module exports), 'complete' (prefix completion), 'goto' \
        \(definition jump), 'doc' (Haddock block). \
        \WHEN NOT: your host's LSP already surfaces most of this to \
        \humans — reach here when the agent loop needs it inline. \
        \PREREQUISITES: a loaded session for project-scope queries. \
        \OUTPUT: per-action envelope of the merged read verbs. \
        \SEE ALSO: ghc_check, ghc_eval. \
        \Wave 2b successor to the seven thin read tools."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= Act.actionEnumValues Act.inspectSpec
                  ]
              , "expression" .= object [ "type" .= ("string" :: Text) ]
              , "module_path" .= object [ "type" .= ("string" :: Text) ]
              , "name" .= object [ "type" .= ("string" :: Text) ]
              , "module" .= object [ "type" .= ("string" :: Text) ]
              , "hole_name" .= object [ "type" .= ("string" :: Text) ]
              , "prefix" .= object [ "type" .= ("string" :: Text) ]
              , "limit" .= object [ "type" .= ("integer" :: Text) ]
              ]
          , "additionalProperties" .= False
          ]
    }

handle :: ToolEnv -> Value -> IO ToolResponse
handle _ rawArgs = case parseEither (Act.parsePayloadAction Act.inspectSpec) rawArgs of
  Left err -> pure (refusal err)
  -- Every inspect action executes in IdeBacked.handleInspect* via
  -- Server routeIde (the only backend since the F1 strangler
  -- completed). The arms are unreachable backstops keeping the
  -- action table exhaustive — a live arrival means dispatch broke.
  Right action -> dispatchRegressionBackstop (actionName action)
  where
    actionName = \case
      Act.InspectType     -> "type"
      Act.InspectHole     -> "hole"
      Act.InspectInfo     -> "info"
      Act.InspectBrowse   -> "browse"
      Act.InspectComplete -> "complete"
      Act.InspectGoto     -> "goto"

    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))

-- | Unreachable-response for the six routeIde-served actions.
dispatchRegressionBackstop :: Text -> IO ToolResponse
dispatchRegressionBackstop act =
  pure
    ( Env.mkFailed
        ( Env.mkErrorEnvelope
            Env.InternalError
            ( "ghc_inspect(action=" <> act <> ") is served by the in-process "
                <> "ghcide route; reaching this handler is a dispatch regression"
            )
        )
    )
