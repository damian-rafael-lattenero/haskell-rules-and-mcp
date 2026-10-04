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
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Action as Act
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.Browse as Browse
import qualified HaskellFlows.Tool.Complete as Complete
import HaskellFlows.Tool.Env (ToolEnv (..))
import qualified HaskellFlows.Tool.Goto as Goto
import qualified HaskellFlows.Tool.Hole as Hole
import qualified HaskellFlows.Tool.Info as Info
import qualified HaskellFlows.Tool.Type as Type

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
handle env rawArgs = case parseEither (Act.parsePayloadAction Act.inspectSpec) rawArgs of
  Left err     -> pure (refusal err)
  Right action -> do
    let inner = Act.stripActionField rawArgs
    case action of
      Act.InspectType     -> Env.withResultAction "type" <$> Type.handle env inner
      Act.InspectHole     -> Env.withResultAction "hole" <$> Hole.handle env inner
      Act.InspectInfo     -> Env.withResultAction "info" <$> Info.handle env inner
      Act.InspectBrowse   -> Env.withResultAction "browse" <$> Browse.handle env inner
      Act.InspectComplete -> Env.withResultAction "complete" <$> Complete.handle env inner
      Act.InspectGoto     -> Env.withResultAction "goto" <$> Goto.handle env inner
  where
    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))
