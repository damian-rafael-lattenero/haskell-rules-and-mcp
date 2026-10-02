-- | @ghc_module@ — action-discriminated module authorship (wave 2b
-- consolidation).
--
-- Subsumes ghc_modules + ghc_scratch. Actions:
--
--   * @add@ / @remove@ — register/de-register modules in the .cabal
--     (verbatim ghc_modules vocabulary, forwarded with the action)
--   * @write@ / @check@ / @list@ / @show@ / @clear@ / @promote@ — the
--     persistent scratchpad canvas (verbatim vocabulary)
module HaskellFlows.Tool.ModuleMgmt
  ( descriptor
  , handle
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Aeson.KeyMap as KM
import qualified HaskellFlows.Mcp.Action as Act
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.Env (ToolEnv (..))
import qualified HaskellFlows.Tool.Modules as Modules
import qualified HaskellFlows.Tool.Scratch as Scratch

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcModule
    , tdDescription =
        "PURPOSE: Module authorship — register modules in the .cabal and \
        \grow them from the persistent scratchpad. \
        \WHEN: action='add' scaffolds stubs + registers; 'remove' \
        \de-registers; 'write' records a hypothesis on the canvas; \
        \'check' type-checks it in project context; 'promote' splices a \
        \verified entry into a real module (snapshot + compile-verify). \
        \WHEN NOT: edits to code that already compiles — ghc_edit. \
        \PREREQUISITES: a .cabal in the active project. \
        \OUTPUT: per-action envelope of the merged verbs. \
        \SEE ALSO: ghc_edit for changes to code that already compiles. \
        \Wave 2b successor to ghc_modules + ghc_scratch."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= Act.actionEnumValues Act.moduleSpec
                  ]
              , "modules" .= object [ "type" .= ("string" :: Text) ]
              , "stanza" .= object [ "type" .= ("string" :: Text) ]
              , "id" .= object [ "type" .= ("string" :: Text) ]
              , "code" .= object [ "type" .= ("string" :: Text) ]
              , "kind" .= object [ "type" .= ("string" :: Text) ]
              , "note" .= object [ "type" .= ("string" :: Text) ]
              , "binding_name" .= object [ "type" .= ("string" :: Text) ]
              , "imports" .= object [ "type" .= ("string" :: Text) ]
              , "module" .= object [ "type" .= ("string" :: Text) ]
              , "target_module" .= object [ "type" .= ("string" :: Text) ]
              , "target_line" .= object [ "type" .= ("integer" :: Text) ]
              , "delete_files" .= object [ "type" .= ("boolean" :: Text) ]
              , "force" .= object [ "type" .= ("boolean" :: Text) ]
              , "confirm" .= object [ "type" .= ("boolean" :: Text) ]
              ]
          , "additionalProperties" .= False
          ]
    }

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = case parseEither (Act.parsePayloadAction Act.moduleSpec) rawArgs of
  Left err     -> pure (refusal err)
  Right action -> case action of
    Act.ModuleAdd     -> Env.withResultAction "add" <$> Modules.handle env rawArgs
    Act.ModuleRemove  -> Env.withResultAction "remove" <$> Modules.handle env rawArgs
    Act.ModuleScratch -> Env.withResultAction (rawAction rawArgs) <$> Scratch.handle env rawArgs
  where
    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))

-- | The raw @action@ string of the request, for valve (sub-handler
-- verb) provenance tagging.
rawAction :: Value -> Text
rawAction (Object o) = case KM.lookup "action" o of
  Just (String t) -> t
  _               -> "list"
rawAction _ = "list"
