-- | @ghc_edit@ — action-discriminated source editing (wave 2b
-- consolidation).
--
-- Subsumes ghc_refactor + ghc_add_import + ghc_apply_exports +
-- ghc_fix_warning + ghc_format. Actions:
--
--   * @rename_local@ / @extract_binding@ / @move_symbol@ /
--     @list_actions@ — the snapshot-verified refactor engine (verbatim
--     vocabulary, forwarded with the action intact)
--   * @import@       — add an import (session + file hint)
--   * @exports@      — apply a module export list
--   * @fix_warning@  — auto-patch one GHC warning
--   * @format@       — fourmolu/ormolu
module HaskellFlows.Tool.Edit
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
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.ApplyExports as ApplyExports
import HaskellFlows.Tool.Env (ToolEnv (..))
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Tool.Format as Format
import qualified HaskellFlows.Tool.Refactor as Refactor

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcEdit
    , tdDescription =
        "PURPOSE: Edit Haskell source with verification — the refactor \
        \engine snapshots, compile-verifies, and rolls back on failure. \
        \WHEN: rename a local binding (rename_local), extract a binding \
        \(extract_binding), move a top-level symbol (move_symbol), add \
        \an import (import), rewrite an export list (exports), auto-patch \
        \a GHC warning (fix_warning), or format (format). \
        \WHEN NOT: writing whole new modules — that is ghc_module. \
        \PREREQUISITES: the target module(s) in the active project. \
        \OUTPUT: {applied|preview, diff}; on compile error the file is \
        \restored from snapshot (atomic). \
        \PREREQUISITES: target module in the active project. \
        \SEE ALSO: ghc_module, ghc_check. \
        \Wave 2b successor to ghc_refactor + ghc_add_import + \
        \ghc_apply_exports + ghc_fix_warning + ghc_format."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= Act.actionEnumValues Act.editSpec
                  ]
              , "module_path" .= object [ "type" .= ("string" :: Text) ]
              , "old_name" .= object [ "type" .= ("string" :: Text) ]
              , "new_name" .= object [ "type" .= ("string" :: Text) ]
              , "name" .= object [ "type" .= ("string" :: Text) ]
              , "alias" .= object [ "type" .= ("string" :: Text) ]
              , "exports" .= object [ "type" .= ("string" :: Text) ]
              , "code" .= object [ "type" .= ("string" :: Text) ]
              , "message" .= object [ "type" .= ("string" :: Text) ]
              , "line" .= object [ "type" .= ("integer" :: Text) ]
              , "scope_line_start" .= object [ "type" .= ("integer" :: Text) ]
              , "scope_line_end" .= object [ "type" .= ("integer" :: Text) ]
              , "qualified" .= object [ "type" .= ("boolean" :: Text) ]
              , "write" .= object [ "type" .= ("boolean" :: Text) ]
              , "apply" .= object [ "type" .= ("boolean" :: Text) ]
              , "dry_run" .= object [ "type" .= ("boolean" :: Text) ]
              ]
          , "additionalProperties" .= False
          ]
    }

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = case parseEither (Act.parsePayloadAction Act.editSpec) rawArgs of
  Left err     -> pure (refusal err)
  Right action -> do
    let inner = Act.stripActionField rawArgs
    case action of
      Act.EditRenameLocal    -> Env.withResultAction "rename_local" <$> Refactor.handle env rawArgs
      Act.EditExtractBinding -> Env.withResultAction "extract_binding" <$> Refactor.handle env rawArgs
      Act.EditMoveSymbol       -> Env.withResultAction "move_symbol" <$> Refactor.handle env rawArgs
      Act.EditListActions    -> Env.withResultAction "list_actions" <$> Refactor.handle env rawArgs
      Act.EditImport         -> Env.withResultAction "import" <$> AddImport.handle env inner
      Act.EditExports        -> Env.withResultAction "exports" <$> ApplyExports.handle env inner
      Act.EditFixWarning     -> Env.withResultAction "fix_warning" <$> FixWarning.handle env inner
      Act.EditFormat         -> Env.withResultAction "format" <$> Format.handle env inner
  where
    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))
