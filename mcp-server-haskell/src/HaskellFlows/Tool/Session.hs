-- | @ghc_session@ — action-discriminated wayfinder + live-imports view
-- (wave 2b consolidation).
--
-- Subsumes ghc_workflow + ghc_toolchain + ghc_imports. Actions:
--
--   * @status@ / @help@ / @plan@ / @discover@ / @post-mortem@ — the
--     workflow engine (verbatim vocabulary)
--   * @toolchain@ — external-binary probe (ghc_toolchain action=status)
--   * @warmup@    — pre-resolve those binaries (ghc_toolchain action=warmup)
--   * @imports@   — the GHCi session's live import list
module HaskellFlows.Tool.Session
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
import qualified HaskellFlows.Tool.Imports as Imports
import qualified HaskellFlows.Tool.Toolchain as Toolchain
import qualified HaskellFlows.Tool.Workflow as Workflow

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcSession
    , tdDescription =
        "PURPOSE: Session wayfinding — phase-aware guidance, toolchain \
        \health, and the live import list. \
        \WHEN: start of session (action='status' then 'help'); before \
        \heavy work (action='toolchain' confirms cabal/ghc/hlint); \
        \action='warmup' pre-resolves binaries; action='imports' shows \
        \what the GHCi session has in scope; 'plan' turns a goal into a \
        \batchable chain; 'discover' ranks unused tools; 'post-mortem' \
        \retros the session. \
        \WHEN NOT: build/test execution — ghc_gate. \
        \PREREQUISITES: none. \
        \OUTPUT: per-action envelope of the merged verbs. \
        \SEE ALSO: ghc_batch executes a plan chain. \
        \Wave 2b successor to ghc_workflow + ghc_toolchain + ghc_imports."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= Act.actionEnumValues Act.sessionSpec
                  , "description" .=
                      ("Workflow verbs (status/help/plan/discover/\
                       \post-mortem), toolchain probes (toolchain/warmup), \
                       \or the live import list (imports)." :: Text)
                  ], "goal" .= object [ "type" .= ("string" :: Text), "description" .= ("plan/discover: the session's stated objective." :: Text) ]
              ]
          , "additionalProperties" .= False
          ]
    }

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = case parseEither (Act.parsePayloadAction Act.sessionSpec) rawArgs of
  Left err     -> pure (refusal err)
  Right action -> case action of
    Act.SessionToolchain -> Env.withResultAction "toolchain" <$> Toolchain.handle env (Act.setActionField "status" rawArgs)
    Act.SessionWarmup    -> Env.withResultAction "warmup" <$> Toolchain.handle env (Act.setActionField "warmup" rawArgs)
    Act.SessionImports   -> Env.withResultAction "imports" <$> Imports.handle env (Act.stripActionField rawArgs)
    Act.SessionWorkflow  -> Env.withResultAction (rawAction rawArgs) <$> Workflow.handle env rawArgs
  where
    asAction :: Text -> Value
    asAction a =
      case rawArgs of
        Object o -> Object (KeyMap.insert "action" (String a) o)
        v        -> v

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
