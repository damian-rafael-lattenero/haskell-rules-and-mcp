-- | @ghc_property@ — action-discriminated property-first pipeline
-- (wave 2b consolidation).
--
-- Subsumes ghc_quickcheck + ghc_property_store + ghc_arbitrary into a
-- single tool. Actions:
--
--   * @check@     — run one property (QuickCheck); @runs >= 2@ routes
--                   to the determinism detector
--   * @arbitrary@ — generate an Arbitrary template for a type
--   * @list@ / @run@ / @export@ / @audit@ — the persisted store
--     (forwards verbatim; these actions keep the store's own
--     vocabulary)
module HaskellFlows.Tool.Property
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
import qualified HaskellFlows.Tool.PropertyStore as PropertyStore

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcProperty
    , tdDescription =
        "PURPOSE: The property-first loop — check a law, generate \
        \Arbitrary instances, and replay/audit the persisted store. \
        \WHEN: action='check' with a property expression (passes \
        \auto-persist to the store); action='arbitrary' with a \
        \type_name; action='run' replays every persisted property; \
        \action='list'/'export'/'audit' manage the store. \
        \runs >= 2 on 'check' routes to the determinism detector. \
        \WHEN NOT: one-off value checks — ghc_eval. \
        \PREREQUISITES: QuickCheck in the test-suite for the anchor module. \
        \OUTPUT: per-action envelope of the merged verbs. \
        \SEE ALSO: ghc_suggest proposes the laws. \
        \Wave 2b successor to ghc_quickcheck + ghc_property_store + \
        \ghc_arbitrary."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= Act.actionEnumValues Act.propertySpec
                  , "description" .=
                      ("'check' runs one property (default); 'arbitrary' \
                       \generates a template; list/run/export/audit are \
                       \the persisted store verbs." :: Text)
                  ]
              , "property" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Property expression for action='check'." :: Text)
                  ]
              , "module" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Module anchoring the property." :: Text)
                  ]
              , "runs" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Repeat count; >= 2 enables determinism detection." :: Text)
                  ]
              , "type_name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Type for action='arbitrary'." :: Text)
                  ]
              , "target_module" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Where to anchor the Arbitrary template." :: Text)
                  ]
              , "output_path" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("export: destination file." :: Text)
                  ]
              , "force" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("export: overwrite an existing file." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = case parseEither (Act.parsePayloadAction Act.propertySpec) rawArgs of
  Left err     -> pure (refusal err)
  Right action -> do
    let inner = Act.stripActionField rawArgs
        store = PropertyStore.handle env . (`Act.setActionField` rawArgs)
    case action of
      Act.PropertyCheck     -> Env.withResultAction "check" <$> routeCheck env inner
      -- Unreachable since W6.5: routeIde serves arbitrary via
      -- IdeBacked.handlePropertyArbitrary. Same backstop shape as
      -- routeCheck below.
      Act.PropertyArbitrary -> routeArbitraryBackstop
      Act.PropertyList      -> Env.withResultAction "list" <$> store "list"
      Act.PropertyRun       -> Env.withResultAction "run" <$> store "run"
      Act.PropertyExport    -> Env.withResultAction "export" <$> store "export"
      Act.PropertyAudit     -> Env.withResultAction "audit" <$> store "audit"
  where
    withAction :: Text -> Value
    withAction a =
      case rawArgs of
        Object o -> Object (KeyMap.insert "action" (String a) o)
        v        -> v

    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))

-- | Unreachable-response for the routeIde-served arbitrary action.
routeArbitraryBackstop :: IO ToolResponse
routeArbitraryBackstop =
  pure
    ( Env.mkFailed
        ( Env.mkErrorEnvelope
            Env.InternalError
            ( "ghc_property(action=arbitrary) is served by the in-process "
                <> "ghcide route; reaching this handler is a dispatch regression"
            )
        )
    )

-- | Unreachable since the F1 strangler completed: the Server routes
-- every ghc_property(action=check) through 'IdeBacked.routeIde'
-- (handlePropertyCheck), which handles runs >= 2 internally via
-- qcExpr rendering. Kept only as the exhaustive-dispatch backstop.
routeCheck :: ToolEnv -> Value -> IO ToolResponse
routeCheck _ _ =
  pure
    ( Env.mkFailed
        ( Env.mkErrorEnvelope
            Env.InternalError
            ( "property check is served by the in-process ghcide route; "
                <> "reaching this handler is a dispatch regression"
            )
        )
    )
