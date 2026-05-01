-- | @ghc_modules@ — action-discriminated primitive that subsumes
-- 'HaskellFlows.Tool.AddModules' (@action: \"add\"@) and
-- 'HaskellFlows.Tool.RemoveModules' (@action: \"remove\"@).
--
-- Issue #94 Phase B introduces this as the consolidation target for
-- the per-verb tools. The two legacy tools remain registered for one
-- minor release (deprecation lifecycle per #99 Phase C) so existing
-- callers keep working unchanged. The new tool is a thin dispatcher:
-- both actions delegate to the same handlers the legacy tools use,
-- so behaviour is byte-identical.
--
-- Schema is action-discriminated:
--
-- @
--   { \"action\":  \"add\" \| \"remove\"
--   , \"modules\": [\"Foo.Bar\", \"Foo.Baz\"]   -- or \"Foo.Bar, Foo.Baz\"
--   , \"stanza\":  \"library\" \| \"test-suite\" \| ...   (optional)
--   , \"delete_files\": true \| false                   (remove only, optional)
--   , \"force\":        true \| false                   (remove only, optional)
--   }
-- @
--
-- The legacy tools' descriptions still document their full payload
-- shapes; this module only adds the @action@ discriminator on top
-- and delegates everything else.
module HaskellFlows.Tool.Modules
  ( descriptor
  , handle
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.AddModules    as AddModules
import qualified HaskellFlows.Tool.RemoveModules as RemoveModules
import HaskellFlows.Types (ProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcModules
    , tdDescription =
        "Manage exposed-modules / other-modules in the project's \
        \.cabal (and the matching .hs source files). \
        \action='add' registers new modules + scaffolds empty stubs; \
        \action='remove' de-registers and (with delete_files=true) \
        \removes the .hs files. Stanza selector controls which \
        \stanza is touched (default: library). Idempotent on add. \
        \Phase B successor to ghc_add_modules + ghc_remove_modules \
        \(issue #94)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["add", "remove"] :: [Text])
                  , "description" .=
                      ("Operation: 'add' registers + scaffolds stubs; \
                       \'remove' de-registers (delete_files=true also \
                       \removes the .hs)." :: Text)
                  ]
              , "modules" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Module names. Accepts a JSON array \
                       \[\"Foo.Bar\",\"Foo.Baz\"] (passed as a JSON \
                       \string) or a single string with comma-/whitespace-\
                       \separated names. Same parser as ghc_add_modules / \
                       \ghc_remove_modules." :: Text)
                  ]
              , "stanza" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional stanza selector. Same vocabulary as \
                       \ghc_add_modules: 'library' (default), \
                       \'test-suite[:NAME]', 'executable[:NAME]', \
                       \'benchmark[:NAME]'." :: Text)
                  ]
              , "delete_files" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("(action=remove only) Whether to also delete \
                       \the .hs files. Defaults to false — by default \
                       \we only de-register; the source files remain." :: Text)
                  ]
              , "force" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("(action=remove only) Bypass downstream-import \
                       \safety check. Defaults to false." :: Text)
                  ]
              ]
          , "required"             .= (["action", "modules"] :: [Text])
          , "additionalProperties" .= False
          ]
    }

-- | Dispatch on the @action@ discriminator and forward to the
-- legacy tool's 'handle' with the @action@ field stripped (so the
-- delegate's parser sees the same payload shape it always saw).
--
-- Behaviour-preserving: bytes returned by @ghc_modules
-- {action:add, …}@ match what @ghc_add_modules {…}@ would return
-- for the same payload. Same for remove. The cross-tool equivalence
-- test in Spec.hs locks this in.
handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseAction rawArgs of
  Left err     -> pure (Env.toolResponseToResult (refusal err))
  Right action -> do
    let inner = stripAction rawArgs
    case action of
      "add"    -> AddModules.handle    pd inner
      "remove" -> RemoveModules.handle pd inner
      other    -> pure (Env.toolResponseToResult
                          (refusal ("unknown action: " <> T.unpack other
                                    <> " (expected 'add' or 'remove')")))
  where
    parseAction :: Value -> Data.Aeson.Types.Parser Text
    parseAction = withObject "ModulesArgs" $ \o ->
      o .: "action"

    -- The legacy handlers reject 'additionalProperties' via their
    -- schema enforcement; strip 'action' so they don't see an
    -- unexpected field after we've consumed it. Any non-Object
    -- payload (e.g. arg parser already chose to error) is passed
    -- through unchanged so the delegate's own parser produces the
    -- canonical error.
    stripAction :: Value -> Value
    stripAction (Object o) = Object (KeyMap.delete "action" o)
    stripAction v          = v

    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))
