-- | @ghc_toolchain@ — action-discriminated probe / warm-up of the
-- external binaries the MCP delegates to (cabal, ghc, hlint,
-- fourmolu, ormolu, hoogle, hls).
--
-- Issue #94 Phase C consolidation: subsumes the per-verb
-- 'GhcToolchainStatus' + 'GhcToolchainWarmup' constructors into a
-- single tool with an @action@ discriminator
-- (@status@ \| @warmup@).
--
-- @
--   { \"action\": \"status\" \| \"warmup\" }
-- @
--
-- Behaviour-preserving thin dispatcher: both branches forward to the
-- existing internal handlers
-- ('HaskellFlows.Tool.ToolchainStatus.handle' /
--  'HaskellFlows.Tool.ToolchainWarmup.handle') after stripping the
-- @action@ field, so the response shape on the wire is unchanged.
module HaskellFlows.Tool.Toolchain
  ( descriptor
  , handle
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.ToolchainStatus as ToolchainStatus
import qualified HaskellFlows.Tool.ToolchainWarmup as ToolchainWarmup

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcToolchain
    , tdDescription =
        "Probe or warm up the external toolchain (cabal, ghc, hlint, \
        \fourmolu, ormolu, hoogle, hls). action='status' (default) \
        \reports availability + version of every binary; \
        \action='warmup' pre-resolves them on PATH so the first \
        \tool call that needs them does not pay the lookup cost. \
        \Phase C successor to ghc_toolchain_status + \
        \ghc_toolchain_warmup (issue #94)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["status", "warmup"] :: [Text])
                  , "description" .=
                      ("Operation: 'status' (default) reports each \
                       \binary's path + version; 'warmup' resolves \
                       \them on PATH ahead of time." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

-- | Dispatch on @action@ (defaulting to @"status"@) and forward
-- to the existing handler.  The legacy handlers were never
-- @ProjectDir@-aware, so this dispatcher takes no @ProjectDir@.
handle :: Value -> IO ToolResult
handle rawArgs = case parseEither parseAction rawArgs of
  Left err     -> pure (Env.toolResponseToResult (refusal err))
  Right action -> do
    let inner = stripAction rawArgs
    case action of
      "status" -> ToolchainStatus.handle inner
      "warmup" -> ToolchainWarmup.handle inner
      other    -> pure (Env.toolResponseToResult
                          (refusal ("unknown action: " <> T.unpack other
                                    <> " (expected 'status' or 'warmup')")))
  where
    -- Default action = "status".  Mirrors the old ghc_toolchain_status
    -- (the no-arg invocation) so callers that pass an empty object
    -- get the natural read path.
    parseAction :: Value -> Parser Text
    parseAction v = case v of
      Object o -> case KeyMap.lookup "action" o of
        Just (String s) -> pure s
        Just _          -> fail "'action' must be a string"
        Nothing         -> pure "status"
      _ -> pure "status"

    stripAction :: Value -> Value
    stripAction (Object o) = Object (KeyMap.delete "action" o)
    stripAction v          = v

    refusal :: String -> Env.ToolResponse
    refusal msg =
      Env.mkRefused (Env.mkErrorEnvelope Env.Validation (T.pack msg))
