-- | Internal handler for the @warmup@ branch of @ghc_toolchain@.
--
-- Probes each optional binary the MCP delegates to and caches
-- availability in the response. Triggers the cabal repl spawn
-- early so the FIRST real tool call doesn't eat the startup cost.
--
-- Issue #94 Phase C retired the @ghc_toolchain_warmup@ wire surface;
-- 'HaskellFlows.Tool.Toolchain' is the single externally-advertised
-- tool. This module's 'handle' is the implementation
-- 'Toolchain.handle' forwards to when @action="warmup"@.
module HaskellFlows.Tool.ToolchainWarmup
  ( handle
  ) where

import Data.Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)


-- | Optional binaries the MCP probes. Every entry is *optional* —
-- missing binaries don't break the warmup, they just narrow the
-- set of downstream tools the agent can rely on.
optionalBinaries :: [Text]
optionalBinaries =
  [ "fourmolu", "ormolu", "hls", "haskell-language-server", "hoogle" ]

handle :: Value -> IO ToolResult
handle _rawArgs = do
  availabilities <- traverse probe optionalBinaries
  let missing = [ n | (n, False) <- availabilities ]
      payload = object
        [ "tools" .= [ object [ "name" .= n, "available" .= av ]
                     | (n, av) <- availabilities ]
        ]
      response = case missing of
        []    -> Env.mkOk payload
        names -> Env.withWarnings (map missingBinaryWarning names)
                   (Env.mkPartial payload)
  pure (Env.toolResponseToResult response)
  where
    probe :: Text -> IO (Text, Bool)
    probe n = do
      mP <- findExecutable (T.unpack n)
      pure (n, isJust mP)

-- | One warning per missing optional binary. Each warning carries
-- the binary name in 'wExtra' so an agent can programmatically
-- filter without re-parsing the message string.
missingBinaryWarning :: Text -> Env.Warning
missingBinaryWarning n = Env.Warning
  { Env.wKind    = Env.SlowPath
  , Env.wMessage = "optional binary '" <> n
                <> "' is unavailable; tools that delegate to it will return status='unavailable'"
  , Env.wExtra   = Just (object [ "binary" .= n ])
  }
