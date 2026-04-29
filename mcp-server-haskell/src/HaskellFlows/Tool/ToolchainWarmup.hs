-- | @ghc_toolchain_warmup@ — probe each optional binary the MCP
-- delegates to and cache availability in the response. Lean vs
-- the TS version (no background download infrastructure — we
-- install via ghcup, which has its own concurrency model) but
-- still useful because it triggers the cabal repl spawn early
-- so the FIRST real tool call doesn't eat the startup cost.
--
-- Issue #90 Phase B: returns the unified envelope shape — the wire
-- payload now carries both 'status' (\"ok\" when every probed
-- binary is present, \"partial\" when ≥1 optional is missing) and
-- the legacy 'success' field for backwards compatibility during
-- the migration window.
module HaskellFlows.Tool.ToolchainWarmup
  ( descriptor
  , handle
  ) where

import Data.Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcToolchainWarmup
    , tdDescription =
        "Probe every optional toolchain binary (fourmolu, ormolu, hls, "
          <> "hoogle) and return availability so subsequent calls do not "
          <> "pay the lookup cost. Non-blocking; unavailable tools are "
          <> "reported cleanly."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "additionalProperties" .= False
          ]
    }

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
