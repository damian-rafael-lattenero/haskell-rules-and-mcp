-- | @ghci_toolchain_warmup@ — probe each optional binary the MCP
-- delegates to and cache availability in the response. Lean vs
-- the TS version (no background download infrastructure — we
-- install via ghcup, which has its own concurrency model) but
-- still useful because it triggers the cabal repl spawn early
-- so the FIRST real tool call doesn't eat the startup cost.
module HaskellFlows.Tool.ToolchainWarmup
  ( descriptor
  , handle
  ) where

import Data.Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (findExecutable)

import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_toolchain_warmup"
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

handle :: Value -> IO ToolResult
handle _rawArgs = do
  availabilities <- traverse probe
    [ "fourmolu", "ormolu", "hls", "haskell-language-server", "hoogle" ]
  let payload = object
        [ "success" .= True
        , "tools"   .= [ object [ "name" .= n, "available" .= av ]
                       | (n, av) <- availabilities ]
        ]
  pure ToolResult
         { trContent = [ TextContent (encodeUtf8Text payload) ]
         , trIsError = False
         }
  where
    probe :: Text -> IO (Text, Bool)
    probe n = do
      mP <- findExecutable (T.unpack n)
      pure (n, isJust mP)

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
