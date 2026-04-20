-- | @ghci_browse@ — parse @:browse <Module>@ output into a list of
-- exported names + their types. Coarse; one line per binding.
module HaskellFlows.Tool.Browse
  ( descriptor
  , handle
  , parseBrowseOutput
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session (Session, GhciResult (..), execute)
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_browse"
    , tdDescription =
        "Browse all names exported by a module via `:browse`. Returns "
          <> "one entry per top-level binding with its rendered type."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module" .= object [ "type" .= ("string" :: Text) ] ]
          , "required"             .= ["module" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype BrowseArgs = BrowseArgs { baModule :: Text }

instance FromJSON BrowseArgs where
  parseJSON = withObject "BrowseArgs" $ \o -> BrowseArgs <$> o .: "module"

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right (BrowseArgs m) -> do
    res <- execute sess (":browse " <> m)
    let entries = parseBrowseOutput (grOutput res)
        payload = object
          [ "success" .= True
          , "module"  .= m
          , "count"   .= length entries
          , "entries" .= entries
          ]
    pure ToolResult
           { trContent = [ TextContent (encodeUtf8Text payload) ]
           , trIsError = False
           }

-- | @:browse@ output is usually @name :: type@ per line.
parseBrowseOutput :: Text -> [Text]
parseBrowseOutput = filter (not . T.null) . map T.strip . T.lines

errorResult :: Text -> ToolResult
errorResult msg = ToolResult
  { trContent = [ TextContent (encodeUtf8Text (object
      [ "success" .= False, "error" .= msg ])) ]
  , trIsError = True
  }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
