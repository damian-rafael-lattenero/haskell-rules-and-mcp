-- | @ghci_imports@ — list the imports currently in scope in the
-- GHCi session via @:show imports@.
module HaskellFlows.Tool.Imports
  ( descriptor
  , handle
  , parseImportsOutput
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session (Session, GhciResult (..), execute)
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_imports"
    , tdDescription =
        "List the imports currently in scope in the GHCi session. "
          <> "Useful for confirming which modules are already available "
          <> "before suggesting an `ghci_add_import`."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "additionalProperties" .= False
          ]
    }

handle :: Session -> Value -> IO ToolResult
handle sess _rawArgs = do
  res <- execute sess ":show imports"
  let imports = parseImportsOutput (grOutput res)
      payload = object
        [ "success" .= True
        , "count"   .= length imports
        , "imports" .= imports
        ]
  pure ToolResult
         { trContent = [ TextContent (encodeUtf8Text payload) ]
         , trIsError = False
         }

-- | One line per import; empty lines and the interactive prelude
-- marker @\"-- imported via the \\'base' package\"@ dropped.
parseImportsOutput :: Text -> [Text]
parseImportsOutput = filter keep . map T.strip . T.lines
  where
    keep ln =
      not (T.null ln)
      && not (T.isInfixOf "via the command line" ln)

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
