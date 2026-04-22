-- | @ghci_imports@ — Phase-6 tool (GHC-API migrated).
--
-- List the imports currently in the interactive context via
-- 'GHC.getContext'. Pre-migration wrapped @:show imports@; post
-- migration the GhcSession's interactive context is authoritative.
module HaskellFlows.Tool.Imports
  ( descriptor
  , handle
  , parseImportsOutput
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import GHC
  ( Ghc
  , InteractiveImport (IIDecl, IIModule)
  , getContext
  )
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_imports"
    , tdDescription =
        "List the imports currently in the GHC session's interactive "
          <> "context. Useful for confirming which modules are already "
          <> "available before suggesting an ghci_add_import."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "additionalProperties" .= False
          ]
    }

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess _rawArgs = do
  eRes <- try (withGhcSession ghcSess queryImports)
  pure $ case eRes of
    Left (se :: SomeException) -> errorResult (T.pack ("GHC API error: " <> show se))
    Right imports              -> successResult imports

queryImports :: Ghc [Text]
queryImports = do
  ctx <- getContext
  pure (map renderImport ctx)

renderImport :: InteractiveImport -> Text
renderImport = \case
  IIDecl decl  -> T.pack (showPprUnsafe decl)
  IIModule mn  -> "module " <> T.pack (showPprUnsafe mn)

successResult :: [Text] -> ToolResult
successResult imports =
  let payload = object
        [ "success" .= True
        , "count"   .= length imports
        , "imports" .= imports
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

errorResult :: Text -> ToolResult
errorResult msg = ToolResult
  { trContent = [ TextContent (encodeUtf8Text (object
      [ "success" .= False, "error" .= msg ])) ]
  , trIsError = True
  }

-- | Legacy parser kept for unit-test back-compat. Retired with the
-- subprocess-ghci backing in Phase 7.
parseImportsOutput :: Text -> [Text]
parseImportsOutput = filter keep . map T.strip . T.lines
  where
    keep ln =
      not (T.null ln)
      && not (T.isInfixOf "via the command line" ln)

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
