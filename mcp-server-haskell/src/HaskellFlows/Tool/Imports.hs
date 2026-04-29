-- | @ghc_imports@ — Phase-6 tool (GHC-API migrated).
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

import GHC
  ( Ghc
  , InteractiveImport (IIDecl, IIModule)
  , getContext
  )
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcImports
    , tdDescription =
        "List the imports currently in the GHC session's interactive "
          <> "context. Useful for confirming which modules are already "
          <> "available before suggesting an ghc_add_import."
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
  pure $ Env.toolResponseToResult $ case eRes of
    Left (se :: SomeException) ->
      Env.mkFailed
        ((Env.mkErrorEnvelope Env.InternalError
            (T.pack ("GHC API error: " <> show se)))
              { Env.eeCause = Just (T.pack (show se)) })
    Right imports -> Env.mkOk (importsPayload imports)

queryImports :: Ghc [Text]
queryImports = map renderImport <$> getContext

renderImport :: InteractiveImport -> Text
renderImport = \case
  IIDecl decl  -> T.pack (showPprUnsafe decl)
  IIModule mn  -> "module " <> T.pack (showPprUnsafe mn)

-- | Result payload (pre-envelope shape preserved). Issue #90 Phase B
-- moves it under 'result' but keeps the field names so consumers
-- that read @count@ + @imports@ directly stay compatible during
-- the migration window.
importsPayload :: [Text] -> Value
importsPayload imports = object
  [ "count"   .= length imports
  , "imports" .= imports
  ]

-- | Pre-migration parser kept for the unit-test scaffolding. Retained
-- as a pure parser fixture so the tests can pin the text-shape
-- contract without a live session.
parseImportsOutput :: Text -> [Text]
parseImportsOutput = filter keep . map T.strip . T.lines
  where
    keep ln =
      not (T.null ln)
      && not (T.isInfixOf "via the command line" ln)
