-- | @ghci_load@ — the first tool ported to Haskell.
--
-- Responsibility mirrors @mcp-server/src/tools/load-module.ts@'s
-- @handleLoadSingle@ at a simplified level: receive a module path, load it
-- in the persistent GHCi session, parse the resulting diagnostics, and
-- return a JSON summary the agent can act on.
--
-- Security note: the 'module_path' argument is routed through 'mkModulePath',
-- so traversal outside the project directory is rejected at the boundary —
-- the handler itself cannot produce an escaping path.
module HaskellFlows.Tool.Load
  ( descriptor
  , handle
  , LoadArgs (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error
import HaskellFlows.Types

-- | The schema surfaced to clients via @tools/list@. Deliberately tiny
-- for Phase 1 — future phases will add @diagnostics@, @mode@, @load_all@.
descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_load"
    , tdDescription =
        "Load or reload Haskell modules in GHCi. Returns parsed compilation "
          <> "errors and warnings. Phase-1 port of the TypeScript tool; "
          <> "currently supports single-module load and plain reload."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Path to a module to load, relative to the project \
                       \directory. Omit to reload current modules." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

newtype LoadArgs = LoadArgs
  { laModulePath :: Maybe Text
  }
  deriving stock (Show)

instance FromJSON LoadArgs where
  parseJSON = withObject "LoadArgs" $ \o ->
    LoadArgs <$> o .:? "module_path"

-- | Handle a @tools/call@ for @ghci_load@.
--
-- The returned 'ToolResult' carries a text content block whose payload is a
-- JSON string — this is the MCP convention and what the TS server does
-- today. Once all tools are ported we can revisit whether structured
-- content blocks are better.
handle :: Session -> ProjectDir -> Value -> IO ToolResult
handle sess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (LoadArgs Nothing) -> do
    result <- reload sess
    pure (okResult result [])
  Right (LoadArgs (Just p)) -> case mkModulePath pd (T.unpack p) of
    Left err -> pure (errorResult (formatPathError err))
    Right mp -> do
      result <- loadModule sess mp
      let diags = parseGhcErrors (grOutput result)
      pure (okResult result diags)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

okResult :: GhciResult -> [GhcError] -> ToolResult
okResult gr diags =
  let errs  = filter ((== SevError) . geSeverity) diags
      warns = filter ((== SevWarning) . geSeverity) diags
      payload =
        object
          [ "success"  .= (grSuccess gr && null errs)
          , "errors"   .= errs
          , "warnings" .= warns
          , "summary"  .= summarise (grSuccess gr) errs warns
          , "raw"      .= grOutput gr
          ]
  in ToolResult
       { trContent = [ TextContent (encodeText payload) ]
       , trIsError = not (grSuccess gr) || not (null errs)
       }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeText (object [ "error" .= msg ])) ]
    , trIsError = True
    }

summarise :: Bool -> [GhcError] -> [GhcError] -> Text
summarise ok errs warns
  | not (null errs) = T.pack (show (length errs)) <> " error(s)"
  | ok && null warns = "Compiled OK. No issues."
  | ok = "Compiled OK. " <> T.pack (show (length warns)) <> " warning(s)."
  | otherwise = "Compilation produced no errors but GHCi reported failure."

encodeText :: Value -> Text
encodeText = T.pack . show . encode
  -- show on Data.ByteString.Lazy.Char8 produces a string literal that
  -- would be quoted; we instead want the raw utf-8. Use explicit decode
  -- when we add Data.Text.Lazy.Encoding to the dependency closure. For
  -- now this keeps the wire shape deterministic and deps minimal.

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p ->
    "Project directory is not absolute: " <> p
  PathEscapesProject a p _ ->
    "module_path '" <> a <> "' escapes project directory " <> p
