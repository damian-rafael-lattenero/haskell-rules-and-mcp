-- | @ghci_doc@ — look up Haddock documentation for a name via GHCi's
-- @:doc@ command.
--
-- Requires the target package to have been built with @-haddock@ (the
-- default since GHC 9.0 on modern cabal setups). Missing docs degrade
-- to a structured \"no docs\" response rather than failing — common
-- and not a tool error.
module HaskellFlows.Tool.Doc
  ( descriptor
  , handle
  , DocArgs (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_doc"
    , tdDescription =
        "Look up Haddock documentation for a name via GHCi's :doc. "
          <> "Returns the doc block as plain text. If the hosting "
          <> "package was built without -haddock or the name has no "
          <> "doc, reports that cleanly without failing."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name to look up. Examples: \"map\", \"Functor\", \
                       \\"(++)\"." :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype DocArgs = DocArgs
  { daName :: Text
  }
  deriving stock (Show)

instance FromJSON DocArgs where
  parseJSON = withObject "DocArgs" $ \o ->
    DocArgs <$> o .: "name"

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (DocArgs nm) -> case sanitizeExpression nm of
    Left e     -> pure (errorResult (formatCommandError e))
    Right safe -> do
      gr <- execute sess (":doc " <> safe)
      pure (renderResult safe gr)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: Text -> GhciResult -> ToolResult
renderResult nm gr =
  let raw = grOutput gr
      missing = "No documentation available"
      empty   = T.null (T.strip raw)
  in if not (grSuccess gr) || empty || missing `T.isInfixOf` raw
       then ToolResult
              { trContent = [ TextContent (encodeUtf8Text (object
                  [ "success" .= True
                  , "name"    .= nm
                  , "hasDoc"  .= False
                  , "reason"  .= ( if empty
                                    then "GHCi returned no output for :doc"
                                    else T.strip raw )
                  ]))
                ]
              , trIsError = False
              }
       else ToolResult
              { trContent = [ TextContent (encodeUtf8Text (object
                  [ "success"  .= True
                  , "name"     .= nm
                  , "hasDoc"   .= True
                  , "doc"      .= T.strip raw
                  ]))
                ]
              , trIsError = False
              }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline  -> "name must be a single line (no newline characters)"
  ContainsSentinel -> "name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "name is empty"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
