-- | @ghci_doc@ — Phase-2 tool (GHC-API migrated).
--
-- Looks up Haddock documentation for a name. Pre-migration wrapped
-- @:doc@ over stdio; post-migration calls 'GHC.getDocs' directly.
--
-- Packages without @-haddock@ still degrade gracefully: 'getDocs'
-- returns 'Left', which we surface as @{success: true, hasDoc: false}@.
-- Same shape as before, same 'success: true' invariant that
-- @FlowExploratory@ checks.
module HaskellFlows.Tool.Doc
  ( descriptor
  , handle
  , DocArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import GHC (Ghc, getDocs, getNamesInScope)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghci.Session (CommandError (..), sanitizeExpression)
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_doc"
    , tdDescription =
        "Look up Haddock documentation for a name via the GHC API. "
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

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (DocArgs nm) -> case sanitizeExpression nm of
    Left e     -> pure (errorResult (formatCommandError e))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryDoc safe))
      pure $ case eRes of
        Left (se :: SomeException) ->
          errorResult (T.pack ("GHC API error: " <> show se))
        Right Nothing ->
          noDocResult safe "Name not in scope"
        Right (Just Nothing) ->
          noDocResult safe
            "No Haddock available (package may have been built without -haddock)"
        Right (Just (Just t)) ->
          hasDocResult safe t

-- | Result shape:
--
-- * 'Nothing'          — name isn't in scope at all
-- * 'Just Nothing'     — name found but no doc (no -haddock, or no doc string)
-- * 'Just (Just txt)'  — doc text
queryDoc :: Text -> Ghc (Maybe (Maybe Text))
queryDoc nm = do
  names <- getNamesInScope
  let target = T.unpack nm
      matches =
        [ n
        | n <- names
        , occNameString (nameOccName n) == target
        ]
  case matches of
    []    -> pure Nothing
    (n:_) -> do
      result <- getDocs n
      pure . Just $ case result of
        Left _                   -> Nothing
        Right (Nothing, _)       -> Nothing
        Right (Just docStr, _)   -> Just (T.pack (showPprUnsafe docStr))

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

hasDocResult :: Text -> Text -> ToolResult
hasDocResult nm doc =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= True
        , "name"    .= nm
        , "hasDoc"  .= True
        , "doc"     .= T.strip doc
        ]))
      ]
    , trIsError = False
    }

noDocResult :: Text -> Text -> ToolResult
noDocResult nm reason =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= True
        , "name"    .= nm
        , "hasDoc"  .= False
        , "reason"  .= reason
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
  InputTooLarge sz cap ->
    "name is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
