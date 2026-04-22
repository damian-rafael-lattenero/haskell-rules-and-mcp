-- | @ghci_complete@ — Phase-2 tool (GHC-API migrated).
--
-- Returns in-scope identifiers that start with the given prefix.
-- Pre-migration this wrapped @:complete repl "prefix"@ and parsed
-- its framed count+list output; post-migration it queries
-- 'getNamesInScope' directly and filters in-process.
--
-- Boundary safety: prefix still routes through 'sanitizeExpression'
-- so the newline/sentinel/empty/too-large contract is identical.
module HaskellFlows.Tool.Complete
  ( descriptor
  , handle
  , CompleteArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC (Ghc, getNamesInScope)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (CommandError (..), sanitizeExpression)
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_complete"
    , tdDescription =
        "Return in-scope identifiers that start with the given prefix, "
          <> "via the GHC API. Useful before calling :info or :type on a "
          <> "candidate."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "prefix" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Prefix to complete. Example: \"fold\" returns \
                       \foldr, foldl, foldMap, ..." :: Text)
                  ]
              , "limit" .= object
                  [ "type"        .= ("integer" :: Text)
                  , "description" .=
                      ("Maximum candidates to return. Default 25, capped \
                       \at 200." :: Text)
                  ]
              ]
          , "required"             .= ["prefix" :: Text]
          , "additionalProperties" .= False
          ]
    }

data CompleteArgs = CompleteArgs
  { caPrefix :: !Text
  , caLimit  :: !Int
  }
  deriving stock (Show)

instance FromJSON CompleteArgs where
  parseJSON = withObject "CompleteArgs" $ \o -> do
    p <- o .:  "prefix"
    l <- o .:? "limit" .!= 25
    pure CompleteArgs { caPrefix = p, caLimit = clampLimit l }

clampLimit :: Int -> Int
clampLimit n
  | n <= 0    = 1
  | n > 200   = 200
  | otherwise = n

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (CompleteArgs prefix limit) ->
    case sanitizeExpression prefix of
      Left e -> pure (errorResult (formatCommandError e))
      Right safe -> do
        eRes <- try (withGhcSession ghcSess (queryCompletions safe))
        case eRes of
          Left (se :: SomeException) ->
            pure (errorResult (T.pack ("GHC API error: " <> show se)))
          Right cands ->
            pure (successResult prefix limit cands)

-- | Scan every name currently in the interactive context, keep the
-- ones whose occurrence name starts with the prefix. Sort + dedupe
-- to match the shape the subprocess @:complete@ produced.
queryCompletions :: Text -> Ghc [Text]
queryCompletions prefix = do
  names <- getNamesInScope
  let pfxStr = T.unpack prefix
      matches =
        [ s
        | n <- names
        , let s = occNameString (nameOccName n)
        , pfxStr `isPrefixOf` s
        ]
  pure (map T.pack (sort (nub matches)))

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

successResult :: Text -> Int -> [Text] -> ToolResult
successResult prefix limit candidates =
  let capped = take limit candidates
      payload =
        object
          [ "success"    .= True
          , "prefix"     .= prefix
          , "count"      .= length capped
          , "candidates" .= capped
          , "truncated"  .= (length candidates > limit)
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
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
  ContainsNewline  -> "prefix must be a single line (no newline characters)"
  ContainsSentinel -> "prefix contains the internal framing sentinel and was rejected"
  EmptyInput       -> "prefix is empty"
  InputTooLarge sz cap ->
    "prefix is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
