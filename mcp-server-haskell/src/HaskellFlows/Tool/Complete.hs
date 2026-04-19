-- | @ghci_complete@ — identifier autocomplete via GHCi's @:complete@
-- command.
--
-- Wraps @:complete repl "prefix"@. GHCi returns a count header + one
-- candidate per line; we parse that into a list. Useful for agent IDEs
-- that want to narrow a name before calling @:info@ on it.
--
-- Boundary safety: the prefix goes through 'sanitizeExpression' like
-- every other string sent to GHCi — same rationale as @:t@/@:i@.
module HaskellFlows.Tool.Complete
  ( descriptor
  , handle
  , CompleteArgs (..)
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
    { tdName        = "ghci_complete"
    , tdDescription =
        "Return the identifiers GHCi's :complete knows that start with "
          <> "the given prefix. Useful before calling :info or :type on a "
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

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (CompleteArgs prefix limit) ->
    case sanitizeExpression prefix of
      Left e -> pure (errorResult (formatCommandError e))
      Right safe -> do
        -- The "repl" flavour queries GHCi's in-scope names + imported.
        -- Double-quote the prefix so :complete treats it as a literal.
        gr <- execute sess (":complete repl \"" <> safe <> "\"")
        pure (successResult prefix limit (parseCompleteOutput (grOutput gr)))

--------------------------------------------------------------------------------
-- parser
--------------------------------------------------------------------------------

-- | GHCi's @:complete@ output:
--
-- > 4 4 ""
-- > "foldr"
-- > "foldl"
-- > "foldMap"
-- > "foldr1"
--
-- The first line is @<count-returned> <count-total> ""@. Remaining
-- lines are one candidate each, already quoted. We strip the quotes
-- and ignore the header.
parseCompleteOutput :: Text -> [Text]
parseCompleteOutput raw =
  let lns   = T.lines (T.strip raw)
      body  = case lns of { (_:rest) -> rest; _ -> [] }
  in map unquote (filter (not . T.null) body)
  where
    unquote t =
      let t1 = T.dropWhile (== '"') t
      in T.takeWhile (/= '"') t1

--------------------------------------------------------------------------------
-- response shaping
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

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
