-- | @ghci_eval@ — Phase 3 tool. Evaluate an arbitrary Haskell expression.
--
-- Mirrors @mcp-server/src/tools/eval.ts@ with two important hardening
-- additions that come from the port:
--
-- 1. Output is capped at 'maxEvalBytes' characters. An agent asking for
--    @print [1..]@ will still cause the GHCi child to consume memory
--    until the process is killed, but the MCP server never hands its
--    client more than the cap, and reports 'truncated' so the agent
--    knows to narrow its query.
-- 2. The expression is routed through 'sanitizeExpression' — newlines and
--    the framing sentinel are rejected at the boundary so a crafted input
--    can't split a single @tools/call@ into two GHCi commands (which
--    would desync our single-sentinel response protocol) or falsify the
--    delimiter itself.
module HaskellFlows.Tool.Eval
  ( descriptor
  , handle
  , EvalArgs (..)
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
    { tdName        = "ghci_eval"
    , tdDescription =
        "Evaluate a Haskell expression in the persistent GHCi session. "
          <> "Output is capped at " <> T.pack (show maxEvalBytes)
          <> " characters. Input must be a single-line expression."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Expression to evaluate. Examples: \"1 + 2\", \
                       \\"map (+1) [1..5]\", \"fmap show Nothing\"" :: Text)
                  ]
              ]
          , "required"             .= ["expression" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype EvalArgs = EvalArgs
  { eaExpression :: Text
  }
  deriving stock (Show)

instance FromJSON EvalArgs where
  parseJSON = withObject "EvalArgs" $ \o ->
    EvalArgs <$> o .: "expression"

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (EvalArgs expr) -> do
    res <- evaluate sess expr
    case res of
      Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
      Right er    -> pure (renderResult er)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: EvalResult -> ToolResult
renderResult er =
  let payload =
        object
          [ "success"   .= erSuccess er
          , "output"    .= erOutput er
          , "truncated" .= erTruncated er
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = not (erSuccess er)
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
  ContainsNewline ->
    "expression must be a single line (no newline characters allowed)"
  ContainsSentinel ->
    "expression contains the internal framing sentinel and was rejected"
  EmptyInput ->
    "expression is empty"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
