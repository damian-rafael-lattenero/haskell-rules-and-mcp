-- | @ghci_quickcheck@ — run a QuickCheck property in the persistent GHCi.
--
-- Phase-4 port of @mcp-server/src/tools/quickcheck.ts@. Scope deliberately
-- narrower than the TS version for this first pass:
--
-- * No law-suggestion engine ('suggestFunctionProperties').
-- * No ambiguous-type-variable auto-hint.
-- * No persistence to the property store (regression will come when the
--   store itself is ported).
-- * No auto @load_all@ of the project — the agent is expected to call
--   'ghci_load' first.
--
-- What it does cover is the high-value path: take a property expression,
-- ensure @Test.QuickCheck@ is in scope, invoke @quickCheck@, and translate
-- the four observable QuickCheck states into a structured JSON payload.
--
-- Boundary safety is inherited from 'runProperty', which routes the
-- expression through 'sanitizeExpression' before any bytes reach GHCi.
module HaskellFlows.Tool.QuickCheck
  ( descriptor
  , handle
  , QuickCheckArgs (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.QuickCheck

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_quickcheck"
    , tdDescription =
        "Run a QuickCheck property against the current GHCi session. "
          <> "The property is passed directly to quickCheck, so it must be a "
          <> "value of type Testable (e.g. `\\x -> reverse (reverse x) == x`). "
          <> "Returns structured pass/fail/gave-up/exception output."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "property" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("QuickCheck-testable property expression. Examples: \
                       \\"\\\\(xs :: [Int]) -> reverse (reverse xs) == xs\", \
                       \\"prop_idempotent\"" :: Text)
                  ]
              ]
          , "required"             .= ["property" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype QuickCheckArgs = QuickCheckArgs
  { qaProperty :: Text
  }
  deriving stock (Show)

instance FromJSON QuickCheckArgs where
  parseJSON = withObject "QuickCheckArgs" $ \o ->
    QuickCheckArgs <$> o .: "property"

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (QuickCheckArgs prop) -> do
    res <- runProperty sess prop
    case res of
      Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
      Right gr    -> pure (renderResult (parseQuickCheckOutput prop (grOutput gr)))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: QuickCheckResult -> ToolResult
renderResult qr =
  let payload = case qr of
        QcPassed p n ->
          object
            [ "success"  .= True
            , "state"    .= ("passed" :: Text)
            , "property" .= p
            , "passed"   .= n
            ]
        QcFailed p n shr cex ->
          object
            [ "success"        .= False
            , "state"          .= ("failed" :: Text)
            , "property"       .= p
            , "passed"         .= n
            , "shrinks"        .= shr
            , "counterexample" .= cex
            ]
        QcException p err ->
          object
            [ "success"  .= False
            , "state"    .= ("exception" :: Text)
            , "property" .= p
            , "error"    .= err
            ]
        QcGaveUp p n disc ->
          object
            [ "success"   .= False
            , "state"     .= ("gave_up" :: Text)
            , "property"  .= p
            , "passed"    .= n
            , "discarded" .= disc
            , "hint"      .= ( "Too many inputs rejected by precondition (==>). \
                              \Consider relaxing the precondition or writing a \
                              \custom generator." :: Text)
            ]
        QcUnparsed p raw ->
          object
            [ "success"  .= False
            , "state"    .= ("unparsed" :: Text)
            , "property" .= p
            , "raw"      .= raw
            ]
      isErr = case qr of
        QcPassed _ _ -> False
        _            -> True
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = isErr
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
  ContainsNewline  -> "property must be a single line (no newline characters)"
  ContainsSentinel -> "property contains the internal framing sentinel and was rejected"
  EmptyInput       -> "property is empty"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
