-- | @ghci_determinism@ — re-run a property N times with independent
-- QuickCheck seeds and check every run passes. Narrow
-- determinism sanity-check — the focus is \"does it pass
-- consistently\" rather than \"did it exercise every edge case\".
module HaskellFlows.Tool.Determinism
  ( descriptor
  , handle
  , DeterminismArgs (..)
  ) where

import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session (Session, GhciResult (..), runProperty)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..), parseQuickCheckOutput)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_determinism"
    , tdDescription =
        "Run a property 3 times (or `runs` param) to confirm every "
          <> "run passes. Any failing or non-passed run makes the tool "
          <> "return overall: false. Use to catch flakiness before "
          <> "committing a property."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "property" .= object [ "type" .= ("string" :: Text) ]
              , "runs"     .= object [ "type" .= ("integer" :: Text) ]
              ]
          , "required"             .= ["property" :: Text]
          , "additionalProperties" .= False
          ]
    }

data DeterminismArgs = DeterminismArgs
  { daProperty :: !Text
  , daRuns     :: !Int
  }
  deriving stock (Show)

instance FromJSON DeterminismArgs where
  parseJSON = withObject "DeterminismArgs" $ \o ->
    DeterminismArgs
      <$> o .:  "property"
      <*> o .:? "runs" .!= 3

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> do
    results <- replicateM (daRuns args) (runOnce (daProperty args))
    let allPassed = all isPassed results
        payload = object
          [ "success" .= allPassed
          , "runs"    .= daRuns args
          , "states"  .= map stateText results
          , "summary" .=
              ( if allPassed
                  then "All " <> T.pack (show (daRuns args)) <> " runs passed — no flakiness observed."
                  else "At least one run did not pass — property is flaky or broken."
              )
          ]
    pure ToolResult
           { trContent = [ TextContent (encodeUtf8Text payload) ]
           , trIsError = not allPassed
           }
  where
    runOnce expr = do
      res <- runProperty sess expr
      pure $ case res of
        Left _   -> QcUnparsed expr "sanitizer rejected"
        Right gr -> parseQuickCheckOutput expr (grOutput gr)

    isPassed QcPassed {} = True
    isPassed _           = False

    stateText QcPassed    {} = "passed" :: Text
    stateText QcFailed    {} = "failed"
    stateText QcGaveUp    {} = "gave_up"
    stateText QcException {} = "exception"
    stateText QcUnparsed  {} = "unparsed"

errorResult :: Text -> ToolResult
errorResult msg = ToolResult
  { trContent = [ TextContent (encodeUtf8Text (object
      [ "success" .= False, "error" .= msg ])) ]
  , trIsError = True
  }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
