-- | @ghci_regression@ — replay every persisted QuickCheck property as a
-- regression suite.
--
-- Properties are auto-saved by 'HaskellFlows.Tool.QuickCheck' the first
-- time they pass. This tool lets the agent re-verify the whole suite in
-- one call — essential at the start of a session to confirm nothing
-- drifted, or after a large refactor.
--
-- Actions:
--
-- * @list@ — return the stored property inventory without running
--   anything. Useful for introspection.
-- * @run@ (default) — execute every stored property. Aggregates the
--   per-property outcomes into a summary + list of regressions.
--
-- No new property is ever persisted by this tool — it's a
-- /verification/ layer, not a capture layer.
module HaskellFlows.Tool.Regression
  ( descriptor
  , handle
  , RegressionArgs (..)
  , Action (..)
    -- * Reusable runners for other tools (e.g. Tool.Gate)
  , Replay (..)
  , runOne
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  )
import HaskellFlows.Ghci.Session
  ( Session
  , GhciResult (..)
  , runProperty
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_regression"
    , tdDescription =
        "Replay every persisted QuickCheck property as a regression "
          <> "suite. Actions: 'list' (inspect the store without running), "
          <> "'run' (execute all). Properties are auto-persisted by "
          <> "ghci_quickcheck on first pass."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["list", "run"] :: [Text])
                  , "description" .= ("Default: 'run'." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data Action = ActList | ActRun
  deriving stock (Eq, Show)

newtype RegressionArgs = RegressionArgs
  { raAction :: Action
  }
  deriving stock (Show)

instance FromJSON RegressionArgs where
  parseJSON = withObject "RegressionArgs" $ \o -> do
    mAct <- o .:? "action"
    a <- case mAct :: Maybe Text of
      Nothing     -> pure ActRun
      Just "list" -> pure ActList
      Just "run"  -> pure ActRun
      Just other  -> fail ("unknown action: " <> T.unpack other)
    pure (RegressionArgs a)

handle :: Store -> Session -> Value -> IO ToolResult
handle store sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (RegressionArgs a) -> do
    props <- loadAll store
    case a of
      ActList -> pure (listResult props)
      ActRun  -> do
        results <- mapM (runOne sess) props
        pure (runResult results)

--------------------------------------------------------------------------------
-- running
--------------------------------------------------------------------------------

-- | One stored property's replay result, ready for JSON shaping.
data Replay = Replay
  { rpStored :: !StoredProperty
  , rpResult :: !QuickCheckResult
  }

runOne :: Session -> StoredProperty -> IO Replay
runOne sess sp = do
  res <- runProperty sess (spExpression sp)
  let qr = case res of
        Left _   -> QcUnparsed (spExpression sp)
                     "boundary-sanitiser rejected the stored expression"
        Right gr -> parseQuickCheckOutput (spExpression sp) (grOutput gr)
  pure Replay { rpStored = sp, rpResult = qr }

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

listResult :: [StoredProperty] -> ToolResult
listResult props =
  let payload =
        object
          [ "success"    .= True
          , "action"     .= ("list" :: Text)
          , "count"      .= length props
          , "properties" .= map renderStored props
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

runResult :: [Replay] -> ToolResult
runResult replays =
  let total        = length replays
      regressions  = filter (not . isPass . rpResult) replays
      regressed    = length regressions
      payload =
        object
          [ "success"     .= (regressed == 0)
          , "action"      .= ("run" :: Text)
          , "total"       .= total
          , "passed"      .= (total - regressed)
          , "regressions" .= map renderRegression regressions
          , "summary"     .= summarise total regressed
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = regressed > 0
       }

isPass :: QuickCheckResult -> Bool
isPass (QcPassed _ _) = True
isPass _              = False

summarise :: Int -> Int -> Text
summarise 0 _ =
  "No stored properties. Run ghci_quickcheck and it'll auto-persist on pass."
summarise total 0 =
  T.pack (show total) <> " / " <> T.pack (show total) <> " stored properties pass."
summarise total regressed =
  T.pack (show regressed) <> " of " <> T.pack (show total) <> " stored \
  \properties regressed. Details in 'regressions'."

renderStored :: StoredProperty -> Value
renderStored sp =
  object
    [ "expression" .= spExpression sp
    , "module"     .= spModule sp
    , "passed"     .= spPassed sp
    , "updated"    .= spUpdated sp
    ]

renderRegression :: Replay -> Value
renderRegression r =
  object
    [ "expression" .= spExpression (rpStored r)
    , "module"     .= spModule (rpStored r)
    , "outcome"    .= renderOutcome (rpResult r)
    ]

renderOutcome :: QuickCheckResult -> Value
renderOutcome = \case
  QcPassed _ n          -> object [ "state" .= ("passed" :: Text), "passed" .= n ]
  QcFailed _ n shr cex  -> object
                             [ "state"          .= ("failed" :: Text)
                             , "passed"         .= n
                             , "shrinks"        .= shr
                             , "counterexample" .= cex
                             ]
  QcException _ err     -> object [ "state" .= ("exception" :: Text), "error" .= err ]
  QcGaveUp _ n disc     -> object
                             [ "state"     .= ("gave_up" :: Text)
                             , "passed"    .= n
                             , "discarded" .= disc
                             ]
  QcUnparsed _ raw      -> object [ "state" .= ("unparsed" :: Text), "raw" .= raw ]

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

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
