-- | @ghci_regression@ — Wave-3 full in-process.
--
-- Replay every persisted QuickCheck property as a regression suite.
-- For each stored property, pick the target that owns its recorded
-- @module@ (test-suite / library / …), compile-load that target, then
-- run the property via 'evalIOString'.
--
-- Compared to the legacy subprocess path, no more @:load@ / @:show
-- modules@ dance — 'loadForTarget' owns scope selection.
module HaskellFlows.Tool.Regression
  ( descriptor
  , handle
  , RegressionArgs (..)
  , Action (..)
  , Replay (..)
  , runOne
  , parseShowModulesPaths
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Timeout (timeout)

import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  )
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , evalIOString
  , firstTestSuiteOrLibrary
  , loadForTarget
  , targetForPath
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error (GhcError)
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

-- | 30 s per property replay, mirroring the ghci_quickcheck budget.
replayTimeoutMicros :: Int
replayTimeoutMicros = 30_000_000

handle :: Store -> GhcSession -> Value -> IO ToolResult
handle store ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (RegressionArgs a) -> do
    props <- loadAll store
    case a of
      ActList -> pure (listResult props)
      ActRun  -> do
        results <- mapM (runOne ghcSess) props
        pure (runResult results)

--------------------------------------------------------------------------------
-- running
--------------------------------------------------------------------------------

data Replay = Replay
  { rpStored :: !StoredProperty
  , rpResult :: !QuickCheckResult
  }

runOne :: GhcSession -> StoredProperty -> IO Replay
runOne ghcSess sp = do
  let expr = spExpression sp
  -- Pick the target that governs the scope of the property's module.
  tgt <- case spModule sp of
    Just m | not (T.null m) -> targetForPath ghcSess (T.unpack m)
    _                       -> firstTestSuiteOrLibrary ghcSess
  -- Load the target (Strict — warnings don't matter for replay,
  -- errors would poison the property) and if that succeeds, run
  -- the property via evalIOString.
  eLoad <- try (loadForTarget ghcSess tgt Strict)
  qr <- case eLoad :: Either SomeException (Bool, [GhcError]) of
    Left ex ->
      pure (QcException expr ("loadForTarget failed: " <> T.pack (show ex)))
    Right (False, diags) ->
      pure (QcException expr ("target did not compile; "
                              <> T.pack (show (length diags)) <> " diag(s)"))
    Right (True, _) ->
      case sanitizeExpression expr of
        Left _ -> pure (QcUnparsed expr
                        "boundary-sanitiser rejected the stored expression")
        Right safe -> do
          let stmt = "fmap Test.QuickCheck.output "
                <> "(Test.QuickCheck.quickCheckWithResult "
                <> "(Test.QuickCheck.stdArgs { Test.QuickCheck.chatty = False }) "
                <> "(" <> T.unpack safe <> "))"
          mRes <- timeout replayTimeoutMicros $
            try $ withGhcSession ghcSess (evalIOString stmt)
          case mRes of
            Nothing                      -> pure (QcException expr "timeout")
            Just (Left (ex :: SomeException)) ->
              pure (QcException expr (T.pack (show ex)))
            Just (Right out)             ->
              pure (parseQuickCheckOutput expr (T.pack out))
  pure Replay { rpStored = sp, rpResult = qr }

--------------------------------------------------------------------------------
-- :show modules parser (kept for unit-test coverage even though the
-- Wave-3 code path no longer invokes ghci meta-commands)
--------------------------------------------------------------------------------

parseShowModulesPaths :: Text -> [Text]
parseShowModulesPaths raw =
  [ path
  | ln <- T.lines raw
  , Just path <- [pathFromLine ln]
  ]
  where
    pathFromLine ln =
      let (_, afterOpen) = T.breakOn "(" ln
      in if T.null afterOpen
           then Nothing
           else
             let inside          = T.strip (T.drop 1 afterOpen)
                 (rawPath, _)    = T.breakOn "," inside
                 pathStripped    = T.strip rawPath
             in if T.null pathStripped
                  then Nothing
                  else Just pathStripped

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
