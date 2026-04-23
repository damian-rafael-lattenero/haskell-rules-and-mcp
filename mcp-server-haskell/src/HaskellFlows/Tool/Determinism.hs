-- | @ghci_determinism@ — Wave-3 full in-process.
--
-- Re-run a QuickCheck property N times (default 3) with independent
-- 'stdArgs' seeds and report whether every run passes. Uses the same
-- 'evalIOString' primitive as 'ghci_quickcheck' — compile the property
-- once per run, coerce the HValue to @IO String@, execute it, parse.
module HaskellFlows.Tool.Determinism
  ( descriptor
  , handle
  , DeterminismArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Timeout (timeout)

import GHC
  ( InteractiveImport (IIDecl)
  , getContext
  , mkModuleName
  , setContext
  , simpleImportDecl
  )

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , evalIOString
  , firstTestSuiteOrLibrary
  , loadForTarget
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error (GhcError)
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

-- | 30 s per run, mirroring ghci_quickcheck's budget.
runTimeoutMicros :: Int
runTimeoutMicros = 30_000_000

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> case sanitizeExpression (daProperty args) of
    Left _ -> pure (errorResult "property is empty or contains forbidden characters")
    Right safe -> do
      tgt <- firstTestSuiteOrLibrary ghcSess
      eLoad <- try (loadForTarget ghcSess tgt Strict)
      case eLoad :: Either SomeException (Bool, [GhcError]) of
        Left ex ->
          pure (errorResult ("loadForTarget failed: " <> T.pack (show ex)))
        Right _ -> do
          results <- replicateM (daRuns args) (runOnce ghcSess (daProperty args) safe)
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
    runOnce sess origExpr safe = do
      let stmt = "fmap Test.QuickCheck.output "
            <> "(Test.QuickCheck.quickCheckWithResult "
            <> "(Test.QuickCheck.stdArgs { Test.QuickCheck.chatty = False }) "
            <> "(" <> T.unpack safe <> "))"
      mRes <- timeout runTimeoutMicros $
        try $ withGhcSession sess $ do
          -- Ensure Test.QuickCheck is in the interactive import
          -- context; loadForTarget doesn't import external
          -- packages, only local module-graph entries.
          ctx <- getContext
          setContext (ctx
            <> [IIDecl (simpleImportDecl (mkModuleName "Test.QuickCheck"))])
          evalIOString stmt
      case mRes of
        Nothing -> pure (QcException origExpr "timeout")
        Just (Left (ex :: SomeException)) ->
          pure (QcException origExpr (T.pack (show ex)))
        Just (Right out) ->
          pure (parseQuickCheckOutput origExpr (T.pack out))

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
