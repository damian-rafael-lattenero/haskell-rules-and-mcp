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

import HaskellFlows.Ghc.ApiSession (GhcSession, gsProject)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..), parseQuickCheckOutput)
import qualified HaskellFlows.Tool.QuickCheck as QcTool

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
      -- Route through the same subprocess-cabal-repl vehicle as
      -- ghci_quickcheck. The in-process evalIOString path was
      -- tripping on the GHC-API package-resolution bug even when
      -- the stanza flags had -package-id QuickCheck — cabal repl
      -- sidesteps that entirely.
      mRes <- timeout runTimeoutMicros $
        try $ QcTool.runQuickCheckViaCabalRepl (gsProject sess) Nothing safe
      case mRes of
        Nothing -> pure (QcException origExpr "timeout")
        Just (Left (ex :: SomeException)) ->
          pure (QcException origExpr (T.pack (show ex)))
        Just (Right (out, _err)) ->
          -- Determinism runs the same property N times; the
          -- stderr from each run is collapsed into QcUnparsed if
          -- any invocation fails to compile. Stdout carries the
          -- QC verdict; stderr is informational for this tool.
          pure (parseQuickCheckOutput origExpr out)

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
