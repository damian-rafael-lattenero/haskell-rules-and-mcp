-- | Internal handler for @ghc_quickcheck@ when called with @runs >= 2@.
--
-- Re-runs a QuickCheck property N times (default 3) with independent
-- 'stdArgs' seeds and reports whether every run passes. Uses the same
-- 'evalIOString' primitive as 'ghc_quickcheck' — compile the property
-- once per run, coerce the HValue to @IO String@, execute it, parse.
--
-- Issue #94 Phase C retired the @ghc_determinism@ wire surface;
-- @ghc_quickcheck@ now accepts an optional @runs: Int@ field, and
-- the dispatcher routes to this handler when @runs >= 2@.
module HaskellFlows.Tool.Determinism
  ( handle
  , DeterminismArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import HaskellFlows.Ghc.ApiSession (GhcSession, gsProject)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..), parseQuickCheckOutput)
import qualified HaskellFlows.Tool.QuickCheck as QcTool


data DeterminismArgs = DeterminismArgs
  { daProperty :: !Text
  , daRuns     :: !Int
  , daModule   :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON DeterminismArgs where
  parseJSON = withObject "DeterminismArgs" $ \o ->
    DeterminismArgs
      <$> o .:  "property"
      <*> o .:? "runs" .!= 3
      <*> o .:? "module"

-- | 30 s per run, mirroring ghc_quickcheck's budget.
runTimeoutMicros :: Int
runTimeoutMicros = 30_000_000

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> case sanitizeExpression (daProperty args) of
    Left e ->
      pure (Env.toolResponseToResult
              (Env.mkRefused (Env.sanitizeRejection "property" e)))
    Right safe -> do
      results <- replicateM (daRuns args)
                   (runOnce ghcSess (daProperty args) safe (daModule args))
      let allPassed = all isPassed results
          summaryTxt
            | allPassed =
                "All " <> T.pack (show (daRuns args))
                  <> " runs passed — no flakiness observed."
            | otherwise =
                "At least one run did not pass — property is flaky \
                \or broken."
          payload = object
            [ "runs"    .= daRuns args
            , "states"  .= map stateText results
            , "summary" .= summaryTxt
            ]
      -- Issue #90 Phase C: every run passed → status='ok'. Any
      -- non-pass → status='failed' kind='validation' (the property
      -- is flaky). Per-run states stay under 'result.states' so
      -- consumers can pinpoint which run flipped.
      pure $ if allPassed
        then Env.toolResponseToResult (Env.mkOk payload)
        else
          let envErr   = Env.mkErrorEnvelope Env.Validation summaryTxt
              response = (Env.mkFailed envErr) { Env.reResult = Just payload }
          in Env.toolResponseToResult response
  where
    runOnce sess origExpr safe mModule = do
      -- Route through the same subprocess-cabal-repl vehicle as
      -- ghc_quickcheck. The in-process evalIOString path was
      -- tripping on the GHC-API package-resolution bug even when
      -- the stanza flags had -package-id QuickCheck — cabal repl
      -- sidesteps that entirely. 'mModule' mirrors the
      -- 'ghc_quickcheck' 'module' parameter — without it a
      -- property that references project-local types fails to
      -- compile since the test-suite stanza's auto-load set
      -- doesn't cover ad-hoc 'test/Gen.hs'-style helpers.
      mRes <- timeout runTimeoutMicros $
        try $ QcTool.runQuickCheckViaCabalRepl (gsProject sess) mModule safe
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

