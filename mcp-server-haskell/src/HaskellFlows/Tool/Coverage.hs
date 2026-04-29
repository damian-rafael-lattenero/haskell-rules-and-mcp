-- | @ghc_coverage@ — shell out to @cabal test --enable-coverage@ and
-- surface the HPC summary in a structured form.
--
-- Pattern follows 'HaskellFlows.Tool.Hoogle': external binary spawn
-- with argv-form, hard timeout (coverage builds can be slow but not
-- infinite), availability detection for @cabal@, and structured
-- output parsing.
module HaskellFlows.Tool.Coverage
  ( descriptor
  , handle
  , CoverageArgs (..)
    -- * Pure helpers (re-exported for unit tests; see Spec.hs)
  , summarise
  , renderMetric
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified HaskellFlows.Mcp.Envelope as Env
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hGetContents)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  )
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcCoverage
    , tdDescription =
        "Run 'cabal test --enable-coverage' and parse the HPC report. "
          <> "Requires cabal on PATH. Coverage runs are slow; hard "
          <> "timeout at 5 minutes."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "additionalProperties" .= False
          ]
    }

data CoverageArgs = CoverageArgs
  deriving stock (Show)

instance FromJSON CoverageArgs where
  parseJSON = withObject "CoverageArgs" $ \_ -> pure CoverageArgs

-- | Upper bound for the coverage run. HPC over a medium project is
-- ~30s typical; 5 minutes is generous without leaving a stuck process
-- around forever.
coverageTimeoutMicros :: Int
coverageTimeoutMicros = 5 * 60 * 1_000_000

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (parseErrorResult parseError)
  Right CoverageArgs -> do
    mCabal <- findExecutable "cabal"
    case mCabal of
      Nothing   -> pure (unavailableResult "cabal binary not found on PATH")
      Just _    -> do
        outcome <- runCoverage pd
        pure (renderResult outcome)

-- | Issue #90 Phase C: caller-side parse failure.
parseErrorResult :: String -> ToolResult
parseErrorResult err =
  let kind | "key" `isInfixOfStr` err = Env.MissingArg
           | otherwise                = Env.TypeMismatch
      envErr = (Env.mkErrorEnvelope kind
                  (T.pack ("Invalid arguments: " <> err)))
                    { Env.eeCause = Just (T.pack err) }
  in Env.toolResponseToResult (Env.mkFailed envErr)
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

--------------------------------------------------------------------------------
-- subprocess
--------------------------------------------------------------------------------

data CovOutcome
  = CovSuccess !Text   -- raw cabal/hpc output
  | CovTimeout
  | CovFailure !Int !Text
  deriving stock (Eq, Show)

runCoverage :: ProjectDir -> IO CovOutcome
runCoverage pd = do
  let cp = (proc "cabal" ["test", "--enable-coverage"])
             { cwd     = Just (unProjectDir pd)
             , std_in  = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  outVar <- newEmptyMVar
  errVar <- newEmptyMVar
  _ <- forkIO (hGetContents hOut >>= putMVar outVar)
  _ <- forkIO (hGetContents hErr >>= putMVar errVar)
  exited <- timeout coverageTimeoutMicros (waitForProcess ph)
  case exited of
    Nothing -> do
      terminateProcess ph
      hClose hOut
      hClose hErr
      pure CovTimeout
    Just ExitSuccess -> do
      o <- takeMVar outVar
      -- Modern cabal + HPC only write HTML to disk; the stdout that
      -- used to carry the "NN% expressions used (X/Y)" summary lines
      -- is now just a list of "Writing: …html" paths. Post-process by
      -- locating the .tix and mix dir produced by the coverage run
      -- and asking `hpc report` for the text summary. If anything in
      -- the post-processing chain fails we fall back to the raw
      -- cabal output — the parser will just emit "no metrics" as
      -- before, with no regression risk.
      enriched <- enrichWithHpcReport pd (T.pack o)
      pure (CovSuccess enriched)
    Just (ExitFailure code) -> do
      e <- takeMVar errVar
      pure (CovFailure code (T.pack e))

-- | Look for the .tix file cabal produced, then ask `hpc report` for
-- the text summary. Append the result to the cabal stdout so
-- downstream 'parseCoverage' can pick up the metrics without needing
-- to understand cabal's HTML-only output shape.
--
-- Cabal 3.14 actually writes mix files to **two** separate paths:
--
--   dist-newstyle/build/<arch>/ghc-<ver>/<pkg>-<ver>/build/extra-compilation-artifacts/hpc/vanilla/mix/<pkg>-<ver>-inplace/
--   dist-newstyle/build/<arch>/ghc-<ver>/<pkg>-<ver>/t/<test>/build/<test>/<test>-tmp/extra-compilation-artifacts/hpc/vanilla/mix/
--
-- (library modules vs. test-suite entry point). `hpc report` needs a
-- @--hpcdir@ flag for each one; passing only the first gets
-- \"can not find dogfood-rle-0.1.0.0-inplace/Main in …\". Earlier
-- attempts derived a single mix dir from the tix path by chopping
-- parents — but there is no @<hpc/vanilla>/mix@ in the actual
-- layout, so the chop landed on a nonexistent directory. We now
-- locate every mix dir via a targeted @find -path@ pattern.
enrichWithHpcReport :: ProjectDir -> Text -> IO Text
enrichWithHpcReport pd cabalOut = do
  let distDir = unProjectDir pd </> "dist-newstyle"
  mTix <- findTixFile distDir
  case mTix of
    Nothing  -> pure cabalOut
    Just tix -> do
      mixDirs <- findMixDirs distDir
      if null mixDirs
        then pure cabalOut
        else do
          mReport <- runHpcReport mixDirs tix
          case mReport of
            Nothing  -> pure cabalOut
            Just rpt -> pure (cabalOut <> "\n" <> rpt)

-- | Locate the first @.tix@ file under @root@. Uses @find@ via argv
-- so no shell interpolation path is open; empty output means no file.
findTixFile :: FilePath -> IO (Maybe FilePath)
findTixFile root = do
  let cp = (proc "find" [root, "-name", "*.tix"])
             { std_out = CreatePipe
             , std_err = NoStream
             }
  (_, Just hOut, _, ph) <- createProcess cp
  out <- hGetContents hOut
  _   <- waitForProcess ph
  case filter (not . null) (lines out) of
    (p:_) -> pure (Just p)
    []    -> pure Nothing

-- | Locate every cabal-generated mix directory under @root@. Matches
-- the layout @.../extra-compilation-artifacts/hpc/vanilla/mix@ which
-- cabal uses for both library and test-suite coverage data. Returns
-- an empty list when none are found.
findMixDirs :: FilePath -> IO [FilePath]
findMixDirs root = do
  let cp = (proc "find"
             [ root, "-type", "d"
             , "-path", "*extra-compilation-artifacts/hpc/vanilla/mix"
             ])
             { std_out = CreatePipe
             , std_err = NoStream
             }
  (_, Just hOut, _, ph) <- createProcess cp
  out <- hGetContents hOut
  _   <- waitForProcess ph
  pure (filter (not . null) (lines out))

-- | Invoke @hpc report --hpcdir=<d1> --hpcdir=<d2> … <tix>@ and
-- return its stdout on success. Passing multiple mix dirs is the
-- fix for F-11: library modules and test entry points live in
-- different mix trees and @hpc@ needs all of them to resolve every
-- @*.mix@ reference the tix file carries. Any failure (hpc not on
-- PATH, nonzero exit, missing paths) collapses to 'Nothing' so the
-- caller can fall back to the cabal stdout untouched.
runHpcReport :: [FilePath] -> FilePath -> IO (Maybe Text)
runHpcReport mixDirs tix = do
  mHpc <- findExecutable "hpc"
  case mHpc of
    Nothing -> pure Nothing
    Just _  -> do
      let hpcDirArgs = [ "--hpcdir=" <> d | d <- mixDirs ]
          cp = (proc "hpc" (["report"] <> hpcDirArgs <> [tix]))
                 { std_out = CreatePipe
                 , std_err = CreatePipe
                 }
      (_, Just hOut, Just hErr, ph) <- createProcess cp
      out <- hGetContents hOut
      _   <- forkIO (void (hGetContents hErr))
      ec  <- waitForProcess ph
      case ec of
        ExitSuccess -> pure (Just (T.pack out))
        _           -> pure Nothing

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C:
--
-- * 'CovSuccess' → status='ok' with parsed metrics under 'result'.
-- * 'CovTimeout' → status='timeout' kind='inner_timeout', cause='5m'.
-- * 'CovFailure' → status='failed' kind='subprocess_error',
--                  cause=<exit code>.
renderResult :: CovOutcome -> ToolResult
renderResult (CovSuccess out) =
  let report  = parseCoverage out
      metrics = crMetrics report
      payload =
        object
          [ "metrics" .= map renderMetric metrics
          , "summary" .= summarise metrics
          , "raw"     .= out
          ]
  in Env.toolResponseToResult (Env.mkOk payload)
renderResult CovTimeout =
  let envErr = (Env.mkErrorEnvelope Env.InnerTimeout
                  ("cabal test --enable-coverage timed out after 5 minutes" :: Text))
                 { Env.eeCause = Just "5m" }
  in Env.toolResponseToResult (Env.mkTimeout envErr)
renderResult (CovFailure code err) =
  let msg    = "cabal test --enable-coverage exited with code "
                 <> T.pack (show code) <> ": " <> T.strip err
      envErr = (Env.mkErrorEnvelope Env.SubprocessError msg)
                 { Env.eeCause = Just (T.pack (show code)) }
  in Env.toolResponseToResult (Env.mkFailed envErr)

-- | Issue #89: 'percent' is null when the metric has no applicable
-- program points (@total == 0@). 'status' is the categorical
-- discriminator @\"covered\" | \"uncovered\" | \"not_applicable\"@
-- — agents should branch on it instead of treating @percent: 100,
-- total: 0@ as a positive contribution.
renderMetric :: Metric -> Value
renderMetric m =
  object
    [ "label"   .= mLabel m
    , "percent" .= mPercent m
    , "covered" .= mCovered m
    , "total"   .= mTotal m
    , "status"  .= mStatus m
    ]

-- | Issue #89: average across only the *applicable* metrics
-- (@total > 0@). The @0/0@ rows that HPC reports as @100%@ aren't
-- evidence of coverage — folding them into the average mis-anchors
-- the headline number for any project that doesn't exercise every
-- branch flavour.
--
-- The summary string names the count of applicable metrics so an
-- agent can tell at a glance how many categories actually applied
-- (vs. the old fixed-8 wording that made the absent ones invisible).
summarise :: [Metric] -> Text
summarise [] = "No coverage metrics parsed from the cabal output."
summarise ms =
  let applicable = [p | Metric { mPercent = Just p } <- ms]
      n          = length applicable
  in case n of
       0 -> "No applicable HPC metrics for this project ("
              <> T.pack (show (length ms))
              <> " metrics seen, all with total=0)."
       _ -> let avg = sum applicable `div` n
            in "Average coverage across " <> T.pack (show n)
                 <> " applicable metrics: "
                 <> T.pack (show avg) <> "%."

-- | Issue #90 Phase C: cabal binary not on PATH → status='unavailable'
-- kind='binary_unavailable'.
unavailableResult :: Text -> ToolResult
unavailableResult msg =
  let payload  = object
        [ "remediation" .= ( "Install cabal (`ghcup install cabal`) and \
                            \retry." :: Text )
        ]
      envErr   = Env.mkErrorEnvelope Env.BinaryUnavailable msg
      response = (Env.mkUnavailable envErr) { Env.reResult = Just payload }
  in Env.toolResponseToResult response
