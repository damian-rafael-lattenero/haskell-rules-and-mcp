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
  , coverageTimeoutMicros
    -- * Outcome type (#163: exported for tests)
  , CovOutcome (..)
  , renderResult
    -- * Pure helpers (re-exported for unit tests; see Spec.hs)
  , summarise
  , renderMetric
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hGetContents')
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )

import HaskellFlows.Config (Micros (..))
import HaskellFlows.Util.Process (SubprocessOutcome (..), SubprocessResult (..), runArgv)

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  )
import HaskellFlows.Types (ProjectDir, unProjectDir)
import HaskellFlows.Tool.Env (ToolEnv (..))

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcCoverage
    , tdDescription =
        "PURPOSE: Run 'cabal test --enable-coverage' and parse the HPC "
          <> "report into 8 structured metrics. "
          <> "WHEN: after tests pass, to find untested code paths before "
          <> "pushing. "
          <> "WHEN NOT: ghc_gate for the pre-push composite; ghc_quickcheck "
          <> "for property testing itself. "
          <> "PREREQUISITES: cabal on PATH and a test-suite; runs are slow "
          <> "(hard 5-minute timeout). "
          <> "OUTPUT: {metrics:{expressions, alternatives, local_bindings, "
          <> "top_level, literals, module, ...}}. "
          <> "SEE ALSO: ghc_gate, ghc_quickcheck."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "timeout_minutes" .= object
                  [ "type"        .= ("integer" :: Text)
                  , "description" .=
                      ("Maximum minutes to wait for 'cabal test \
                       \--enable-coverage'. Default 5. Clamp [1, 60]. \
                       \Raise for large projects where HPC instrumentation \
                       \adds significant compile time." :: Text)
                  ]
              , "verbose" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("When true, include the full cabal stdout in \
                       \result.raw. Default false — omit raw to keep \
                       \responses small. The structured metrics array \
                       \already contains all parsed information." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

-- | #163: timeout is now configurable via the @timeout_minutes@
-- argument. Default 5 minutes; clamped to [1, 60] minutes.
--
-- #178: @verbose@ (default @false@) controls whether the full cabal
-- stdout is included in @result.raw@. When false the raw field is
-- omitted, keeping responses small.
data CoverageArgs = CoverageArgs
  { caTimeoutMinutes :: !Int
  , caVerbose        :: !Bool
  }
  deriving stock (Show)

instance FromJSON CoverageArgs where
  parseJSON = withObject "CoverageArgs" $ \o -> do
    t <- o .:? "timeout_minutes" .!= 5
    v <- o .:? "verbose"         .!= False
    pure CoverageArgs { caTimeoutMinutes = max 1 (min 60 t), caVerbose = v }

-- | Compute the timeout in microseconds from the args.
coverageTimeoutMicros :: CoverageArgs -> Int
coverageTimeoutMicros args = caTimeoutMinutes args * 60 * 1_000_000

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = do
  pd <- teProjectDir env
  runHandle pd rawArgs

runHandle :: ProjectDir -> Value -> IO ToolResponse
runHandle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (formatParseError parseError)
  Right args -> do
    mCabal <- findExecutable "cabal"
    case mCabal of
      Nothing   -> pure (unavailableResult "cabal binary not found on PATH")
      Just _    -> do
        outcome <- runCoverage (coverageTimeoutMicros args) pd
        pure (renderResult args outcome)


--------------------------------------------------------------------------------
-- subprocess
--------------------------------------------------------------------------------

data CovOutcome
  = CovSuccess !Text   -- raw cabal/hpc output
  | CovTimeout
  | CovFailure !Int !Text
  deriving stock (Eq, Show)

runCoverage :: Int -> ProjectDir -> IO CovOutcome
runCoverage timeoutMicros pd = do
  outcome <- runArgv (Micros timeoutMicros) (Just (unProjectDir pd))
               "cabal" ["test", "--enable-coverage"]
  case outcome of
    TimedOut    -> pure CovTimeout
    Completed r -> case srExit r of
      ExitSuccess -> do
        -- Modern cabal + HPC only write HTML to disk; the stdout that
        -- used to carry the "NN% expressions used (X/Y)" summary lines
        -- is now just a list of "Writing: …html" paths. Post-process by
        -- locating the .tix and mix dir produced by the coverage run
        -- and asking `hpc report` for the text summary. If anything in
        -- the post-processing chain fails we fall back to the raw
        -- cabal output — the parser will just emit "no metrics" as
        -- before, with no regression risk.
        enriched <- enrichWithHpcReport pd (srStdout r)
        pure (CovSuccess enriched)
      ExitFailure code -> pure (CovFailure code (srStderr r))

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
  -- Strict read BEFORE waitForProcess: lazy hGetContents would defer
  -- the read until 'lines out' forces it (after the process is reaped),
  -- which both races the handle close and can deadlock if find's output
  -- exceeds the pipe buffer. Same fix as Util.Process.runArgv.
  out <- hGetContents' hOut
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
  -- Strict read before reap — see findTixFile for the rationale.
  out <- hGetContents' hOut
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
      -- Drain stderr strictly on a thread (void (hGetContents hErr)
      -- never forced the read, so a full stderr pipe could deadlock the
      -- child), and read stdout strictly before reaping the process.
      _   <- forkIO (void (hGetContents' hErr))
      out <- hGetContents' hOut
      ec  <- waitForProcess ph
      case ec of
        ExitSuccess -> pure (Just (T.pack out))
        _           -> pure Nothing

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C + #163 + #176 + #177 + #178:
--
-- * 'CovSuccess' → status='ok' with parsed metrics under 'result'.
--   @raw@ is only included when @verbose=true@ (#178).
-- * 'CovTimeout' → status='timeout' kind='inner_timeout', cause=NmT.
-- * 'CovFailure' → status='failed' kind='subprocess_error',
--                  cause=<exit code>.
renderResult :: CoverageArgs -> CovOutcome -> ToolResponse
renderResult args (CovSuccess out) =
  let report  = parseCoverage out
      metrics = crMetrics report
      base    = [ "metrics" .= map renderMetric metrics
                , "summary" .= summarise metrics
                ]
      payload = object (base <> [ "raw" .= out | caVerbose args ])
  in Env.mkOk payload
renderResult args CovTimeout =
  let mins   = T.pack (show (caTimeoutMinutes args)) <> "m"
      envErr = (Env.mkErrorEnvelope Env.InnerTimeout
                  ("cabal test --enable-coverage timed out after "
                   <> mins))
                 { Env.eeCause      = Just mins
                 , Env.eeRemediation =
                     Just ("Raise the timeout via timeout_minutes=N \
                           \(current: " <> T.pack (show (caTimeoutMinutes args))
                           <> " min). Large projects with HPC \
                           \instrumentation typically need 10-20 min.")
                 }
  in Env.mkTimeout envErr
renderResult _ (CovFailure code err) =
  let msg    = "cabal test --enable-coverage exited with code "
                 <> T.pack (show code) <> ": " <> T.strip err
      envErr = (Env.mkErrorEnvelope Env.SubprocessError msg)
                 { Env.eeCause = Just (T.pack (show code)) }
  in Env.mkFailed envErr

-- | Issue #89 + #177: 'percent' is null when the metric has no applicable
-- program points (@total == 0@). 'status' is the categorical
-- discriminator @\"covered\" | \"uncovered\" | \"not_applicable\"@
-- — agents should branch on it instead of treating @percent: 100,
-- total: 0@ as a positive contribution.
--
-- #177: @alwaysTrue@ and @alwaysFalse@ are only emitted when non-zero
-- to keep the common case (no annotation) clean.
renderMetric :: Metric -> Value
renderMetric m =
  let base =
        [ "label"   .= mLabel m
        , "percent" .= mPercent m
        , "covered" .= mCovered m
        , "total"   .= mTotal m
        , "status"  .= mStatus m
        ]
      annots =
        [ "alwaysTrue"  .= mAlwaysTrue  m | mAlwaysTrue  m > 0 ] <>
        [ "alwaysFalse" .= mAlwaysFalse m | mAlwaysFalse m > 0 ]
  in object (base <> annots)

-- | Issue #89 + #176: average across only the *applicable* metrics
-- (@total > 0@), excluding @\"boolean coverage\"@ which is the parent
-- bucket of guards, @\'if\'@ conditions, and qualifiers. Including it
-- would double-count any @\'if\'@ condition failures in the average.
--
-- The summary string names the count of applicable metrics so an
-- agent can tell at a glance how many categories actually applied
-- (vs. the old fixed-8 wording that made the absent ones invisible).
summarise :: [Metric] -> Text
summarise [] = "No coverage metrics parsed from the cabal output."
summarise ms =
  -- #176: exclude "boolean coverage" (parent bucket) from the average.
  let filtered   = filter ((/= "boolean coverage") . mLabel) ms
      applicable = [p | Metric { mPercent = Just p } <- filtered]
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
unavailableResult :: Text -> ToolResponse
unavailableResult msg =
  let payload  = object
        [ "remediation" .= ( "Install cabal (`ghcup install cabal`) and \
                            \retry." :: Text )
        ]
      envErr   = Env.mkErrorEnvelope Env.BinaryUnavailable msg
      response = (Env.mkUnavailable envErr) { Env.reResult = Just payload }
  in response
