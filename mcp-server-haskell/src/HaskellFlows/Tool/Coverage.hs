-- | @ghci_coverage@ — shell out to @cabal test --enable-coverage@ and
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
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
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
import HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  )
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_coverage"
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
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right CoverageArgs -> do
    mCabal <- findExecutable "cabal"
    case mCabal of
      Nothing   -> pure (unavailableResult "cabal binary not found on PATH")
      Just _    -> do
        outcome <- runCoverage pd
        pure (renderResult outcome)

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

renderResult :: CovOutcome -> ToolResult
renderResult (CovSuccess out) =
  let report  = parseCoverage out
      metrics = crMetrics report
      payload =
        object
          [ "success" .= True
          , "metrics" .= map renderMetric metrics
          , "summary" .= summarise metrics
          , "raw"     .= out
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }
renderResult CovTimeout =
  errorResult "cabal test --enable-coverage timed out after 5 minutes"
renderResult (CovFailure code err) =
  errorResult ( "cabal test --enable-coverage exited with code "
             <> T.pack (show code) <> ": " <> T.strip err )

renderMetric :: Metric -> Value
renderMetric m =
  object
    [ "label"   .= mLabel m
    , "percent" .= mPercent m
    , "covered" .= mCovered m
    , "total"   .= mTotal m
    ]

summarise :: [Metric] -> Text
summarise [] = "No coverage metrics parsed from the cabal output."
summarise ms =
  let avg = sum (map mPercent ms) `div` length ms
  in "Average coverage across "
     <> T.pack (show (length ms)) <> " metrics: "
     <> T.pack (show avg) <> "%."

unavailableResult :: Text -> ToolResult
unavailableResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success"     .= False
        , "error"       .= msg
        , "remediation" .= ( "Install cabal (`ghcup install cabal`) and \
                            \retry." :: Text )
        ]))
      ]
    , trIsError = True
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

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
