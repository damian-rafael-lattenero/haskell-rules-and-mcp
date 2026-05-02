-- | @ghc_perf@ — performance microscope (#61).
--
-- Phase 2 (this commit): adds baseline persistence to
-- @.haskell-flows\/perf.json@. The file maps expression strings to
-- their last-recorded mean_ns. Use @save_baseline=true@ to record a
-- measurement and @compare_baseline=true@ to check for regressions
-- (threshold: >10 % slower than stored mean).
--
-- Remaining deferrals (still Phase 3+):
--   * Criterion-style autotuning + warmup loops.
--   * Core dump parsing + hotspot detection.
--   * Allocation tracking (@+RTS -T@ instrumentation).
--   * AI narration / agent-driven optimisation candidates.
module HaskellFlows.Tool.Perf
  ( descriptor
  , handle
  , PerfArgs (..)
    -- * Pure statistics helpers (exported for unit tests)
  , aggregate
  , Stats (..)
    -- * Baseline helpers (exported for unit tests)
  , BaselineEntry (..)
  , regressionPct
    -- * Response shaping (exported for unit tests)
  , renderResult
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import GHC.Clock (getMonotonicTimeNSec)
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , evalIOString
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcPerf
    , tdDescription =
        "Wall-clock perf harness. Evaluates a Haskell expression N "
          <> "times in-process and returns aggregate statistics "
          <> "(mean/median/min/max ns). Phase 2: set save_baseline=true "
          <> "to persist the mean to .haskell-flows/perf.json, or "
          <> "compare_baseline=true to detect regressions (>10% slower). "
          <> "Criterion warmup, Core dump, and allocation tracking remain "
          <> "deferred."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression"       .= obj "string"
              , "runs"             .= obj "integer"
              , "save_baseline"    .= obj "boolean"
              , "compare_baseline" .= obj "boolean"
              , "verbose"          .= obj "boolean"
              ]
          , "required"             .= (["expression"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data PerfArgs = PerfArgs
  { paExpression      :: !Text
  , paRuns            :: !Int
  , paSaveBaseline    :: !Bool
    -- ^ Phase 2: when True, persist the measured mean_ns to the
    -- project's @.haskell-flows\/perf.json@ baseline store.
  , paCompareBaseline :: !Bool
    -- ^ Phase 2: when True, compare the current mean_ns against the
    -- stored baseline and surface a regression warning if the
    -- current measurement is >10 % slower.
  , paVerbose         :: !Bool
    -- ^ F-26: when False (default), omit the per-sample 'samples'
    -- array from the response. Avoids sending thousands of integers
    -- for large 'runs' values.
  }
  deriving stock (Show)

instance FromJSON PerfArgs where
  parseJSON = withObject "PerfArgs" $ \o -> do
    e  <- o .:  "expression"
    r  <- o .:? "runs"             .!= 30
    sb <- o .:? "save_baseline"    .!= False
    cb <- o .:? "compare_baseline" .!= False
    v  <- o .:? "verbose"          .!= False
    pure PerfArgs
      { paExpression      = e
      , paRuns            = clampRuns r
      , paSaveBaseline    = sb
      , paCompareBaseline = cb
      , paVerbose         = v
      }
    where
      -- Bound the runs so a typo of "1000000" doesn't tie up the
      -- session for hours. Floor at 1 (one sample is technically
      -- valid; the agent decides whether it's enough signal).
      clampRuns n = max 1 (min 1000 n)

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> case sanitizeExpression (paExpression args) of
    Left e ->
      pure (Env.toolResponseToResult
              (Env.mkRefused (Env.sanitizeRejection "expression" e)))
    Right safe -> runPerf ghcSess pd args safe


--------------------------------------------------------------------------------
-- timing harness
--------------------------------------------------------------------------------

runPerf :: GhcSession -> ProjectDir -> PerfArgs -> Text -> IO ToolResult
runPerf ghcSess pd args safe = do
  -- 'evalIOString' unsafeCoerce's the compiled expression to
  -- 'IO String'. Wrap the user expression so it becomes a pure
  -- 'IO' action that returns the @show@-rendered value — that
  -- forces full evaluation under the timing window AND satisfies
  -- the 'IO String' contract (no runtime stg_ap_v_ret crash).
  let wrappedExpr = "pure (show (" <> safe <> ")) :: IO String"
  samples <- mapM (timeOnce ghcSess wrappedExpr) [1 .. paRuns args]
  let nss   = map fst samples
      errs  = [e | (_, Left e) <- map fst' samples]
      stats = aggregate nss
  -- Phase 2: baseline persistence and regression detection.
  let baselinePath = perfBaselinePath pd
  mBaseline <- if paCompareBaseline args
                 then readBaseline baselinePath (paExpression args)
                 else pure Nothing
  when (paSaveBaseline args) $
    saveBaseline baselinePath (paExpression args) stats
  pure (renderResult args nss stats errs mBaseline)
  where
    fst' (n, Right _) = (n, Right ())
    fst' (n, Left e)  = (n, Left e)

-- | One timed evaluation. Returns @(elapsedNanoseconds, result)@
-- where 'result' is either the captured String or a synthetic
-- error message (timeouts, GHC exceptions, ...).
timeOnce
  :: GhcSession -> Text -> a
  -> IO (Word64, Either Text Text)
timeOnce ghcSess expr _ = do
  t0 <- getMonotonicTimeNSec
  res <- try @SomeException $ withGhcSession ghcSess
           (evalIOString (T.unpack expr))
  t1 <- getMonotonicTimeNSec
  let elapsed = if t1 > t0 then t1 - t0 else 0
  pure (elapsed, case res of
                   Left e  -> Left (T.pack (show e))
                   Right s -> Right (T.pack s))

--------------------------------------------------------------------------------
-- statistics
--------------------------------------------------------------------------------

data Stats = Stats
  { sMean   :: !Double
  , sMedian :: !Double
  , sMin    :: !Word64
  , sMax    :: !Word64
  , sCount  :: !Int
  }
  deriving stock (Eq, Show)

aggregate :: [Word64] -> Stats
aggregate [] = Stats 0 0 0 0 0
aggregate ns =
  let cnt    = length ns
      total  = sum (map fromIntegral ns) :: Double
      mean   = total / fromIntegral cnt
      sorted = sort ns
      med    = case (cnt `mod` 2, cnt `div` 2) of
        (1, m) -> fromIntegral (sorted !! m)
        (0, m) -> fromIntegral (sorted !! (m - 1) + sorted !! m) / 2
        _      -> mean
  in Stats
       { sMean   = mean
       , sMedian = med
       , sMin    = minimum ns
       , sMax    = maximum ns
       , sCount  = cnt
       }

--------------------------------------------------------------------------------
-- Phase 2 — baseline persistence
--------------------------------------------------------------------------------

-- | Path to the JSON baseline store inside the project.
perfBaselinePath :: ProjectDir -> FilePath
perfBaselinePath pd = unProjectDir pd </> ".haskell-flows" </> "perf.json"

-- | Stored baseline entry (single expression record).
newtype BaselineEntry = BaselineEntry
  { beMeanNs :: Double
  }
  deriving stock (Show)

instance FromJSON BaselineEntry where
  parseJSON = withObject "BaselineEntry" $ \o ->
    BaselineEntry <$> o .: "mean_ns"

instance ToJSON BaselineEntry where
  toJSON e = object [ "mean_ns" .= beMeanNs e ]

-- | Read the stored baseline for @expr@ from the perf store.
-- Returns 'Nothing' when the file doesn't exist or the expression
-- has no recorded baseline.
readBaseline :: FilePath -> Text -> IO (Maybe BaselineEntry)
readBaseline path expr = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- try @SomeException (BL.readFile path)
      case raw of
        Left  _    -> pure Nothing
        Right bytes ->
          case decode bytes of
            Just (Object km) ->
              case KeyMap.lookup (keyFromText expr) km of
                Just v  -> pure $ case fromJSON v of
                  Success e -> Just e
                  _         -> Nothing
                Nothing -> pure Nothing
            _ -> pure Nothing

-- | Write/update the baseline for @expr@ in the perf store.
-- Creates @.haskell-flows\/@ if it doesn't exist.
saveBaseline :: FilePath -> Text -> Stats -> IO ()
saveBaseline path expr stats = do
  let dir = reverse (dropWhile (/= '/') (reverse path))
  createDirectoryIfMissing True dir
  current <- do
    exists <- doesFileExist path
    if not exists then pure (Object KeyMap.empty)
    else do
      raw <- try @SomeException (BL.readFile path)
      case raw of
        Left  _     -> pure (Object KeyMap.empty)
        Right bytes -> pure (fromMaybe (Object KeyMap.empty) (decode bytes))
  let entry = toJSON (BaselineEntry { beMeanNs = sMean stats })
      updated = case current of
        Object km -> Object (KeyMap.insert (keyFromText expr) entry km)
        _         -> Object (KeyMap.fromList [(keyFromText expr, entry)])
  BL.writeFile path (encode updated)

-- | Compute the regression percentage: positive means slower,
-- negative means faster. @Nothing@ when baseline mean is zero.
regressionPct :: Double -> Double -> Maybe Double
regressionPct baselineMean currentMean
  | baselineMean <= 0 = Nothing
  | otherwise         = Just ((currentMean - baselineMean) / baselineMean * 100)

-- | Internal helper: convert a Text key to an Aeson KeyMap key.
keyFromText :: Text -> AesonKey.Key
keyFromText = AesonKey.fromText

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C + Phase 2: status='ok' carries the measurement
-- table. Per-run errors stay under 'result.errors' so the agent
-- can drill in; 'measurements' has the aggregate stats.
-- Phase 2: when 'mBaseline' is provided and the current mean is
-- >10 % slower, the response carries a 'regression' field with
-- 'kind="validation"' signalling so the agent can act on it.
renderResult :: PerfArgs -> [Word64] -> Stats -> [Text] -> Maybe BaselineEntry -> ToolResult
renderResult args nss stats errs mBaseline =
  -- F-31: when every sample errored the session has likely lost the
  -- module. Surface this directly rather than computing a meaningless
  -- regression percentage against a baseline.
  let allErrored = not (null errs) && length errs == paRuns args
  in if allErrored
       then Env.toolResponseToResult
              (Env.mkFailed
                ((Env.mkErrorEnvelope Env.SubprocessError
                    ("All " <> T.pack (show (paRuns args))
                     <> " measurements errored. The GHC session may have lost "
                     <> "the module — run ghc_load to reload before benchmarking."))
                      { Env.eeCause     = Just (T.unlines (take 3 errs))
                      , Env.eeRemediation = Just "Call ghc_load(module_path=…) to reload the module, then retry ghc_perf."
                      }))
       else
  let mRegression = do
        be  <- mBaseline
        pct <- regressionPct (beMeanNs be) (sMean stats)
        pure (pct, beMeanNs be)
      isRegression = maybe False (\(pct, _) -> pct > 10.0) mRegression
      baselineFields = case mRegression of
        Nothing -> []
        Just (pct, baseMean) ->
          [ "baseline" .= object
              [ "baseline_mean_ns" .= baseMean
              , "current_mean_ns"  .= sMean stats
              , "regression_pct"   .= pct
              , "regressed"        .= isRegression
              ]
          ]
      -- F-26: gate per-sample array behind verbose=true to avoid
      -- sending thousands of integers for large 'runs' values.
      samplesField = [ "samples" .= nss | paVerbose args ]
      payload = object $
        [ "expression"    .= paExpression args
        , "runs_request"  .= paRuns args
        , "runs_executed" .= sCount stats
        , "errors"        .= errs
        , "measurements"  .= object
            ( [ "mean_ns"   .= sMean stats
              , "median_ns" .= sMedian stats
              , "min_ns"    .= sMin stats
              , "max_ns"    .= sMax stats
              ] <> samplesField )
        , "phase"         .= ("2-baseline" :: Text)
        , "deferred"      .= ([ "criterion-warmup"
                              , "core-dump-hotspots"
                              , "allocation-tracking"
                              , "narration-endpoint"
                              ] :: [Text])
        , "narration_context" .= object
            [ "expression"   .= paExpression args
            , "measurements" .= object
                [ "mean_ns"   .= sMean stats
                , "median_ns" .= sMedian stats
                ]
            ]
        , "instructions_for_agent" .=
            ( "Phase 2: set save_baseline=true to persist a mean_ns baseline, \
              \compare_baseline=true to detect regressions (>10% slower). \
              \Pass verbose=true to include per-sample timing array." :: Text )
        ] <> baselineFields
      -- F-32: cause is a human-readable summary, not a stringified JSON blob.
      regressionMsg = case mRegression of
        Just (pct, _) -> "Regression: " <> T.pack (show (round pct :: Int))
                           <> "% slower than stored baseline (threshold 10%)"
        Nothing       -> "Regression detected"
      regressionCause = case mRegression of
        Just (pct, baseMean) ->
          "baseline_mean_ns=" <> T.pack (show (round baseMean :: Int))
          <> ", current_mean_ns=" <> T.pack (show (round (sMean stats) :: Int))
          <> ", regression_pct=" <> T.pack (show (round pct :: Int))
        Nothing -> "baseline exceeded"
  in if isRegression
       then Env.toolResponseToResult
              (Env.mkRefused
                ((Env.mkErrorEnvelope Env.Validation regressionMsg)
                  { Env.eeCause = Just regressionCause }))
       else Env.toolResponseToResult (Env.mkOk payload)
