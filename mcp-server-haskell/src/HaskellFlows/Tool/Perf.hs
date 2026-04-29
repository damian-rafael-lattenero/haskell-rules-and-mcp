-- | @ghc_perf@ — performance microscope (#61).
--
-- The issue describes a four-phase tool (criterion-in-process,
-- Core dump parsing, baseline persistence, AI narration) totalling
-- ~2 weeks. This commit lands Phase 1: a minimal wall-clock
-- harness that compiles + runs a Haskell expression N times
-- in-process and returns aggregate statistics (mean / median /
-- min / max in nanoseconds).
--
-- Phase 1 deferrals (documented in the descriptor and response):
--
--   * Criterion-style autotuning + warmup loops.
--   * Core dump parsing + hotspot detection.
--   * Allocation tracking (would need @+RTS -T@ instrumentation).
--   * Baseline persistence in @.haskell-flows/perf.json@.
--   * AI narration / agent-driven optimisation candidates.
--
-- The structural shape (tool dispatch, evidence package, two-call
-- narration protocol) is in place so Phase 2 can grow without
-- touching the response schema.
module HaskellFlows.Tool.Perf
  ( descriptor
  , handle
  , PerfArgs (..)
    -- * Pure statistics helpers (exported for unit tests)
  , aggregate
  , Stats (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Data.Word (Word64)

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , evalIOString
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcPerf
    , tdDescription =
        "Phase 1 wall-clock perf harness. Evaluates a Haskell "
          <> "expression N times in-process and returns aggregate "
          <> "wall-clock statistics (mean/median/min/max ns). "
          <> "Phase 2 (planned) will add criterion warmup, Core "
          <> "dump parsing + hotspot heuristics, allocation "
          <> "tracking, baseline persistence, and the agent-driven "
          <> "narration protocol described in the issue."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= obj "string"
              , "runs"       .= obj "integer"
              ]
          , "required"             .= (["expression"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data PerfArgs = PerfArgs
  { paExpression :: !Text
  , paRuns       :: !Int
  }
  deriving stock (Show)

instance FromJSON PerfArgs where
  parseJSON = withObject "PerfArgs" $ \o -> do
    e <- o .:  "expression"
    r <- o .:? "runs" .!= 30
    pure PerfArgs { paExpression = e, paRuns = clampRuns r }
    where
      -- Bound the runs so a typo of "1000000" doesn't tie up the
      -- session for hours. Floor at 1 (one sample is technically
      -- valid; the agent decides whether it's enough signal).
      clampRuns n = max 1 (min 1000 n)

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (parseErrorResult err)
  Right args -> case sanitizeExpression (paExpression args) of
    Left e ->
      pure (Env.toolResponseToResult
              (Env.mkRefused (Env.sanitizeRejection "expression" e)))
    Right safe -> runPerf ghcSess args safe

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
-- timing harness
--------------------------------------------------------------------------------

runPerf :: GhcSession -> PerfArgs -> Text -> IO ToolResult
runPerf ghcSess args safe = do
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
  pure (renderResult args nss stats errs)
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
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: status='ok' carries the measurement
-- table. Per-run errors stay under 'result.errors' so the agent
-- can drill in; 'measurements' has the aggregate stats. Phase 1
-- is informational; Phase 2 will fail with kind='validation' if
-- the run regressed against a stored baseline.
renderResult :: PerfArgs -> [Word64] -> Stats -> [Text] -> ToolResult
renderResult args nss stats errs =
  let payload = object
        [ "expression"    .= paExpression args
        , "runs_request"  .= paRuns args
        , "runs_executed" .= sCount stats
        , "errors"        .= errs
        , "measurements"  .= object
            [ "mean_ns"   .= sMean stats
            , "median_ns" .= sMedian stats
            , "min_ns"    .= sMin stats
            , "max_ns"    .= sMax stats
            , "samples"   .= nss
            ]
        , "phase"         .= ("1-mvp" :: Text)
        , "deferred"      .= ([ "criterion-warmup"
                              , "core-dump-hotspots"
                              , "allocation-tracking"
                              , "baseline-persistence"
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
            ( "Phase 1 only measures wall-clock time. To investigate \
              \regressions, hand 'narration_context' to your LLM for \
              \plain-English analysis. Phase 2 will provide a verify \
              \endpoint that checks proposed micro-optimisations \
              \against the regression store before committing." :: Text )
        ]
  in Env.toolResponseToResult (Env.mkOk payload)
