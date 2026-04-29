-- | @ghc_witness@ — property witness explorer (#65).
--
-- When QuickCheck reports @100/100 passed@, it doesn't tell you
-- WHAT it tested. Were all 100 inputs empty lists? Was the
-- recursive case exercised at all? @ghc_witness@ inverts the
-- usual signal — instead of \"did it pass?\" it answers \"what
-- was tested, and what wasn't?\".
--
-- Phase 1 (MVP, total ~1 week):
--
--   * @bucketSize@ — pure helper that maps an input size to one
--     of four canonical buckets ("0", "1-5", "6-20", ">20").
--   * @buildInstrumentedProperty@ — pure helper that wraps the
--     user-supplied property body with @Test.QuickCheck.collect
--     (sizeBucket (length (show args))) (... original body ...)@.
--     Works for any @Show@-able input (the common case) and
--     Phase 1 deliberately uses the @show@-length proxy so we
--     don't need to know the input's structural shape.
--   * @parseLabelDistribution@ — pure helper that walks
--     QuickCheck's formatted output (@\"35.5% size:0-1\"@) and
--     produces @[(label, percent)]@.
--   * @ghc_witness@ tool — runs the instrumented property via
--     the existing @runQuickCheckViaCabalRepl@ harness, parses
--     the distribution, and surfaces \"biased\" warnings when
--     any bucket holds < 1 % of the runs.
--
-- Phase 1 deferrals (documented in the descriptor):
--
--   * Constructor-level distribution — needs to know the type
--     declaration of the input via 'tyConDataCons' (same path
--     'ghc_arbitrary' uses).
--   * Uncovered-branch detection — same dependency.
--   * Smallest-witness extraction — QuickCheck shrinks naturally
--     on failure; for passing properties we'd need an inverted
--     probe and re-execution.
--   * Custom 'classify_by' fields beyond size — Phase 2.
module HaskellFlows.Tool.Witness
  ( descriptor
  , handle
  , WitnessArgs (..)
    -- * Pure helpers (exported for unit tests)
  , bucketSize
  , buildInstrumentedProperty
  , parseLabelDistribution
  , parseLabelCounts
  , countsToDistribution
  , biasWarnings
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isDigit)
import Data.List (sortOn)
import qualified Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Read (readMaybe)

import HaskellFlows.Ghc.ApiSession (GhcSession, gsProject)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import qualified HaskellFlows.Tool.QuickCheck as Qc

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcWitness
    , tdDescription =
        "Phase 1 property-witness explorer. Runs the property "
          <> "through QuickCheck with size-bucket instrumentation, "
          <> "then surfaces the input distribution and flags biased "
          <> "buckets (< 1 %% of total runs). Useful when a "
          <> "'+++ OK, passed N tests' looks suspicious — e.g. all "
          <> "passes were on size-1 inputs. Phase 2 will add "
          <> "constructor-level distribution and uncovered-branch "
          <> "detection via the GHC API."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "required"   .= (["property"] :: [Text])
          , "properties" .= object
              [ "property"    .= obj "string"
              , "module_path" .= obj "string"
              , "runs"        .= obj "integer"
              ]
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data WitnessArgs = WitnessArgs
  { waProperty   :: !Text
  , waModulePath :: !(Maybe Text)
  , waRuns       :: !Int
  }
  deriving stock (Show)

instance FromJSON WitnessArgs where
  parseJSON = withObject "WitnessArgs" $ \o -> do
    prop <- o .:  "property"
    mp   <- o .:? "module_path"
    rs   <- o .:? "runs" .!= 1000
    pure WitnessArgs
      { waProperty   = prop
      , waModulePath = mp
      , waRuns       = max 100 (min 10000 rs)
      }

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (parseErrorResult err)
  Right args -> do
    t0 <- realToFrac <$> getPOSIXTime :: IO Double
    let instrumented = buildInstrumentedProperty (waProperty args) (waRuns args)
    -- Issue #78: use the labels-aware harness so the structured
    -- 'Result.labels' map reaches us regardless of QuickCheck's
    -- 'chatty' setting. The pre-#78 path read 'output r', which
    -- 'chatty=False' (set by the harness) suppresses — leaving the
    -- distribution silently empty.
    res <- try @SomeException $
      Qc.runQuickCheckWithLabelsViaCabalRepl (gsProject ghcSess)
        (waModulePath args) instrumented
    t1 <- realToFrac <$> getPOSIXTime :: IO Double
    case res of
      Left e -> pure (subprocessResult
                        (T.pack ("subprocess error: " <> show e)))
      Right (out, labelsBlock, _err) ->
        let qcResult = parseQuickCheckOutput (waProperty args) out
            -- Issue #78: prefer the structured labels block over
            -- the formatted-output histogram. The latter only
            -- exists when chatty=True; the former is canonical.
            counts   = parseLabelCounts labelsBlock
            dist     = if null counts
                         then parseLabelDistribution out  -- legacy fallback
                         else countsToDistribution counts
            warnings = biasWarnings dist
            -- Use the labels block as 'qc_raw_output' so the agent
            -- (and any failing e2e) can see what cabal-repl actually
            -- emitted between the LABELS sentinels — this is what
            -- the structured parser ate (or didn't).
            rawForResponse =
              if T.null labelsBlock then out else labelsBlock
        in pure (renderReport args qcResult dist warnings rawForResponse
                              (truncate ((t1 - t0) * 1000)))

--------------------------------------------------------------------------------
-- pure helpers
--------------------------------------------------------------------------------

-- | Issue #65: bucket an input size into one of four canonical
-- ranges. Phase 1 uses these four buckets (matching the issue's
-- response shape) so the histogram stays human-readable for any
-- input type.
bucketSize :: Int -> Text
bucketSize n
  | n <= 0    = "0"
  | n <= 5    = "1-5"
  | n <= 20   = "6-20"
  | otherwise = ">20"

-- | Issue #65: synthesise the instrumented property. Wraps the
-- user-supplied lambda with a 'Test.QuickCheck.collect' call
-- whose label is the size-bucket of the show-rendered input.
--
-- @
--   \\args -> Test.QuickCheck.collect ("size:" ++ bucketSize ...)
--                                    ((originalProp) args)
-- @
--
-- We thread the @runs@ count through 'Test.QuickCheck.withMaxSuccess'
-- so the harness honours the user's request without us having to
-- modify the cabal-repl driver.
--
-- Phase 1 caveats (intentional, documented in the descriptor):
--
--   * @show@-length is a proxy for structural size; it works well
--     for lists/strings/tuples, less so for numeric inputs (every
--     'Int' shows as 1–11 chars). Phase 2 will use the type's data
--     constructors.
--   * The wrapper assumes the original property is a single-arg
--     lambda — exactly the shape 'ghc_quickcheck' already accepts.
buildInstrumentedProperty :: Text -> Int -> Text
buildInstrumentedProperty prop runs =
  T.concat
    [ "Test.QuickCheck.withMaxSuccess "
    , T.pack (show runs)
    , " (\\args -> Test.QuickCheck.collect "
    , "(\"size:\" ++ "
    , bucketSizeFn
    , " (length (show args))) "
    , "((", T.strip prop, ") args))"
    ]
  where
    -- Inline let — keeps the wrapper a single expression so it
    -- slots into the existing repl harness without needing a
    -- multi-line :{ … :} block.
    bucketSizeFn =
      "(\\n -> if n <= 0 then \"0\" \
      \else if n <= 5 then \"1-5\" \
      \else if n <= 20 then \"6-20\" \
      \else \">20\")"

-- | Issue #65: parse QuickCheck's label histogram. Lines look like
-- @"35.5% size:0-1"@ or @"100.0% size:>20"@. We tolerate both
-- integer (@35%@) and decimal (@35.5%@) percentages and any
-- amount of leading whitespace.
parseLabelDistribution :: Text -> [(Text, Double)]
parseLabelDistribution raw =
  let candidates = mapMaybe parseLine (T.lines raw)
  in sortOn (Data.Ord.Down . snd) candidates
  where
    parseLine ln =
      let stripped = T.strip ln
          (numTxt, rest) = T.break (== '%') stripped
          numTxtClean    = T.strip numTxt
      in case T.uncons rest of
        Just ('%', after) ->
          let labelTxt = T.strip after
          in case readDouble (T.unpack numTxtClean) of
               Just pct | not (T.null labelTxt) ->
                 Just (labelTxt, pct)
               _ -> Nothing
        _ -> Nothing

    -- Tolerate both integer (\"35\") and decimal (\"35.5\") forms.
    readDouble :: String -> Maybe Double
    readDouble s =
      let trimmed = dropWhile (== ' ') s
          digits  = takeWhile (\c -> isDigit c || c == '.') trimmed
      in readMaybe digits

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ []     = []
    mapMaybe f (x:xs) = case f x of
      Just y  -> y : mapMaybe f xs
      Nothing -> mapMaybe f xs

-- | Issue #78: parse the structured labels block emitted by
-- 'runQuickCheckWithLabelsViaCabalRepl'. Each line is
-- @"<label>\\t<count>"@. Returns @[(label, count)]@.
--
-- Robust to leading/trailing whitespace and silently skips
-- malformed lines. We don't fail the witness over a single
-- corrupt row — every other label still informs the agent.
parseLabelCounts :: Text -> [(Text, Int)]
parseLabelCounts raw =
  let parseLine ln = case T.splitOn "\t" (T.strip ln) of
        [lbl, cnt] | not (T.null lbl)
                   , Just n <- readMaybe (T.unpack (T.strip cnt))
                   -> Just (lbl, n)
        _          -> Nothing
  in foldr (\ln acc -> maybe acc (:acc) (parseLine ln))
           []
           (T.lines raw)

-- | Issue #78: convert structured @[(label, count)]@ pairs into
-- the legacy @[(label, percent)]@ shape the renderer + bias
-- detector consume. Total is the sum of counts; if zero (no
-- labels recorded), returns an empty distribution.
countsToDistribution :: [(Text, Int)] -> [(Text, Double)]
countsToDistribution counts =
  let total = fromIntegral (sum (map snd counts)) :: Double
  in if total <= 0
       then []
       else [ (label, fromIntegral n / total * 100)
            | (label, n) <- counts
            ]

-- | Issue #65: emit a 'biased-distribution' warning for any
-- bucket whose share is < 1 %% of the total runs. Phase 1 only
-- inspects the size dimension (the only one Phase 1 instruments).
biasWarnings :: [(Text, Double)] -> [Text]
biasWarnings dist =
  [ "biased-bucket: '"
      <> label
      <> "' holds only "
      <> T.pack (show pct)
      <> "% of runs (< 1% threshold)"
  | (label, pct) <- dist
  , pct < 1.0
  , "size:" `T.isPrefixOf` label
  ]

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: the witness report is informational —
-- distribution warnings are flagged as 'warnings' under 'result'
-- but the run itself is always 'ok' (tool successfully measured).
-- Consumers branch on the structured 'distribution' / 'warnings'
-- fields. The legacy in-payload 'nextStep' object is preserved
-- because the existing nextStep injection plumbing keys on it.
renderReport
  :: WitnessArgs -> QuickCheckResult
  -> [(Text, Double)] -> [Text] -> Text -> Int -> ToolResult
renderReport args qc dist warnings rawForResponse wallMs =
  let (passed, failed, _qcRaw) = qcCounts qc
      raw = rawForResponse
      sizeDist = filter (("size:" `T.isPrefixOf`) . fst) dist
      payload = object
        [ "property"     .= waProperty args
        , "module"       .= waModulePath args
        , "runs"         .= waRuns args
        , "passed"       .= passed
        , "failed"       .= failed
        , "distribution" .= object
            [ "by_size" .= object
                [ "buckets" .= map renderBucket sizeDist
                , "total_labels" .= length sizeDist
                ]
            ]
        , "warnings"     .= map (\w -> object [ "kind" .= ("biased-distribution" :: Text)
                                              , "message" .= w
                                              ]) warnings
        , "wall_time_ms" .= wallMs
        , "phase"        .= ("1-mvp" :: Text)
        , "deferred"     .= ([ "by-constructor"
                             , "uncovered-branches"
                             , "smallest-witness"
                             , "custom-classify-by"
                             ] :: [Text])
        , "qc_raw_output" .= T.take 1000 raw
        , "nextStep"      .= object
            [ "tool"    .= ("ghc_quickcheck" :: Text)
            , "why"     .= ("Re-run the same property with ghc_quickcheck "
                            <> "to verify the pass/fail signal "
                            <> "without the extra instrumentation overhead." :: Text)
            , "example" .= object
                [ "property"    .= waProperty args
                , "module_path" .= waModulePath args
                ]
            ]
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

renderBucket :: (Text, Double) -> Value
renderBucket (label, pct) = object
  [ "label"   .= label
  , "percent" .= pct
  ]

qcCounts :: QuickCheckResult -> (Int, Int, Text)
qcCounts = \case
  QcPassed _ n          -> (n, 0, "")
  QcFailed _ n _ cex    -> (n, 1, cex)
  QcException _ msg     -> (0, 1, msg)
  QcGaveUp _ n d        -> (n, 0, T.pack ("gave up after " <> show d <> " discards"))
  QcUnparsed _ raw      -> (0, 0, raw)

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

-- | Issue #90 Phase C: cabal-repl subprocess threw.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))
