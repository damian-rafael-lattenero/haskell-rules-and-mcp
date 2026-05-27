-- | @ghc_witness@ — property witness explorer (#65).
--
-- When QuickCheck reports @100/100 passed@, it doesn't tell you
-- WHAT it tested. Were all 100 inputs empty lists? Was the
-- recursive case exercised at all? @ghc_witness@ inverts the
-- usual signal — instead of \"did it pass?\" it answers \"what
-- was tested, and what wasn't?\".
--
-- Phase 1 (MVP): size-based distribution via @show@-length proxy.
--
-- Phase 2 (this commit): @classify_by=\"constructor\"@ uses
-- @head (words (show args))@ as the label, yielding per-constructor
-- counts for any @Show@-able algebraic type. No GHC API dependency —
-- the @show@ output directly exposes the leading constructor name.
-- Set @classify_by=\"size\"@ (the default) for Phase-1 behaviour.
--
-- Remaining deferrals:
--   * Uncovered-branch detection — needs data-constructor enumeration
--     via the GHC API to know which constructors are ABSENT.
--   * Smallest-witness extraction — requires an inverted probe + re-run.
module HaskellFlows.Tool.Witness
  ( descriptor
  , handle
  , WitnessArgs (..)
  , ClassifyBy (..)           -- #199: exported for unit tests
    -- * Pure helpers (exported for unit tests)
  , bucketSize
  , buildInstrumentedProperty
  , buildConstructorProperty
  , parseLabelDistribution
  , parseLabelCounts
  , countsToDistribution
  , biasWarnings
  , isPrimitiveBuckets        -- #199: exported for unit tests
  , renderReport              -- #199: exported for unit tests
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

import HaskellFlows.Ghc.ApiSession (GhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
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
        "Property-witness explorer. Runs the property through QuickCheck "
          <> "with distribution instrumentation, then surfaces the input "
          <> "histogram and flags biased buckets (< 1 %% of total runs). "
          <> "Useful when '+++ OK, passed N tests' looks suspicious. "
          <> "classify_by='size' (default) buckets by show-length. "
          <> "classify_by='constructor' extracts the leading constructor "
          <> "name from show-output — useful for Maybe/Either/list inputs. "
          <> "OUTPUT: distribution (by_size or by_constructor), warnings "
          <> "(biased-bucket entries), wall_time_ms (subprocess time only), "
          <> "deferred (list of features not yet implemented: "
          <> "uncovered-branches, smallest-witness). "
          <> "NOTE: instrumentation adds ~55 ms over a plain ghc_quickcheck "
          <> "run because each of the N test cases executes a collect call. "
          <> "Uses the in-process GHC API session (#220) — no subprocess overhead."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "required"   .= (["property"] :: [Text])
          , "properties" .= object
              [ "property"      .= obj "string"
              , "module_path"   .= obj "string"
              , "runs"          .= obj "integer"
              , "classify_by"   .= obj "string"
              ]
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

-- | Phase 2: classification mode.
data ClassifyBy
  = ClassifyBySize         -- ^ Phase 1 default: show-length buckets
  | ClassifyByConstructor  -- ^ Phase 2: leading constructor name from show
  deriving stock (Show, Eq)

parseClassifyBy :: Text -> ClassifyBy
parseClassifyBy t
  | T.strip t == "constructor" = ClassifyByConstructor
  | otherwise                  = ClassifyBySize

data WitnessArgs = WitnessArgs
  { waProperty    :: !Text
  , waModulePath  :: !(Maybe Text)
  , waRuns        :: !Int
  , waClassifyBy  :: !ClassifyBy
    -- ^ Phase 2: 'ClassifyByConstructor' uses the leading constructor
    -- name from @show args@ as the label. Default: 'ClassifyBySize'.
  }
  deriving stock (Show)

instance FromJSON WitnessArgs where
  parseJSON = withObject "WitnessArgs" $ \o -> do
    prop <- o .:  "property"
    mp   <- o .:? "module_path"
    rs   <- o .:? "runs"        .!= 1000
    cb   <- o .:? "classify_by" .!= ("size" :: Text)
    pure WitnessArgs
      { waProperty   = prop
      , waModulePath = mp
      , waRuns       = max 100 (min 10000 rs)
      , waClassifyBy = parseClassifyBy cb
      }

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> do
    -- Build the instrumented property string first (pure, instant).
    -- #171: start the timer AFTER this step so wall_time_ms reflects
    -- only the subprocess time, not the (negligible) string-building
    -- overhead that was previously inside the measured window.
    let instrumented = case waClassifyBy args of
          ClassifyBySize        -> buildInstrumentedProperty   (waProperty args) (waRuns args)
          ClassifyByConstructor -> buildConstructorProperty    (waProperty args) (waRuns args)
    t0 <- realToFrac <$> getPOSIXTime :: IO Double
    -- Issue #220: replaced the cabal-repl subprocess (~40s startup) with
    -- the in-process GHC API path. 'runQuickCheckWithLabelsInProcess'
    -- loads the test-suite stanza, augments the interactive context with
    -- Test.QuickCheck/Data.Map/Data.List, then compiles and runs the
    -- instrumented property via 'evalIOString'. Same (out, labels, err)
    -- shape — 'parseLabelCounts' and 'extractQcOutput' work unchanged.
    res <- try @SomeException $
      Qc.runQuickCheckWithLabelsInProcess ghcSess
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

-- | Phase 2: synthesise an instrumented property that labels each
-- input by the leading constructor name extracted from @show args@.
--
-- #239: For list types, @show [1,2,3]@ → @\"[1,2,3]\"@ (no spaces),
-- so @head (words (show args))@ would give the full repr as a single
-- word — one bucket per unique list. We detect the list case specially:
-- if the show output starts with @\"[]\"@ it is an empty list (@[]@
-- constructor), otherwise it starts with @[@ → non-empty list (@(:)@
-- constructor). For all other ADTs, @head (words (show args))@
-- extracts the leading constructor name (e.g. @show (Just 5)@ →
-- @\"Just 5\"@ → @\"Just\"@).
--
-- Label format: @"ctor:X"@ (parallel to Phase 1's @"size:Y"@).
buildConstructorProperty :: Text -> Int -> Text
buildConstructorProperty prop runs =
  T.concat
    [ "Test.QuickCheck.withMaxSuccess "
    , T.pack (show runs)
    , " (\\args -> Test.QuickCheck.collect "
    , "(\"ctor:\" ++ "
    , ctorExtractFn
    , " (show args)) "
    , "((", T.strip prop, ") args))"
    ]
  where
    -- Inline helper that detects the leading constructor from show output.
    -- For list show-output (starts with '['), return "[]" or "(:)".
    -- For all other types, take the first word (up to first space).
    -- This avoids the #239 bug where show [1,2,3] = "[1,2,3]" (no spaces)
    -- causing one bucket per unique list representation.
    ctorExtractFn =
      "(\\s -> case s of \
      \  []      -> \"()\"; \
      \  ('[':_) -> if s == \"[]\" then \"[]\" else \"(:)\"; \
      \  _       -> takeWhile (\\c -> c /= ' ' && c /= '(') s)"

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

-- | #199: Detect whether @classify_by=constructor@ is producing
-- numeric noise instead of ADT-constructor signal.
--
-- Haskell data constructors must start with an uppercase letter.
-- When @show@ is applied to a numeric type (@Int@, @Double@, etc.)
-- the result starts with a digit (@\"42\"@) or a minus sign
-- (@\"-1\"@).  When more than 80 %% of the @\"ctor:\"@-prefixed
-- bucket labels fall into this category we know the user fed a
-- primitive type — the constructor breakdown is useless noise.
isPrimitiveBuckets :: [(Text, Double)] -> Bool
isPrimitiveBuckets [] = False
isPrimitiveBuckets ctorDist =
  let total    = fromIntegral (length ctorDist) :: Double
      primCount = fromIntegral (length (filter (isPrimLabel . fst) ctorDist)) :: Double
  in primCount / total > 0.8
  where
    isPrimLabel lbl =
      let core = T.drop (T.length "ctor:") lbl
      in case T.uncons core of
           Just (c, _) -> isDigit c || c == '-'
           Nothing     -> False

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: the witness report is informational —
-- distribution warnings are flagged as 'warnings' under 'result'
-- but the run itself is always 'ok' (tool successfully measured).
-- Consumers branch on the structured 'distribution' / 'warnings'
-- fields. #119: the in-payload 'nextStep' has been removed; the
-- top-level injection in 'enrichWithNextStep' is the sole source.
renderReport
  :: WitnessArgs -> QuickCheckResult
  -> [(Text, Double)] -> [Text] -> Text -> Int -> ToolResult
renderReport args qc dist warnings rawForResponse wallMs =
  let (passed, failed, _qcRaw) = qcCounts qc
      raw = rawForResponse
      -- Phase 2: route labels to the right distribution field based on mode.
      isCtor = waClassifyBy args == ClassifyByConstructor
      ctorDist = filter (("ctor:" `T.isPrefixOf`) . fst) dist
      sizeDist = filter (("size:" `T.isPrefixOf`) . fst) dist
      -- #199 Bug 1: when constructor mode was requested but the buckets are
      -- all numeric (digits / minus-sign labels), fall back to size and warn.
      primFallback = isCtor && isPrimitiveBuckets ctorDist
      effectiveCtor = isCtor && not primFallback
      distObj = if effectiveCtor
        then object
          [ "by_constructor" .= object
              [ "buckets"       .= map renderBucket ctorDist
              , "total_labels"  .= length ctorDist
              ]
          ]
        else object
          [ "by_size" .= object
              [ "buckets"      .= map renderBucket sizeDist
              , "total_labels" .= length sizeDist
              ]
          ]
      -- Build the warnings array, prepending the primitive-fallback notice
      -- with its own kind tag so consumers can distinguish it from
      -- the generic bias warnings.
      biasObjs =
        map (\w -> object [ "kind"    .= ("biased-distribution" :: Text)
                          , "message" .= w
                          ]) warnings
      primObjs =
        [ object [ "kind"    .= ("primitive-constructor-fallback" :: Text)
                 , "message" .= ("classify_by=constructor is not meaningful "
                                  <> "for numeric/primitive types; "
                                  <> "showing size distribution instead" :: Text)
                 ]
        | primFallback
        ]
      allWarningObjs = primObjs ++ biasObjs
      -- #199 Bug 2: flag silent raw-output truncation.
      rawTruncated = T.length raw > 1000
      payload = object
        ( [ "property"     .= waProperty args
          , "module"       .= waModulePath args
          , "runs"         .= waRuns args
          , "classify_by"  .= (if effectiveCtor then "constructor" else "size" :: Text)
          , "passed"       .= passed
          , "failed"       .= failed
          , "distribution" .= distObj
          , "warnings"     .= allWarningObjs
          , "wall_time_ms" .= wallMs
          , "phase"        .= ("2-constructor" :: Text)
          , "deferred"     .= ([ "uncovered-branches"
                               , "smallest-witness"
                               ] :: [Text])
          , "qc_raw_output" .= T.take 1000 raw
          ]
          ++ [ "raw_truncated" .= True | rawTruncated ]
        )
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


-- | Issue #90 Phase C: cabal-repl subprocess threw.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))
