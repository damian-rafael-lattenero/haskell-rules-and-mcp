-- | @ghc_lab@ — module-wide property-first audit (#60).
--
-- Phase 1 (MVP, total scope estimated at 1 week): orchestrate the
-- existing primitives into a single call so the agent stops paying
-- the 12-18-round-trip tax for a module audit.
--
-- Pipeline per binding:
--   1. Walk the module's top-level signatures (regex over the
--      source — the GHC API already loaded the module via the
--      caller's preceding 'ghc_load').
--   2. Run 'Suggest.Rules.applyRules' filtered by 'min_confidence'.
--   3. For each suggestion, route through 'Tool.QuickCheck' so
--      passing properties auto-persist via the same code path as
--      the standalone tool (no duplicate code; idempotent on
--      retry).
--   4. Aggregate per-function reports.
--
-- Phase 1 deferrals (documented in the response so the agent
-- knows what to expect):
--
--   * 'generate_missing_arbitrary' — return Arbitrary suggestions.
--     Phase 1 reports an empty array; the agent runs 'ghc_arbitrary'
--     manually for now.
--   * 'determinism_runs' — Phase 1 ignores it. Phase 2 wires
--     'ghc_determinism' into the per-property loop.
--   * Coverage delta vs the project's PropertyStore.
module HaskellFlows.Tool.Lab
  ( descriptor
  , handle
  , LabArgs (..)
    -- * Pure helpers (exported for unit tests)
  , Binding (..)
  , listTopLevelBindings
  , confidenceAtLeast
  , qcResultStatus   -- #201
  , qcResultDetail   -- #201
    -- * Issue #254 — compute suggest (exported for unit tests)
  , computeSuggest
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesFileExist)

import HaskellFlows.Data.PropertyStore (Store, saveCases)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..))
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , Suggestion (..)
  , applyRules
  )
import qualified HaskellFlows.Tool.Determinism as DeterminismTool
import qualified HaskellFlows.Tool.QuickCheck as Qc
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Types
  ( ProjectDir
  , mkModulePath
  , unModulePath
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcLab
    , tdDescription =
        "PURPOSE: Module-wide property-first audit — suggest and run "
          <> "QuickCheck laws for every top-level binding. "
          <> "WHEN: discovering laws across a whole module in one call; "
          <> "filter by min_confidence; determinism_runs>0 re-runs each "
          <> "passing property N times to flag unstable ones. "
          <> "WHEN NOT: ghc_suggest for a single function; ghc_quickcheck "
          <> "to run one property you already have. "
          <> "PREREQUISITES: the module loaded; QuickCheck in a stanza. "
          <> "OUTPUT: {bindings:[{name, suggestions, results}]}; passing "
          <> "properties auto-persist (Arbitrary-template generation is "
          <> "still deferred). "
          <> "SEE ALSO: ghc_suggest, ghc_quickcheck, ghc_property_store."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path"       .= obj "string"
              , "min_confidence"    .= obj "string"
              , "determinism_runs"  .= obj "integer"
              ]
          , "required"             .= (["module_path"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data LabArgs = LabArgs
  { laModulePath      :: !Text
  , laMinConfidence   :: !Confidence
  , laDeterminismRuns :: !Int
    -- ^ Phase 2: when > 0, each passing property is re-run this many
    -- times via 'ghc_determinism' to detect flakiness. Default 0
    -- (disabled) keeps Phase-1 behaviour and avoids the extra cabal-
    -- repl overhead for quick audits.
  }
  deriving stock (Show)

instance FromJSON LabArgs where
  parseJSON = withObject "LabArgs" $ \o -> do
    mp <- o .:  "module_path"
    mc <- o .:? "min_confidence"   .!= "medium"
    dr <- o .:? "determinism_runs" .!= (0 :: Int)
    pure LabArgs
      { laModulePath      = mp
      , laMinConfidence   = parseConfidence mc
      , laDeterminismRuns = max 0 (min 10 dr)
      }

parseConfidence :: Text -> Confidence
parseConfidence raw = case T.toLower raw of
  "low"    -> Low
  "medium" -> Medium
  "high"   -> High
  _        -> Medium

confidenceAtLeast :: Confidence -> Confidence -> Bool
confidenceAtLeast threshold candidate =
  rank candidate >= rank threshold
  where
    rank :: Confidence -> Int
    rank Low    = 0
    rank Medium = 1
    rank High   = 2

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = do
  ghcSess <- teSession env
  store   <- teStore env
  pd      <- teProjectDir env
  runHandle ghcSess store pd rawArgs

runHandle :: GhcSession -> Store -> ProjectDir -> Value -> IO ToolResponse
runHandle ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> case mkModulePath pd (T.unpack (laModulePath args)) of
    Left e   -> pure (pathTraversalResult (T.pack (show e)))
    Right mp -> do
      let full = unModulePath mp
      -- #160: check existence before reading — a missing file was
      -- previously reported as kind=subprocess_error because
      -- TIO.readFile throws an IOException. Use the semantically
      -- correct kind=module_path_does_not_exist instead.
      exists <- doesFileExist full
      if not exists
        then pure (Env.mkFailed
          ((Env.mkErrorEnvelope Env.ModulePathDoesNotExist
              ("module_path '" <> laModulePath args <> "' does not exist"))
                { Env.eeField = Just "module_path" }))
        else do
          eBody <- try (TIO.readFile full)
                     :: IO (Either SomeException Text)
          case eBody of
            Left e -> pure (subprocessResult
              (T.pack ("Could not read module: " <> show e)))
            Right body -> runLab ghcSess store pd args (laModulePath args) body


-- | Issue #90 Phase C: 'mkModulePath' rejection.
pathTraversalResult :: Text -> ToolResponse
pathTraversalResult msg =
  Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg)

-- | Issue #90 Phase C: filesystem read failure.
subprocessResult :: Text -> ToolResponse
subprocessResult msg =
  Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg)

runLab
  :: GhcSession -> Store -> ProjectDir
  -> LabArgs -> Text -> Text -> IO ToolResponse
runLab ghcSess store pd args modulePath body = do
  t0 <- realToFrac <$> getPOSIXTime :: IO Double
  let bindings = listTopLevelBindings body
      -- Pure: suggestions per binding (Left = skipped reason, Right = candidates)
      analyzed    = [(b, computeSuggest args b) | b <- bindings]
      -- Flatten: all (binding, suggestion) pairs across every binding with
      -- at least one matching law — batch them into a single subprocess.
      flatWork    = [ (b, sug)
                    | (b, Right sugs) <- analyzed
                    , sug             <- sugs
                    ]
      flatProps   = map (sProperty . snd) flatWork
  -- Single batch run — one cabal v2-repl startup for ALL suggestions (#201).
  batchQc <- Qc.runBatchPropertiesViaCabalRepl pd (Just modulePath) flatProps
  -- Persist passing properties to the regression store.
  mapM_ (\((_, sug), qr) ->
           case qr of
             QcPassed _ _ -> saveCases store (sProperty sug) (Just modulePath) Qc.qcMaxSuccess
             _            -> pure ())
         (zip flatWork batchQc)
  -- Phase 2: determinism (per-property, only on passing; still individual runs).
  batchStab <-
    if laDeterminismRuns args > 0
      then mapM (\((_, sug), qr) ->
                   case qr of
                     QcPassed _ _ ->
                       checkDeterminism ghcSess args modulePath (sProperty sug)
                     _ -> pure Nothing)
                (zip flatWork batchQc)
      else pure (replicate (length flatWork) Nothing)
  -- Distribute flat results back into per-binding function reports.
  let triples = zip3 (map snd flatWork) batchQc batchStab
      perFn   = distributeResults analyzed triples
  t1 <- realToFrac <$> getPOSIXTime :: IO Double
  pure (renderReport args modulePath perFn (truncate ((t1 - t0) * 1000)))

--------------------------------------------------------------------------------
-- top-level binding extraction
--------------------------------------------------------------------------------

-- | A top-level binding discovered in a module body.
data Binding = Binding
  { bName      :: !Text  -- ^ identifier
  , bSignature :: !Text  -- ^ raw signature text (after \"::\")
  }
  deriving stock (Eq, Show)

-- | Phase 1: the listing is a regex-style line-walk over the
-- module body. We pick up @name :: <sig>@ lines starting at
-- column 0. Pattern-bound declarations and class-method
-- defaults are deferred to Phase 2.
--
-- Multi-line signatures are joined: any continuation line that
-- starts with whitespace AND immediately follows a recognised
-- signature line is appended to the previous binding's
-- signature.
listTopLevelBindings :: Text -> [Binding]
listTopLevelBindings body = walk (T.lines body) Nothing []
  where
    walk [] (Just b) acc = reverse (b : acc)
    walk [] Nothing  acc = reverse acc
    walk (ln : rest) curr acc =
      case parseSignatureLine ln of
        Just b ->
          walk rest (Just b) (close curr acc)
        Nothing ->
          -- Phase 1 multi-line shape:
          --   concatPairs
          --     :: (Eq a, Show b) => [(a, b)] -> [b]
          --     concatPairs = undefined
          -- A column-0 identifier alone, followed by a
          -- whitespace-leading line that begins with @::@, is
          -- still a signature.
          case (curr, parseBareNameLine ln, looksLikeColonStart rest) of
            (_, Just nm, Just sig) ->
              walk (drop 1 rest) (Just (Binding nm sig)) (close curr acc)
            _ -> case curr of
              Just b
                | isContinuation ln ->
                    walk rest (Just b { bSignature =
                                          bSignature b <> " "
                                           <> T.strip ln }) acc
              _ -> walk rest Nothing (close curr acc)

    close Nothing  acc = acc
    close (Just b) acc = b : acc

    isContinuation ln =
      not (T.null ln)
        && not (T.null (T.takeWhile (== ' ') ln))
        && not (T.null (T.strip ln))

    -- A line that is JUST a lowercase identifier at column 0
    -- (no spaces, no symbols).
    parseBareNameLine ln =
      let stripped = T.strip ln
      in if T.takeWhile (== ' ') ln /= ""
           then Nothing
           else case T.uncons stripped of
                  Just (c, _)
                    | isAsciiLower c
                    , T.all isIdent stripped
                    -> Just stripped
                  _ -> Nothing
      where
        isIdent c = isAsciiLower c
                 || isAsciiUpper c
                 || isDigit c
                 || c == '_' || c == '\''

    -- Does the next line look like an indented '::' continuation?
    -- If yes, return the joined signature (consuming this and
    -- subsequent indented lines as one signature).
    looksLikeColonStart [] = Nothing
    looksLikeColonStart (next : afterNext) =
      let stripped = T.stripStart next
      in case T.stripPrefix ":: " stripped of
           Just rhs ->
             let extras = takeWhile isContinuation afterNext
             in Just (T.strip rhs <> " "
                       <> T.unwords (map T.strip extras))
           Nothing -> Nothing

-- | Parse a single signature line of shape @name :: <sig>@.
-- Returns 'Nothing' on anything that isn't a top-level
-- signature.
parseSignatureLine :: Text -> Maybe Binding
parseSignatureLine ln =
  let stripped = T.strip ln
  in if T.null stripped || T.takeWhile (== ' ') ln /= ""
       then Nothing
       else case T.breakOn " :: " stripped of
              (lhs, rhs)
                | not (T.null rhs)
                , isIdent lhs
                -> Just Binding
                     { bName      = lhs
                     , bSignature = T.drop 4 rhs
                     }
              _ -> Nothing
  where
    isIdent t = case T.uncons t of
      Just (c, _) -> isAsciiLower c
      Nothing     -> False

--------------------------------------------------------------------------------
-- per-binding audit
--------------------------------------------------------------------------------

data PropertyOutcome = PropertyOutcome
  { poLaw        :: !Text
  , poCategory   :: !Text
  , poConfidence :: !Confidence
  , poExpression :: !Text
  , poStatus     :: !Text        -- "passed" | "failed" | "exception" | "gave_up" | "unparsed"
  , poDetail     :: !Text        -- extra info from quickcheck
  , poStability  :: !(Maybe Text)
    -- ^ Phase 2: @Nothing@ = not checked (determinism_runs=0 or status≠passed).
    --   @Just "stable"@ = all determinism runs passed.
    --   @Just "unstable"@ = at least one rerun failed.
  }
  deriving stock (Show)

data FunctionReport = FunctionReport
  { frName       :: !Text
  , frSignature  :: !Text
  , frProperties :: ![PropertyOutcome]
  , frReason     :: !Text   -- "" or e.g. "no-laws-matched"
  }
  deriving stock (Show)

-- | Pure: compute filtered suggestions for a single binding (#201).
-- Returns @Left reason@ when the binding has no runnable laws, or
-- @Right sugs@ when at least one suggestion passes the confidence
-- threshold.
--
-- Issue #254: disambiguate the empty-result reason so callers can
-- distinguish \"no rule template applies to this shape\"
-- (@"no-template-matched"@) from \"rules matched but all fell below
-- the confidence threshold\" (@"low-confidence"@). The old catch-all
-- @"no-laws-matched"@ lumped both into one opaque code.
computeSuggest :: LabArgs -> Binding -> Either Text [Suggestion]
computeSuggest args bind = case parseSignature (bSignature bind) of
  Nothing -> Left "signature-parse-failed"
  Just sig ->
    let allSugs = applyRules (bName bind) sig
        sugs    = filter
                    (confidenceAtLeast (laMinConfidence args) . sConfidence)
                    allSugs
    in if null allSugs
         then Left "no-template-matched"  -- no rule applies to this shape at all
         else if null sugs
           then Left "low-confidence"     -- rules matched but fell below threshold
           else Right sugs

-- | Pure: build a 'FunctionReport' from a binding and its pre-computed
-- QC result triples @(suggestion, qcResult, stability)@.
buildFnReport
  :: Binding
  -> Either Text [(Suggestion, QuickCheckResult, Maybe Text)]
  -> FunctionReport
buildFnReport bind (Left reason) =
  FunctionReport
    { frName       = bName bind
    , frSignature  = bSignature bind
    , frProperties = []
    , frReason     = reason
    }
buildFnReport bind (Right results) =
  FunctionReport
    { frName       = bName bind
    , frSignature  = bSignature bind
    , frProperties = map toOutcome results
    , frReason     = ""
    }

-- | Build a 'PropertyOutcome' from a suggestion + QC result pair.
toOutcome :: (Suggestion, QuickCheckResult, Maybe Text) -> PropertyOutcome
toOutcome (sug, qr, stab) = PropertyOutcome
  { poLaw        = sLaw sug
  , poCategory   = sCategory sug
  , poConfidence = sConfidence sug
  , poExpression = sProperty sug
  , poStatus     = qcResultStatus qr
  , poDetail     = T.take 400 (qcResultDetail qr)
  , poStability  = stab
  }

-- | Status string for a 'QuickCheckResult' (#201).
qcResultStatus :: QuickCheckResult -> Text
qcResultStatus (QcPassed _ _)    = "passed"
qcResultStatus QcFailed {}       = "failed"
qcResultStatus (QcException _ _) = "exception"
qcResultStatus QcGaveUp {}       = "gave_up"
-- #237: distinguish identifiable runtime failures in the raw output
-- from genuine "couldn't parse QuickCheck output" failures.
-- 'runBatchPropertiesViaCabalRepl' stores 'summariseStderr' in the
-- raw field when the repl exits non-zero, so stack overflows and
-- similar crashes surface here as recognisable substrings.
qcResultStatus (QcUnparsed _ raw)
  | any (`T.isInfixOf` T.toLower raw)
        [ "stack space overflow", "heap overflow", "out of memory"
        , "stack overflow",       "rts error" ] = "exception"
  | otherwise = "unparsed"

-- | Detail string for a 'QuickCheckResult' (#201). Surfaces the
-- counterexample for 'QcFailed' so the agent can read it directly
-- instead of digging into the @raw@ field.
-- #237: also surfaces the raw output for 'QcUnparsed' so the agent
-- can see the actual error (e.g. \"Stack space overflow\") instead
-- of a silent empty string.
qcResultDetail :: QuickCheckResult -> Text
qcResultDetail (QcFailed _ n shr cex) =
  "counterexample: " <> cex
    <> " (after " <> T.pack (show n) <> " passes, "
    <> T.pack (show shr) <> " shrinks)"
qcResultDetail (QcUnparsed _ raw)
  | not (T.null (T.strip raw)) = T.take 400 raw
qcResultDetail _ = ""

-- | Distribute the flat batch result triples back into per-binding
-- function reports (#201). Consumes the triple list sequentially —
-- the first @length sugs@ triples belong to the first binding with
-- @Right sugs@, and so on.
distributeResults
  :: [(Binding, Either Text [Suggestion])]
  -> [(Suggestion, QuickCheckResult, Maybe Text)]
  -> [FunctionReport]
distributeResults [] _ = []
distributeResults ((b, Left reason) : rest) ts =
  buildFnReport b (Left reason) : distributeResults rest ts
distributeResults ((b, Right sugs) : rest) ts =
  let (mine, remaining) = splitAt (length sugs) ts
  in buildFnReport b (Right mine) : distributeResults rest remaining

-- | Phase 2: run a passing property via 'DeterminismTool' to check
-- for flakiness. Returns @Just "stable"@ when all reruns pass,
-- @Just "unstable"@ when any rerun fails, or @Nothing@ on error.
checkDeterminism
  :: GhcSession -> LabArgs -> Text -> Text -> IO (Maybe Text)
checkDeterminism ghcSess args modulePath expr = do
  let detArgs = object
        [ "property" .= expr
        , "module"   .= modulePath
        , "runs"     .= laDeterminismRuns args
        ]
  res <- DeterminismTool.runHandle ghcSess detArgs
  pure $ if Env.reStatus res == Env.StatusOk
         then Just "stable"
         -- 'failed' status or any other non-ok maps to "unstable" so
         -- the agent sees a signal even if the determinism tool itself
         -- hit an unexpected error.
         else Just "unstable"

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | The lab report is informational — status='ok' always;
-- consumers branch on the structured 'covered'/'uncovered' fields.
--
-- Phase 2: when 'laDeterminismRuns' > 0 the report includes a
-- 'determinism_runs' field and each property object gains a
-- 'stability' key (@"stable"@ / @"unstable"@).
renderReport :: LabArgs -> Text -> [FunctionReport] -> Int -> ToolResponse
renderReport args modulePath fns wallMs =
  let totalProps = sum (map (length . frProperties) fns)
      passedProps = sum
        [ 1 | f <- fns, p <- frProperties f, poStatus p == "passed" ]
      coveredFns = length
        [ () | f <- fns, any ((== "passed") . poStatus) (frProperties f) ]
      unstableProps = length
        [ () | f <- fns, p <- frProperties f, poStability p == Just "unstable" ]
      uncovered  = length fns - coveredFns
      detRuns    = laDeterminismRuns args
      payload = object $
        [ "module_path"        .= modulePath
        , "audited_bindings"   .= length fns
        , "covered"            .= coveredFns
        , "uncovered"          .= uncovered
        , "properties_total"   .= totalProps
        , "properties_passed"  .= passedProps
        , "wall_time_ms"       .= wallMs
        , "functions"          .= map renderFn fns
        -- #119: omit 'arbitrary_suggestions' when empty (still deferred).
        -- Including an empty array suggests the feature exists and is broken
        -- rather than being intentionally unimplemented.
        , "summary"            .= summarise totalProps passedProps
                                            (length fns) coveredFns
        ] <>
        if detRuns > 0
          then [ "determinism_runs"    .= detRuns
               , "unstable_properties" .= unstableProps
               ]
          else []
  in Env.mkOk payload

renderFn :: FunctionReport -> Value
renderFn f = object $
  [ "name"      .= frName f
  , "signature" .= frSignature f
  ] <> case frReason f of
         "" -> [ "properties" .= map renderProp (frProperties f) ]
         r  -> [ "status" .= ("skipped" :: Text)
               , "reason" .= r
               ]

renderProp :: PropertyOutcome -> Value
renderProp p = object $
  [ "law"        .= poLaw p
  , "category"   .= poCategory p
  , "confidence" .= confidenceText (poConfidence p)
  , "expression" .= poExpression p
  , "status"     .= poStatus p
  , "detail"     .= poDetail p
  ] <> case poStability p of
         Nothing  -> []
         Just stb -> [ "stability" .= stb ]

confidenceText :: Confidence -> Text
confidenceText Low    = "low"
confidenceText Medium = "medium"
confidenceText High   = "high"

summarise :: Int -> Int -> Int -> Int -> Text
summarise total passed nFns covered =
  T.pack (show passed) <> "/" <> T.pack (show total)
    <> " properties passed across " <> T.pack (show covered) <> "/"
    <> T.pack (show nFns) <> " functions."


