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
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)

import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , Suggestion (..)
  , applyRules
  )
import qualified HaskellFlows.Tool.Determinism as DeterminismTool
import qualified HaskellFlows.Tool.QuickCheck as Qc
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
        "Module-wide property-first audit. For every top-level "
          <> "binding in the supplied module, propose candidate "
          <> "QuickCheck laws via the same engine 'ghc_suggest' uses, "
          <> "filter by min_confidence, and run each via "
          <> "'ghc_quickcheck'. Passing properties auto-persist to "
          <> "the regression store. Set determinism_runs>0 to re-run "
          <> "each passing property N times and flag unstable ones. "
          <> "Arbitrary-template generation is still deferred."
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

handle :: GhcSession -> Store -> ProjectDir -> Value -> IO ToolResult
handle ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right args -> case mkModulePath pd (T.unpack (laModulePath args)) of
    Left e   -> pure (pathTraversalResult (T.pack (show e)))
    Right mp -> do
      let full = unModulePath mp
      eBody <- try (TIO.readFile full)
                 :: IO (Either SomeException Text)
      case eBody of
        Left e -> pure (subprocessResult
          (T.pack ("Could not read module: " <> show e)))
        Right body -> runLab ghcSess store pd args (laModulePath args) body


-- | Issue #90 Phase C: 'mkModulePath' rejection.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | Issue #90 Phase C: filesystem read failure.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))

runLab
  :: GhcSession -> Store -> ProjectDir
  -> LabArgs -> Text -> Text -> IO ToolResult
runLab ghcSess store pd args modulePath body = do
  t0 <- realToFrac <$> getPOSIXTime :: IO Double
  let bindings = listTopLevelBindings body
  perFn <- mapM (auditOne ghcSess store pd args modulePath) bindings
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
  , poStatus     :: !Text        -- "passed" | "failed" | "skipped"
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

auditOne
  :: GhcSession -> Store -> ProjectDir -> LabArgs -> Text -> Binding
  -> IO FunctionReport
auditOne ghcSess store pd args modulePath bind =
  case parseSignature (bSignature bind) of
    Nothing -> pure FunctionReport
      { frName       = bName bind
      , frSignature  = bSignature bind
      , frProperties = []
      , frReason     = "signature-parse-failed"
      }
    Just sig ->
      let suggestions = filter
            (confidenceAtLeast (laMinConfidence args) . sConfidence)
            (applyRules (bName bind) sig)
      in if null suggestions
           then pure FunctionReport
             { frName       = bName bind
             , frSignature  = bSignature bind
             , frProperties = []
             , frReason     = "no-laws-matched"
             }
           else do
             outs <- mapM (runProperty ghcSess store pd args modulePath) suggestions
             pure FunctionReport
               { frName       = bName bind
               , frSignature  = bSignature bind
               , frProperties = outs
               , frReason     = ""
               }

-- | Drive 'Tool.QuickCheck.handle' once and translate its JSON
-- payload into our compact 'PropertyOutcome'.
--
-- Phase 2: when 'laDeterminismRuns' > 0 and the property passes,
-- also runs it N more times via 'DeterminismTool.handle' and sets
-- 'poStability' to @"stable"@ or @"unstable"@.
runProperty
  :: GhcSession -> Store -> ProjectDir -> LabArgs -> Text -> Suggestion
  -> IO PropertyOutcome
runProperty ghcSess store pd args modulePath sug = do
  let qcArgs = object
        [ "property" .= sProperty sug
        , "module"   .= modulePath
        ]
  res <- Qc.handle store ghcSess qcArgs
  let payload = decodeFirst (trContent res)
      status  = decideStatus payload
      detail  = fromMaybe "" (lookupString "raw" payload)
  -- Phase 2: determinism check on passing properties when enabled.
  stability <- if status == "passed" && laDeterminismRuns args > 0
                 then checkDeterminism ghcSess args modulePath (sProperty sug)
                 else pure Nothing
  pure PropertyOutcome
    { poLaw        = sLaw sug
    , poCategory   = sCategory sug
    , poConfidence = sConfidence sug
    , poExpression = sProperty sug
    , poStatus     = status
    , poDetail     = T.take 400 detail
    , poStability  = stability
    }
  where
    _ = pd  -- pd kept in signature for cohesion; unused in Phase 1
    -- Decode the tool-response envelope and peel the @result@ wrapper
    -- so downstream field lookups (@"state"@, @"passed"@) find what
    -- they expect.  'Qc.handle' returns
    -- @{"status":"ok","result":{"state":"passed",...}}@ — before this
    -- fix, @lookupString "state"@ was searching the top-level object
    -- (which has @"status"@, not @"state"@) and always fell back to
    -- @"unknown"@.  That made the lab report @properties_passed: 0@
    -- even when the store was growing (issue #104).
    decodeFirst (TextContent t : _) =
      case decode (TLE.encodeUtf8 (TL.fromStrict t)) of
        Just (Object o) ->
          case AKM.lookup (AKey.fromText "result") o of
            Just r -> r
            Nothing -> Object o
        Just v  -> v
        Nothing -> Null
    decodeFirst _ = Null

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
  res <- DeterminismTool.handle ghcSess detArgs
  let payload = decodeToolJson (trContent res)
  pure $ case lookupString "status" payload of
    Just "ok" -> Just "stable"
    _         ->
      -- 'failed' status or parse failure both map to "unstable" so
      -- the agent sees a signal even if the determinism tool itself
      -- hit an unexpected error.
      Just "unstable"
  where
    decodeToolJson (TextContent t : _) =
      fromMaybe Null (decode (TLE.encodeUtf8 (TL.fromStrict t)))
    decodeToolJson _ = Null

decideStatus :: Value -> Text
decideStatus payload = case lookupString "state" payload of
  Just s  -> s   -- usually "passed" / "failed" / "exception" / "gave_up"
  Nothing -> case lookupBool "success" payload of
    Just True  -> "passed"
    Just False -> "failed"
    Nothing    -> "unknown"

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | The lab report is informational — status='ok' always;
-- consumers branch on the structured 'covered'/'uncovered' fields.
--
-- Phase 2: when 'laDeterminismRuns' > 0 the report includes a
-- 'determinism_runs' field and each property object gains a
-- 'stability' key (@"stable"@ / @"unstable"@).
renderReport :: LabArgs -> Text -> [FunctionReport] -> Int -> ToolResult
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
  in Env.toolResponseToResult (Env.mkOk payload)

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


--------------------------------------------------------------------------------
-- JSON walk helpers
--------------------------------------------------------------------------------

lookupString :: Text -> Value -> Maybe Text
lookupString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

lookupBool :: Text -> Value -> Maybe Bool
lookupBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = AKM.lookup (AKey.fromText k) o
lookupField _ _          = Nothing
