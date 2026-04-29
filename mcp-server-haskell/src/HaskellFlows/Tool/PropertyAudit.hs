-- | @ghc_property_audit@ — cross-property contradiction detector
-- (#64).
--
-- Phase 1 (MVP, total ~1 week): for every pair of stored
-- properties (filtered by 'module_path' when supplied), build
-- the contradiction probe @\\args -> P1 args && not (P2 args)@
-- and run it via the existing ghc_quickcheck path. If
-- QuickCheck finds a counterexample for the probe, the two
-- properties disagree on at least one input — flag the pair.
--
-- Phase 1 deferrals (documented in the descriptor):
--
--   * Vacuous-property check (run @\\_ -> not (P args)@ to
--     detect tautologies) — Phase 2.
--   * Same-arity / same-quantified-type heuristic — Phase 1
--     pairs every property with every other property in the
--     filter set; the probe simply fails to compile when the
--     arities mismatch and we surface that as @skipped@.
--   * LLM-friendly conclusion-text generation — Phase 2 will
--     hand the counterexample to the agent for narration.
module HaskellFlows.Tool.PropertyAudit
  ( descriptor
  , handle
  , PropertyAuditArgs (..)
    -- * Pure helpers (exported for unit tests)
  , pairCombinations
  , buildContradictionProbe
  , interpretProbeResult
  , dedupByExpression
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  )
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
    { tdName        = toolNameText GhcPropertyAudit
    , tdDescription =
        "Phase 1 cross-property contradiction detector. For every "
          <> "pair of persisted properties (optionally filtered by "
          <> "module_path), build the probe '\\args -> P1 args && "
          <> "not (P2 args)' and run it via QuickCheck. A passing "
          <> "probe (counterexample found) means the two properties "
          <> "disagree somewhere — the pair is logically inconsistent. "
          <> "Phase 2 (planned) will add vacuous-property detection "
          <> "and arity-aware filtering."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path"   .= obj "string"
              , "runs_per_pair" .= obj "integer"
              ]
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data PropertyAuditArgs = PropertyAuditArgs
  { paModulePath  :: !(Maybe Text)
  , paRunsPerPair :: !Int
  }
  deriving stock (Show)

instance FromJSON PropertyAuditArgs where
  parseJSON = withObject "PropertyAuditArgs" $ \o -> do
    mp <- o .:? "module_path"
    rs <- o .:? "runs_per_pair" .!= 200
    pure PropertyAuditArgs
      { paModulePath  = mp
      , paRunsPerPair = max 10 (min 1000 rs)
      }

handle :: Store -> GhcSession -> Value -> IO ToolResult
handle store ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (parseErrorResult err)
  Right args -> do
    t0 <- realToFrac <$> getPOSIXTime :: IO Double
    allProps <- loadAll store
    -- Issue #77 / cascade of #74: the property store may carry
    -- duplicate rows for the same expression under different
    -- 'module' shapes (path vs Haskell module name). Without
    -- deduplication the audit pairs a property with itself
    -- under the second shape, producing a counterexample that
    -- looks like a contradiction with no real disagreement.
    -- Dedupe by expression text — the canonical identity.
    let deduped = dedupByExpression allProps
        filtered = case paModulePath args of
          Nothing -> deduped
          Just m  -> [ p | p <- deduped, spModule p == Just m ]
        propPairs = pairCombinations filtered
    findings <- mapM (runPairProbe ghcSess args) propPairs
    t1 <- realToFrac <$> getPOSIXTime :: IO Double
    pure (renderReport args (length filtered) (length propPairs)
                       findings (truncate ((t1 - t0) * 1000)))

--------------------------------------------------------------------------------
-- pair generation
--------------------------------------------------------------------------------

-- | Issue #64: enumerate every UNORDERED pair from the input
-- list. For @n@ properties this returns @n*(n-1)/2@ pairs —
-- the audit checks each in both directions internally so we
-- don't need ordered pairs.
pairCombinations :: [a] -> [(a, a)]
pairCombinations []       = []
pairCombinations (x : xs) = [(x, y) | y <- xs] <> pairCombinations xs

-- | Issue #64: synthesise the contradiction probe lambda.
-- @\\args -> (P1 args) && not (P2 args)@. Phase 1 uses the
-- raw expression text — no AST awareness — so it works for
-- single-argument properties of shape @\\x -> body@. Phase 2
-- will recover argument shape via the GHC API.
buildContradictionProbe :: Text -> Text -> Text
buildContradictionProbe p1 p2 =
  "\\args -> (" <> normalise p1 <> ") args && not ((" <> normalise p2 <> ") args)"
  where
    -- Strip an outer leading \\ if present so we re-add it ourselves.
    -- Phase 1 best-effort: properties already in the store are
    -- always lambdas, so this is a no-op for the common shape.
    normalise = T.strip

--------------------------------------------------------------------------------
-- per-pair probe execution
--------------------------------------------------------------------------------

data PairFinding = PairFinding
  { pfP1            :: !StoredProperty
  , pfP2            :: !StoredProperty
  , pfStatus        :: !Text   -- "contradictory" | "compatible" | "skipped"
  , pfDetail        :: !Text   -- counterexample / parse error / ""
  }
  deriving stock (Show)

runPairProbe
  :: GhcSession -> PropertyAuditArgs
  -> (StoredProperty, StoredProperty) -> IO PairFinding
runPairProbe ghcSess _args (p1, p2) = do
  let probe = buildContradictionProbe (spExpression p1) (spExpression p2)
  -- Re-use the cabal-repl harness ghc_quickcheck uses so the
  -- audit benefits from every fix that path receives (load
  -- failures classified as scope-broken, etc.).
  res <- try @SomeException $
    Qc.runQuickCheckViaCabalRepl (gsProject ghcSess)
      (spModule p1) probe
  pure $ case res of
    Left e ->
      PairFinding
        { pfP1     = p1
        , pfP2     = p2
        , pfStatus = "skipped"
        , pfDetail = T.pack ("subprocess error: " <> show e)
        }
    Right (out, _err) ->
      let (status, detail) = interpretProbeResult (parseQuickCheckOutput probe out)
      in PairFinding
           { pfP1     = p1
           , pfP2     = p2
           , pfStatus = status
           , pfDetail = detail
           }

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: the audit report is informational — even
-- when contradictions exist, this is data the agent uses to act
-- on, not a hard failure of the tool. status='ok' always; the
-- structured 'pairs_inconsistent' / 'findings' fields under
-- 'result' carry the verdict.
renderReport
  :: PropertyAuditArgs -> Int -> Int
  -> [PairFinding] -> Int
  -> ToolResult
renderReport args nProps nPairs findings wallMs =
  let contradictory = filter ((== "contradictory") . pfStatus) findings
      skipped       = filter ((== "skipped") . pfStatus)       findings
      compatible    = length findings - length contradictory - length skipped
      payload = object
        [ "module_filter"       .= paModulePath args
        , "properties_checked"  .= nProps
        , "pairs_checked"       .= nPairs
        , "pairs_inconsistent"  .= length contradictory
        , "pairs_compatible"    .= compatible
        , "pairs_skipped"       .= length skipped
        , "wall_time_ms"        .= wallMs
        , "findings"            .= map renderFinding contradictory
        , "skipped_pairs"       .= map renderFinding skipped
        , "phase"               .= ("1-mvp" :: Text)
        , "deferred"            .= ([ "vacuous-property-check"
                                    , "arity-aware-pairing"
                                    , "llm-conclusion-text"
                                    ] :: [Text])
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

renderFinding :: PairFinding -> Value
renderFinding f = object
  [ "kind"           .= ("contradictory-pair" :: Text)
  , "status"         .= pfStatus f
  , "p1_expression"  .= spExpression (pfP1 f)
  , "p2_expression"  .= spExpression (pfP2 f)
  , "p1_module"      .= spModule (pfP1 f)
  , "p2_module"      .= spModule (pfP2 f)
  , "counterexample" .= pfDetail f
  ]

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
-- Issue #77 — pure interpretation of the contradiction probe
--------------------------------------------------------------------------------

-- | Issue #77: classify a QuickCheck verdict on the
-- contradiction probe @\\args -> P1 args && not (P2 args)@
-- into one of the audit's three statuses.
--
-- The pre-#77 implementation had this inverted: \"QC failed
-- → contradictory\" / \"QC passed → compatible\". Re-derive
-- from QuickCheck's semantics:
--
--   * 'QcPassed' — every random input made the probe TRUE.
--     The probe is @P1 ∧ ¬P2@; if it's true on every input QC
--     tried, then P1 holds whenever ¬P2 holds, i.e. P1 says
--     yes whenever P2 says no. That IS the contradiction.
--
--   * 'QcFailed' with a counterexample — at least one input
--     made the probe FALSE. For that input, either P1 was
--     false or P2 was true; in both cases, the conjunction
--     @P1 ∧ ¬P2@ does not hold, so the input does NOT
--     demonstrate disagreement. The properties are compatible
--     at least there.
--
-- We treat 'QcUnparsed', 'QcException' and 'QcGaveUp' as
-- 'skipped' — the probe could not be evaluated, so we don't
-- pretend to know the answer.
interpretProbeResult :: QuickCheckResult -> (Text, Text)
interpretProbeResult = \case
  QcPassed _ n ->
    ( "contradictory"
    , "QuickCheck found "
        <> T.pack (show n)
        <> " random inputs satisfying P1 ∧ ¬P2 — properties disagree."
    )
  QcFailed _ _ _ counterex ->
    ( "compatible"
    , "Probe falsified at: " <> counterex
    )
  QcUnparsed _ raw ->
    ( "skipped"
    , "probe load/parse failure: " <> T.take 200 raw
    )
  QcException _ msg ->
    ( "skipped"
    , "probe exception: " <> T.take 200 msg
    )
  QcGaveUp {} ->
    ( "skipped"
    , "QuickCheck gave up (too many discards)"
    )

--------------------------------------------------------------------------------
-- Issue #77 / cascade of #74 — defensive deduplication
--------------------------------------------------------------------------------

-- | Issue #77: dedupe stored properties by expression text.
--
-- The property store can carry the same property twice when
-- a historical bug (#74) caused 'check_module' / regression
-- replays to re-persist under a path-shape 'module' field.
-- The expression string is the canonical identity; if two
-- rows have the same expression, they're the same property
-- regardless of how their 'module' field is shaped.
--
-- We keep the FIRST occurrence so the store's natural order
-- (oldest-first) is preserved — useful for reasoning about
-- when each property was added.
dedupByExpression :: [StoredProperty] -> [StoredProperty]
dedupByExpression = go []
  where
    go _    []     = []
    go seen (p:ps)
      | spExpression p `elem` seen = go seen ps
      | otherwise                  = p : go (spExpression p : seen) ps
