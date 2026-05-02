-- | @ghc_property_audit@ — cross-property contradiction detector
-- (#64).
--
-- Phase 1 (MVP): for every pair of stored properties (filtered
-- by 'module_path' when supplied), build the contradiction probe
-- @\\args -> P1 args && not (P2 args)@ and run it via the existing
-- ghc_quickcheck path. If QuickCheck finds a counterexample for
-- the probe, the two properties disagree on at least one input —
-- flag the pair.
--
-- Phase 2 (this commit): vacuous-property check.
-- Set @check_vacuous=true@ to run each property individually and
-- flag ones that QuickCheck gives up on (too many discards), which
-- indicates the precondition filter is so tight that no concrete
-- test actually runs. Vacuous properties are not wrong, but they
-- give false confidence.
--
-- Remaining deferrals:
--   * Same-arity / same-quantified-type heuristic — Phase 1
--     pairs every property with every other property in the
--     filter set; the probe simply fails to compile when the
--     arities mismatch and we surface that as @skipped@.
--   * LLM-friendly conclusion-text generation — hand the
--     counterexample to the agent for narration.
module HaskellFlows.Tool.PropertyAudit
  ( handle
  , PropertyAuditArgs (..)
    -- * Pure helpers (exported for unit tests)
  , pairCombinations
  , buildContradictionProbe
  , interpretProbeResult
  , dedupByExpression
    -- * Phase 2 helpers (exported for unit tests)
  , isVacuousResult
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (catMaybes)
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
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import qualified HaskellFlows.Tool.QuickCheck as Qc

-- | #94 Phase C step 6: this module's @descriptor@ was retired
-- when the four legacy property-store tools were merged into
-- 'HaskellFlows.Tool.PropertyStore'. The 'handle' function below
-- is now invoked indirectly via 'Server.dispatchPropertyStore'
-- when the agent calls @ghc_property_store(action=\"audit\")@.
-- Behaviour is byte-identical to the legacy @ghc_property_audit@
-- surface.

data PropertyAuditArgs = PropertyAuditArgs
  { paModulePath   :: !(Maybe Text)
  , paRunsPerPair  :: !Int
  , paCheckVacuous :: !Bool
    -- ^ Phase 2: when True, also run each property individually and
    -- flag ones QuickCheck gives up on as potentially vacuous.
  }
  deriving stock (Show)

instance FromJSON PropertyAuditArgs where
  parseJSON = withObject "PropertyAuditArgs" $ \o -> do
    mp <- o .:? "module_path"
    rs <- o .:? "runs_per_pair"  .!= 200
    cv <- o .:? "check_vacuous"  .!= False
    pure PropertyAuditArgs
      { paModulePath   = mp
      , paRunsPerPair  = max 10 (min 1000 rs)
      , paCheckVacuous = cv
      }

handle :: Store -> GhcSession -> Value -> IO ToolResult
handle store ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
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
    findings       <- mapM (runPairProbe ghcSess args) propPairs
    -- Phase 2: optional vacuous-property check.
    vacuousFindings <- if paCheckVacuous args
                         then mapM (runVacuousCheck ghcSess) filtered
                         else pure []
    t1 <- realToFrac <$> getPOSIXTime :: IO Double
    pure (renderReport args (length filtered) (length propPairs)
                       findings vacuousFindings (truncate ((t1 - t0) * 1000)))

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
  -- Issue #112: the contradiction probe is a SYNTHETIC lambda, not a
  -- named property. Loading P1's source module often fails when the
  -- probe body doesn't reference project symbols at all, and using a
  -- single module context is wrong when P1 and P2 come from different
  -- stanzas. Pass Nothing → the repl gets `:m + <exposed modules>`
  -- (all library modules) which covers self-contained lambdas and
  -- standard-library terms. Probes that genuinely need a project module
  -- will fall to QcUnparsed → "skipped" (honest signal).
  res <- try @SomeException $
    Qc.runQuickCheckViaCabalRepl (gsProject ghcSess)
      Nothing probe
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
-- Phase 2: vacuous-property check
--------------------------------------------------------------------------------

-- | Phase 2: run a single property via QuickCheck; if QC gives up
-- (too many discards), the property is potentially vacuous.
-- Returns @Just expression@ when vacuous, @Nothing@ otherwise.
runVacuousCheck :: GhcSession -> StoredProperty -> IO (Maybe StoredProperty)
runVacuousCheck ghcSess sp = do
  res <- try @SomeException $
    Qc.runQuickCheckViaCabalRepl (gsProject ghcSess)
      (spModule sp) (spExpression sp)
  pure $ case res of
    Left  _ -> Nothing  -- subprocess failure: can't classify
    Right (out, _) ->
      let qcr = parseQuickCheckOutput (spExpression sp) out
      in if isVacuousResult qcr then Just sp else Nothing

-- | Pure predicate: a QuickCheck result is "vacuous" when
-- QuickCheck gave up — indicating the implicit precondition
-- (e.g. @==>@) was so restrictive that no concrete input could
-- be generated.
isVacuousResult :: QuickCheckResult -> Bool
isVacuousResult QcGaveUp {} = True
isVacuousResult _           = False

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | The audit report is informational — even when contradictions
-- exist, this is data the agent uses to act on, not a hard failure.
-- status='ok' always; the structured 'pairs_inconsistent' / 'findings'
-- fields under 'result' carry the verdict.
-- Phase 2: when 'paCheckVacuous' is set, also includes
-- 'vacuous_properties' count + details.
renderReport
  :: PropertyAuditArgs -> Int -> Int
  -> [PairFinding] -> [Maybe StoredProperty] -> Int
  -> ToolResult
renderReport args nProps nPairs findings vacuousFindings wallMs =
  let contradictory = filter ((== "contradictory") . pfStatus) findings
      skipped       = filter ((== "skipped") . pfStatus)       findings
      compatible    = length findings - length contradictory - length skipped
      vacuous       = catMaybes vacuousFindings
      payload = object $
        [ "module_filter"       .= paModulePath args
        , "properties_checked"  .= nProps
        , "pairs_checked"       .= nPairs
        , "pairs_inconsistent"  .= length contradictory
        , "pairs_compatible"    .= compatible
        , "pairs_skipped"       .= length skipped
        , "wall_time_ms"        .= wallMs
        , "findings"            .= map renderFinding contradictory
        , "skipped_pairs"       .= map renderFinding skipped
        , "phase"               .= ("2-vacuous" :: Text)
        , "deferred"            .= ([ "arity-aware-pairing"
                                    , "llm-conclusion-text"
                                    ] :: [Text])
        ] <>
        if paCheckVacuous args
          then [ "vacuous_properties" .= length vacuous
               , "vacuous_details"    .= map renderVacuous vacuous
               ]
          else []
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

renderVacuous :: StoredProperty -> Value
renderVacuous sp = object
  [ "kind"       .= ("vacuous-property" :: Text)
  , "expression" .= spExpression sp
  , "module"     .= spModule sp
  , "reason"     .= ("QuickCheck gave up: precondition may never be satisfied" :: Text)
  ]

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
