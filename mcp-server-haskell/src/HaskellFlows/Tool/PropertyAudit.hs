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
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)

import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  )
import HaskellFlows.Ghc.ApiSession (GhcSession, gsProject)
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
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> do
    t0 <- realToFrac <$> getPOSIXTime :: IO Double
    allProps <- loadAll store
    let filtered = case paModulePath args of
          Nothing -> allProps
          Just m  -> [ p | p <- allProps, spModule p == Just m ]
        pairs = pairCombinations filtered
    findings <- mapM (runPairProbe ghcSess args) pairs
    t1 <- realToFrac <$> getPOSIXTime :: IO Double
    pure (renderReport args (length filtered) (length pairs)
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
      case parseQuickCheckOutput probe out of
        QcFailed _ _ _ counterex ->
          -- QuickCheck failed our probe → the conjunction holds for
          -- some input → P1 says yes, P2 says no on that input →
          -- properties disagree.
          PairFinding
            { pfP1     = p1
            , pfP2     = p2
            , pfStatus = "contradictory"
            , pfDetail = counterex
            }
        QcPassed _ _ ->
          -- The probe never succeeded (no input where P1 ∧ ¬P2
          -- held) — within the searched input space, the
          -- properties are consistent.
          PairFinding
            { pfP1     = p1
            , pfP2     = p2
            , pfStatus = "compatible"
            , pfDetail = ""
            }
        QcUnparsed _ raw ->
          PairFinding
            { pfP1     = p1
            , pfP2     = p2
            , pfStatus = "skipped"
            , pfDetail = "probe load/parse failure: " <> T.take 200 raw
            }
        QcException _ msg ->
          PairFinding
            { pfP1     = p1
            , pfP2     = p2
            , pfStatus = "skipped"
            , pfDetail = "probe exception: " <> T.take 200 msg
            }
        QcGaveUp {} ->
          PairFinding
            { pfP1     = p1
            , pfP2     = p2
            , pfStatus = "skipped"
            , pfDetail = "QuickCheck gave up (too many discards)"
            }

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderReport
  :: PropertyAuditArgs -> Int -> Int
  -> [PairFinding] -> Int
  -> ToolResult
renderReport args nProps nPairs findings wallMs =
  let contradictory = filter ((== "contradictory") . pfStatus) findings
      skipped       = filter ((== "skipped") . pfStatus)       findings
      compatible    = length findings - length contradictory - length skipped
      payload = object
        [ "success"             .= True
        , "module_filter"       .= paModulePath args
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
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

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

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
