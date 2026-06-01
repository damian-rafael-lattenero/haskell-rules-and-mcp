-- | Unit tests for the 'HaskellFlows.Mcp.Envelope' contract (#90 Phase A):
-- ToolStatus, ErrorKind, WarningKind round-trips; smart-constructor
-- invariants; FromJSON rejection of malformed payloads; and QuickCheck
-- totality properties for the three enum types. Also covers Phase D
-- legacy-field removal guards.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export
-- shape: the driver keeps the registrations and imports these functions.
module Spec.Envelope
  ( testEnvelopeStatusRoundTrip
  , testEnvelopeErrorKindRoundTrip
  , testEnvelopeWarningKindRoundTrip
  , testEnvelopeMkOk
  , testEnvelopeMkRefused
  , testEnvelopeFromJSONRequiresResult
  , testEnvelopeFromJSONRequiresError
  , testEnvelopeRoundTrip
  , testEnvelopeErrorOptionalFields
  , testEnvelopeWarningsOmittedEmpty
  , prop_envelopeStatusTotal
  , prop_envelopeErrorKindTotal
  , prop_envelopeWarningKindTotal
  -- legacy-field guards (Phase D)
  , testEnvelopeLegacySuccessDropped
  , testEnvelopeLegacyErrorKindDropped
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Test.QuickCheck as QC

import qualified HaskellFlows.Mcp.Envelope as Env

-- | Local pure key-lookup helper (defined at module level so the
-- legacy-field tests can share it).
lookupField :: Text -> A.Value -> Maybe A.Value
lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
lookupField _ _            = Nothing

-- ---------------------------------------------------------------------------
-- ToolStatus round-trip
-- ---------------------------------------------------------------------------

-- | Every 'ToolStatus' encodes to its documented lowercase wire form
-- and decodes back. Anchors the wire string against accidental
-- rename in 'statusToText'. Iterates @[minBound..maxBound]@ so a
-- future eighth status fails compilation, not at runtime.
testEnvelopeStatusRoundTrip :: IO Bool
testEnvelopeStatusRoundTrip =
  let allStatuses = [minBound .. maxBound] :: [Env.ToolStatus]
      expected =
        [ (Env.StatusOk,          "ok")
        , (Env.StatusPartial,     "partial")
        , (Env.StatusNoMatch,     "no_match")
        , (Env.StatusRefused,     "refused")
        , (Env.StatusFailed,      "failed")
        , (Env.StatusTimeout,     "timeout")
        , (Env.StatusUnavailable, "unavailable")
        ]
      wireFormCorrect = all (\(s, t) -> Env.statusToText s == t) expected
      reverseTotal    = all (\s -> Env.textToStatus (Env.statusToText s) == Just s) allStatuses
      jsonRound s     = case A.fromJSON (A.toJSON s) of
                          A.Success s' -> s' == s
                          _            -> False
      jsonAllOk       = all jsonRound allStatuses
  in pure (wireFormCorrect && reverseTotal && jsonAllOk)

-- ---------------------------------------------------------------------------
-- ErrorKind round-trip
-- ---------------------------------------------------------------------------

-- | 'ErrorKind' has 28 documented wire-form strings (issue #90 §4).
-- Spot-check a representative subset against the documented strings,
-- plus assert the round-trip works for the full enum.
testEnvelopeErrorKindRoundTrip :: IO Bool
testEnvelopeErrorKindRoundTrip =
  let allKinds = [minBound .. maxBound] :: [Env.ErrorKind]
      pinned =
        [ (Env.MissingArg,             "missing_arg")
        , (Env.TypeMismatch,           "type_mismatch")
        , (Env.PathTraversal,          "path_traversal")
        , (Env.NewlineInjection,       "newline_injection")
        , (Env.SentinelPoisoning,      "sentinel_poisoning")
        , (Env.OversizedInput,         "oversized_input")
        , (Env.NotInScope,             "not_in_scope")
        , (Env.ModuleNotInGraph,       "module_not_in_graph")
        , (Env.ModulePathDoesNotExist, "module_path_does_not_exist")
        , (Env.OutsideSourceDirs,      "outside_source_dirs")
        , (Env.UnresolvableDep,        "unresolvable_dep")
        , (Env.VerifyFailed,           "verify_failed")
        , (Env.InnerTimeout,           "inner_timeout")
        , (Env.OuterTimeout,           "outer_timeout")
        , (Env.SessionExhausted,       "session_exhausted")
        , (Env.BinaryUnavailable,      "binary_unavailable")
        , (Env.HandWrittenFileGuard,   "hand_written_file_guard")
        , (Env.Regression,             "regression")
        ]
      pinnedOk = all (\(k, t) -> Env.errorKindToText k == t) pinned
      reverseTotal = all (\k -> Env.textToErrorKind (Env.errorKindToText k) == Just k)
                         allKinds
      countOk = length allKinds == 28
  in pure (pinnedOk && reverseTotal && countOk)

-- ---------------------------------------------------------------------------
-- WarningKind round-trip
-- ---------------------------------------------------------------------------

-- | Companion round-trip for 'WarningKind'.
testEnvelopeWarningKindRoundTrip :: IO Bool
testEnvelopeWarningKindRoundTrip =
  let allKinds = [minBound .. maxBound] :: [Env.WarningKind]
      pinned =
        [ (Env.DeprecatedField,     "deprecated_field")
        , (Env.DeprecatedTool,      "deprecated_tool")
        , (Env.LowConfidence,       "low_confidence")
        , (Env.SlowPath,            "slow_path")
        , (Env.RecoveredAfterRetry, "recovered_after_retry")
        , (Env.OtherWarning,        "other")
        ]
      pinnedOk     = all (\(k, t) -> Env.warningKindToText k == t) pinned
      reverseTotal = all (\k -> Env.textToWarningKind (Env.warningKindToText k) == Just k)
                         allKinds
  in pure (pinnedOk && reverseTotal)

-- ---------------------------------------------------------------------------
-- Smart-constructor invariants
-- ---------------------------------------------------------------------------

-- | 'mkOk' produces the canonical happy-path shape: status=ok,
-- result present, error absent. Encodes through Aeson and asserts
-- the wire-form fields.
testEnvelopeMkOk :: IO Bool
testEnvelopeMkOk =
  let payload = A.object [ "answer" A..= (42 :: Int) ]
      response = Env.mkOk payload
      encoded = A.toJSON response
      lookupKey k v = case encoded of
        A.Object o -> AKM.lookup (AKey.fromText k) o == Just v
        _          -> False
  in pure
       ( Env.reStatus response == Env.StatusOk
      && Env.reResult response == Just payload
      && isNothing (Env.reError response)
      && lookupKey "status"  (A.String "ok")
      && lookupKey "result"  payload
       )

-- | 'mkRefused' produces the canonical refusal shape: status=refused,
-- error present, result absent.
testEnvelopeMkRefused :: IO Bool
testEnvelopeMkRefused =
  let err      = Env.mkErrorEnvelope Env.PathTraversal "target path escapes project root"
      response = Env.mkRefused err
      encoded  = A.toJSON response
      lookupKey k v = case encoded of
        A.Object o -> AKM.lookup (AKey.fromText k) o == Just v
        _          -> False
      hasErrorObj = case encoded of
        A.Object o -> case AKM.lookup (AKey.fromText "error") o of
          Just (A.Object eo) ->
            AKM.lookup (AKey.fromText "kind")    eo == Just (A.String "path_traversal")
              && AKM.lookup (AKey.fromText "message") eo == Just (A.String "target path escapes project root")
          _ -> False
        _ -> False
  in pure
       ( Env.reStatus response == Env.StatusRefused
      && isNothing (Env.reResult response)
      && lookupKey "status"  (A.String "refused")
      && hasErrorObj
       )

-- ---------------------------------------------------------------------------
-- FromJSON rejection
-- ---------------------------------------------------------------------------

-- | 'FromJSON' enforces the §2 invariant: a payload that announces
-- @status: ok@ but omits @result@ is malformed and must fail the parser.
testEnvelopeFromJSONRequiresResult :: IO Bool
testEnvelopeFromJSONRequiresResult =
  let bytes = "{\"status\":\"ok\"}"
  in pure $ case A.eitherDecode bytes :: Either String Env.ToolResponse of
       Left err -> "requires" `isInfixOf` err
       Right _  -> False

-- | Inverse: @status: failed@ without @error@ must fail.
testEnvelopeFromJSONRequiresError :: IO Bool
testEnvelopeFromJSONRequiresError =
  let bytes = "{\"status\":\"failed\"}"
  in pure $ case A.eitherDecode bytes :: Either String Env.ToolResponse of
       Left err -> "requires" `isInfixOf` err
       Right _  -> False

-- ---------------------------------------------------------------------------
-- Full round-trip
-- ---------------------------------------------------------------------------

-- | Encode → decode round-trip. Builds a representative response
-- with every optional field populated.
testEnvelopeRoundTrip :: IO Bool
testEnvelopeRoundTrip =
  let payload = A.object [ "type" A..= ("Int -> Int" :: Text) ]
      warning = Env.Warning
                  { Env.wKind    = Env.LowConfidence
                  , Env.wMessage = "result inferred via best-effort"
                  , Env.wExtra   = Just (A.object [ "confidence" A..= ("medium" :: Text) ])
                  }
      meta = Env.Meta
               { Env.metaTool       = "ghc_type"
               , Env.metaVersion    = "0.1.0.0"
               , Env.metaDurationMs = 42
               , Env.metaTraceId    = Just "7f3a2b"
               }
      response =
        Env.withMeta meta
        . Env.withNextStep (A.object [ "tool" A..= ("ghc_quickcheck" :: Text) ])
        . Env.withWarnings [warning]
        $ Env.mkOk payload
      encoded = A.encode response
  in pure $ case A.eitherDecode encoded :: Either String Env.ToolResponse of
       Right decoded
         | decoded == response -> True
       _                       -> False

-- ---------------------------------------------------------------------------
-- Optional-field defaults
-- ---------------------------------------------------------------------------

-- | The optional fields on 'ErrorEnvelope' default to 'Nothing' on
-- decode when omitted.
testEnvelopeErrorOptionalFields :: IO Bool
testEnvelopeErrorOptionalFields =
  let bytes = "{\"kind\":\"missing_arg\",\"message\":\"required field 'expression' is missing\"}"
  in pure $ case A.eitherDecode bytes :: Either String Env.ErrorEnvelope of
       Right ee ->
         Env.eeKind ee == Env.MissingArg
           && Env.eeMessage ee == "required field 'expression' is missing"
           && isNothing (Env.eeField ee)
           && isNothing (Env.eeHint ee)
       Left _ -> False

-- | When a response has no warnings, the @warnings@ field is omitted
-- from the wire output.
testEnvelopeWarningsOmittedEmpty :: IO Bool
testEnvelopeWarningsOmittedEmpty =
  let response = Env.mkOk (A.object [])
      encoded  = A.toJSON response
      hasWarningsKey = case encoded of
        A.Object o -> AKM.member (AKey.fromText "warnings") o
        _          -> False
  in pure (not hasWarningsKey)

-- ---------------------------------------------------------------------------
-- Legacy-field guards (#90 Phase D)
-- ---------------------------------------------------------------------------

-- | Issue #90 Phase D step 2: the legacy top-level @\"success\"@ field
-- is gone. Every consumer reads @\"status\"@ now.
testEnvelopeLegacySuccessDropped :: IO Bool
testEnvelopeLegacySuccessDropped = do
  let okJson     = A.toJSON (Env.mkOk     (A.object [ "k" A..= ("v" :: Text) ]))
      failedJson = A.toJSON (Env.mkFailed
                              (Env.mkErrorEnvelope Env.Validation "boom"))
  pure $ isNothing (lookupField "success" okJson)
      && isNothing (lookupField "success" failedJson)
      && lookupField "status"  okJson      == Just (A.String "ok")
      && lookupField "status"  failedJson  == Just (A.String "failed")

-- | Issue #90 Phase D step 2: the legacy top-level @\"error_kind\"@
-- duplicate is gone. Every consumer reads @\"error.kind\"@ (nested) now.
testEnvelopeLegacyErrorKindDropped :: IO Bool
testEnvelopeLegacyErrorKindDropped = do
  let json = A.toJSON (Env.mkFailed
                        (Env.mkErrorEnvelope Env.PathTraversal "tried to escape"))
      topLevelKind = lookupField "error_kind" json
      nestedKind   = lookupField "error" json
                       >>= lookupField "kind"
  pure $ isNothing topLevelKind
      && nestedKind == Just (A.String "path_traversal")

-- ---------------------------------------------------------------------------
-- QuickCheck totality
-- ---------------------------------------------------------------------------

-- | QC: round-trip totality for 'ToolStatus'.
prop_envelopeStatusTotal :: QC.Property
prop_envelopeStatusTotal = QC.forAll (QC.elements [minBound..maxBound]) $ \s ->
  case A.fromJSON (A.toJSON (s :: Env.ToolStatus)) of
    A.Success s' -> s' QC.=== s
    A.Error e    -> QC.counterexample e (QC.property False)

-- | QC: same totality for 'ErrorKind'. 28 values per #90 §4.
prop_envelopeErrorKindTotal :: QC.Property
prop_envelopeErrorKindTotal = QC.forAll (QC.elements [minBound..maxBound]) $ \k ->
  case A.fromJSON (A.toJSON (k :: Env.ErrorKind)) of
    A.Success k' -> k' QC.=== k
    A.Error e    -> QC.counterexample e (QC.property False)

-- | QC: same totality for 'WarningKind'.
prop_envelopeWarningKindTotal :: QC.Property
prop_envelopeWarningKindTotal = QC.forAll (QC.elements [minBound..maxBound]) $ \w ->
  case A.fromJSON (A.toJSON (w :: Env.WarningKind)) of
    A.Success w' -> w' QC.=== w
    A.Error e    -> QC.counterexample e (QC.property False)
