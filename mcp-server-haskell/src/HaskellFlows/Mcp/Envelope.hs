-- | Unified response envelope for every haskell-flows-mcp tool.
--
-- This module is the contract layer described in issue #90. It
-- replaces the per-tool ad-hoc @{success: bool, …}@ shape with a
-- single normative envelope:
--
-- > { status:   ok | partial | no_match | refused | failed | timeout | unavailable
-- > , result:   <tool-specific payload, present iff status ∈ {ok, partial, no_match}>
-- > , error:    <ErrorEnvelope, present iff status ∈ {refused, failed, timeout, unavailable}>
-- > , warnings: [Warning]
-- > , nextStep: NextStep
-- > , meta:     { tool, version, durationMs, trace_id }
-- > , success:  bool   -- DEPRECATED, derived from status; kept for one minor release
-- > }
--
-- Phase A landing (the file you're reading) is purely additive:
-- nothing in the existing codebase consumes 'ToolResponse' yet.
-- Tools migrate one batch at a time per the §5 plan in #90, and the
-- legacy 'HaskellFlows.Mcp.Protocol.ToolResult' shape continues to
-- serve as the wire wrapper around our serialised JSON.
--
-- == Invariants enforced by construction
--
-- * Every smart constructor produces a 'ToolResponse' whose
--   @status@ is consistent with its @result@\/@error@ slots: e.g.
--   'mkOk' forces a non-empty 'Value' for the result field;
--   'mkRefused' forces a non-empty 'ErrorEnvelope'. Tools that
--   construct via the smart constructors cannot emit an
--   ill-shaped payload.
--
-- * The 'FromJSON' instance round-trips the same invariant: a
--   payload that says @status: \"ok\"@ but omits @result@ fails
--   parsing. This catches malformed wire input from a misbehaving
--   client (or a future test fuzz).
--
-- * 'ToolStatus' and 'ErrorKind' are closed enums with
--   @deriving (Bounded, Enum)@. Property tests can exhaustively
--   iterate @[minBound .. maxBound]@; adding a new constructor
--   forces every site that pattern-matches to be updated
--   (-Wincomplete-patterns).
module HaskellFlows.Mcp.Envelope
  ( -- * Status discriminant
    ToolStatus (..)
  , statusToText
  , textToStatus
  , isLegacySuccess
    -- * Error envelope
  , ErrorKind (..)
  , errorKindToText
  , textToErrorKind
  , ErrorEnvelope (..)
  , mkErrorEnvelope
    -- * Warnings
  , Warning (..)
  , WarningKind (..)
  , warningKindToText
  , textToWarningKind
    -- * Meta + response
  , Meta (..)
  , ToolResponse (..)
    -- * Smart constructors (the only sanctioned way to build a 'ToolResponse')
  , mkOk
  , mkPartial
  , mkNoMatch
  , mkRefused
  , mkFailed
  , mkTimeout
  , mkUnavailable
    -- * Optional decorators
  , withWarnings
  , withNextStep
  , withMeta
    -- * Wire-wrapper bridge
  , toolResponseToResult
    -- * Cross-tool helpers
  , sanitizeRejection
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , encode
  , object
  , withObject
  , withText
  , (.!=)
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified HaskellFlows.Ghc.Sanitize as San
import HaskellFlows.Mcp.Protocol
  ( ToolContent (..)
  , ToolResult (..)
  )

--------------------------------------------------------------------------------
-- ToolStatus
--------------------------------------------------------------------------------

-- | The seven discriminant values every response carries. Defined and
-- justified in issue #90 §3. Closed enum — adding an eighth value is a
-- breaking protocol change.
data ToolStatus
  = StatusOk
    -- ^ Tool ran, produced the requested artefact, all gates green.
  | StatusPartial
    -- ^ Tool ran, produced the artefact with caveats (warnings,
    -- partial-gate failures). The 'result' is present; the caller
    -- must inspect 'warnings'.
  | StatusNoMatch
    -- ^ Tool ran, looked for the thing the caller asked about,
    -- didn't find it. The 'result' is present and carries
    -- diagnostic context (e.g. @{ name: \"X\", searched: \"…\" }@)
    -- but the substantive payload is empty.
  | StatusRefused
    -- ^ Tool refused for a *policy* reason the caller could have
    -- predicted (path traversal, sentinel poisoning, sanitize-layer
    -- rejection). Reversible by changing the input.
  | StatusFailed
    -- ^ Tool ran, the operation itself failed (compile error,
    -- type error, deps solver could not resolve, refactor verify
    -- rolled back, subprocess returned non-zero, file not found).
  | StatusTimeout
    -- ^ Tool exceeded its inner per-call budget.
  | StatusUnavailable
    -- ^ Tool depends on an external binary that isn't on PATH
    -- (hoogle, fourmolu, …). Distinct from 'StatusFailed' because
    -- rerunning won't help — the environment is the issue.
  deriving stock (Eq, Show, Enum, Bounded)

-- | Wire-format render. Single source of truth for the lower-cased
-- status name; every emitter MUST go through this.
statusToText :: ToolStatus -> Text
statusToText = \case
  StatusOk          -> "ok"
  StatusPartial     -> "partial"
  StatusNoMatch     -> "no_match"
  StatusRefused     -> "refused"
  StatusFailed      -> "failed"
  StatusTimeout     -> "timeout"
  StatusUnavailable -> "unavailable"

-- | Inverse of 'statusToText'. Built from the @[minBound..maxBound]@
-- list so a future constructor automatically participates.
textToStatus :: Text -> Maybe ToolStatus
textToStatus = flip Map.lookup statusReverseMap

statusReverseMap :: Map.Map Text ToolStatus
statusReverseMap =
  Map.fromList [ (statusToText s, s) | s <- [minBound .. maxBound] ]

-- | Derived legacy @success@ field. Present in the wire output
-- during the migration window so old clients keep working.
-- See issue #90 §5 (Phase D) for removal.
isLegacySuccess :: ToolStatus -> Bool
isLegacySuccess StatusOk      = True
isLegacySuccess StatusPartial = True
isLegacySuccess _             = False

instance ToJSON ToolStatus where
  toJSON = String . statusToText

instance FromJSON ToolStatus where
  parseJSON = withText "ToolStatus" $ \t ->
    case textToStatus t of
      Just s  -> pure s
      Nothing -> fail ("unknown ToolStatus: " <> T.unpack t)

--------------------------------------------------------------------------------
-- ErrorKind
--------------------------------------------------------------------------------

-- | Closed enum of every failure mode the MCP can emit. Defined in
-- issue #90 §4 with the per-kind status pairing + emitter table.
--
-- Note: this is intentionally distinct from
-- 'HaskellFlows.Mcp.ErrorKind.ErrorKind' (the legacy 3-value enum
-- used by the current Server.hs / Eval.hs emitters). The two
-- coexist during Phases A–C of the migration; Phase D collapses
-- them.
data ErrorKind
  = -- Caller error (status = failed) -----------------------------------
    MissingArg
  | TypeMismatch
  | Validation
    -- Refused (status = refused) ---------------------------------------
  | PathTraversal
  | NewlineInjection
  | SentinelPoisoning
  | OversizedInput
  | EmptyInput
    -- Compile-time failure (status = failed) ---------------------------
  | CompileError
  | TypeError
    -- Lookup miss (status = no_match) ----------------------------------
  | NotInScope
  | ModuleNotInGraph
    -- Runtime failure (status = failed) --------------------------------
  | ModulePathDoesNotExist
  | UnresolvableDep
  | VerifyFailed
  | SolverConflict
  | SubprocessError
  | InternalError
    -- Environment (status = unavailable) -------------------------------
  | HpcUnavailable
  | BinaryUnavailable
    -- Time (status = timeout) ------------------------------------------
  | InnerTimeout
  | OuterTimeout
    -- Session (status = failed) ----------------------------------------
  | SessionExhausted
  deriving stock (Eq, Show, Enum, Bounded)

-- | Wire-format render for every 'ErrorKind'. Single source of truth.
errorKindToText :: ErrorKind -> Text
errorKindToText = \case
  MissingArg              -> "missing_arg"
  TypeMismatch            -> "type_mismatch"
  Validation              -> "validation"
  PathTraversal           -> "path_traversal"
  NewlineInjection        -> "newline_injection"
  SentinelPoisoning       -> "sentinel_poisoning"
  OversizedInput          -> "oversized_input"
  EmptyInput              -> "empty_input"
  CompileError            -> "compile_error"
  TypeError               -> "type_error"
  NotInScope              -> "not_in_scope"
  ModuleNotInGraph        -> "module_not_in_graph"
  ModulePathDoesNotExist  -> "module_path_does_not_exist"
  UnresolvableDep         -> "unresolvable_dep"
  VerifyFailed            -> "verify_failed"
  SolverConflict          -> "solver_conflict"
  SubprocessError         -> "subprocess_error"
  InternalError           -> "internal_error"
  HpcUnavailable          -> "hpc_unavailable"
  BinaryUnavailable       -> "binary_unavailable"
  InnerTimeout            -> "inner_timeout"
  OuterTimeout            -> "outer_timeout"
  SessionExhausted        -> "session_exhausted"

-- | Inverse of 'errorKindToText'.
textToErrorKind :: Text -> Maybe ErrorKind
textToErrorKind = flip Map.lookup errorKindReverseMap

errorKindReverseMap :: Map.Map Text ErrorKind
errorKindReverseMap =
  Map.fromList [ (errorKindToText k, k) | k <- [minBound .. maxBound] ]

instance ToJSON ErrorKind where
  toJSON = String . errorKindToText

instance FromJSON ErrorKind where
  parseJSON = withText "ErrorKind" $ \t ->
    case textToErrorKind t of
      Just k  -> pure k
      Nothing -> fail ("unknown ErrorKind: " <> T.unpack t)

--------------------------------------------------------------------------------
-- ErrorEnvelope
--------------------------------------------------------------------------------

-- | Diagnostic envelope. Present on every response with @status ∈
-- {refused, failed, timeout, unavailable}@. Fields beyond
-- @kind@ + @message@ are optional; they're populated when they
-- carry signal.
--
-- Field semantics — see issue #90 §2:
--
-- * 'eeField' — for @missing_arg@ / @type_mismatch@: which JSON-RPC
--   argument failed.
-- * 'eeExpected' \/ 'eeGot' — for @type_mismatch@: human-readable
--   wire-form discrepancy.
-- * 'eeHint' — one-line user-facing suggestion.
-- * 'eeRemediation' — longer actionable steps.
-- * 'eeSchemaRef' — URI back to the @tools/list@ schema for this tool.
-- * 'eeCause' — raw underlying error (Aeson, GHC API, subprocess) for
--   debugging. Deliberately separate from 'eeMessage' so user-facing
--   text isn't poisoned by parser internals.
data ErrorEnvelope = ErrorEnvelope
  { eeKind        :: !ErrorKind
  , eeMessage     :: !Text
  , eeField       :: !(Maybe Text)
  , eeExpected    :: !(Maybe Text)
  , eeGot         :: !(Maybe Text)
  , eeHint        :: !(Maybe Text)
  , eeRemediation :: !(Maybe Text)
  , eeSchemaRef   :: !(Maybe Text)
  , eeCause       :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | Build a minimal 'ErrorEnvelope' with only @kind@ + @message@
-- populated. Optional fields can be set positionally on the result
-- record (record-update syntax) when they're known.
mkErrorEnvelope :: ErrorKind -> Text -> ErrorEnvelope
mkErrorEnvelope k msg = ErrorEnvelope
  { eeKind        = k
  , eeMessage     = msg
  , eeField       = Nothing
  , eeExpected    = Nothing
  , eeGot         = Nothing
  , eeHint        = Nothing
  , eeRemediation = Nothing
  , eeSchemaRef   = Nothing
  , eeCause       = Nothing
  }

instance ToJSON ErrorEnvelope where
  toJSON ee = object $ catMaybes
    [ Just ("kind"     .= eeKind ee)
    , Just ("message"  .= eeMessage ee)
    , optField "field"        (eeField ee)
    , optField "expected"     (eeExpected ee)
    , optField "got"          (eeGot ee)
    , optField "hint"         (eeHint ee)
    , optField "remediation"  (eeRemediation ee)
    , optField "schema_ref"   (eeSchemaRef ee)
    , optField "cause"        (eeCause ee)
    ]
    where
      optField k = fmap (k .=)

instance FromJSON ErrorEnvelope where
  parseJSON = withObject "ErrorEnvelope" $ \o ->
    ErrorEnvelope
      <$> o .:  "kind"
      <*> o .:  "message"
      <*> o .:? "field"
      <*> o .:? "expected"
      <*> o .:? "got"
      <*> o .:? "hint"
      <*> o .:? "remediation"
      <*> o .:? "schema_ref"
      <*> o .:? "cause"

--------------------------------------------------------------------------------
-- Warning
--------------------------------------------------------------------------------

-- | Non-fatal observation that travels alongside a successful or
-- partial response. Examples: a deprecated argument, a slow code
-- path, a low-confidence suggestion.
data Warning = Warning
  { wKind    :: !WarningKind
  , wMessage :: !Text
  , wExtra   :: !(Maybe Value)
  }
  deriving stock (Eq, Show)

-- | Closed enum of known warning categories. Like 'ErrorKind', adding
-- a constructor is a wire-format change.
data WarningKind
  = DeprecatedField
  | DeprecatedTool
  | LowConfidence
  | SlowPath
  | RecoveredAfterRetry
  | OtherWarning
  deriving stock (Eq, Show, Enum, Bounded)

warningKindToText :: WarningKind -> Text
warningKindToText = \case
  DeprecatedField     -> "deprecated_field"
  DeprecatedTool      -> "deprecated_tool"
  LowConfidence       -> "low_confidence"
  SlowPath            -> "slow_path"
  RecoveredAfterRetry -> "recovered_after_retry"
  OtherWarning        -> "other"

textToWarningKind :: Text -> Maybe WarningKind
textToWarningKind = flip Map.lookup warningKindReverseMap

warningKindReverseMap :: Map.Map Text WarningKind
warningKindReverseMap =
  Map.fromList [ (warningKindToText w, w) | w <- [minBound .. maxBound] ]

instance ToJSON WarningKind where
  toJSON = String . warningKindToText

instance FromJSON WarningKind where
  parseJSON = withText "WarningKind" $ \t ->
    case textToWarningKind t of
      Just w  -> pure w
      Nothing -> fail ("unknown WarningKind: " <> T.unpack t)

instance ToJSON Warning where
  toJSON w = object $ catMaybes
    [ Just ("kind"    .= wKind w)
    , Just ("message" .= wMessage w)
    , ("extra" .=) <$> wExtra w
    ]

instance FromJSON Warning where
  parseJSON = withObject "Warning" $ \o ->
    Warning
      <$> o .:  "kind"
      <*> o .:  "message"
      <*> o .:? "extra"

--------------------------------------------------------------------------------
-- Meta
--------------------------------------------------------------------------------

-- | Per-call instrumentation. Optional in every response; tools that
-- have measured their wall-clock and know their own version fill it
-- in. Trace correlation rides 'metaTraceId' (see issue #98).
data Meta = Meta
  { metaTool       :: !Text
  , metaVersion    :: !Text
  , metaDurationMs :: !Int
  , metaTraceId    :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance ToJSON Meta where
  toJSON m = object $ catMaybes
    [ Just ("tool"       .= metaTool m)
    , Just ("version"    .= metaVersion m)
    , Just ("durationMs" .= metaDurationMs m)
    , optField "trace_id" (metaTraceId m)
    ]
    where
      optField k = fmap (k .=)

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \o ->
    Meta
      <$> o .:  "tool"
      <*> o .:  "version"
      <*> o .:  "durationMs"
      <*> o .:? "trace_id"

--------------------------------------------------------------------------------
-- ToolResponse — the top-level envelope
--------------------------------------------------------------------------------

-- | The envelope every haskell-flows-mcp tool returns from its
-- handler (post-migration). The legacy 'success' field is derived
-- from 'reStatus' on encode and is dropped at the end of Phase D.
--
-- Smart constructors ('mkOk' \/ 'mkPartial' \/ …) are the only
-- sanctioned construction path. Direct record construction is
-- exposed for tests + programmatic decoders only.
data ToolResponse = ToolResponse
  { reStatus   :: !ToolStatus
  , reResult   :: !(Maybe Value)
    -- ^ Tool-specific payload. Present iff
    -- @reStatus ∈ {ok, partial, no_match}@.
  , reError    :: !(Maybe ErrorEnvelope)
    -- ^ Diagnostic. Present iff
    -- @reStatus ∈ {refused, failed, timeout, unavailable}@.
  , reWarnings :: ![Warning]
  , reNextStep :: !(Maybe Value)
    -- ^ Existing 'HaskellFlows.Mcp.NextStep' payload. Kept as a
    -- 'Value' here to avoid a cyclic dependency; the consumer
    -- decodes it back into the structured type.
  , reMeta     :: !(Maybe Meta)
  }
  deriving stock (Eq, Show)

instance ToJSON ToolResponse where
  toJSON r = object $ catMaybes
    [ Just ("status"     .= reStatus r)
    , Just ("success"    .= isLegacySuccess (reStatus r))   -- deprecated, kept during migration
      -- Migration-window companion: surface a top-level
      -- 'error_kind' when an error is present, mirroring the
      -- pre-envelope shape several e2e oracles (especially
      -- 'FlowTimeoutEnforcement') key on. The structured value
      -- still lives under 'error.kind' — this is the
      -- backwards-compat duplicate. Dropped in Phase D along
      -- with 'success'.
    , optField "error_kind" (errorKindToText . eeKind <$> reError r)
    , optField "result"   (reResult r)
    , optField "error"    (reError r)
    , optWarnings (reWarnings r)
    , optField "nextStep" (reNextStep r)
    , optField "meta"     (reMeta r)
    ]
    where
      optField k = fmap (k .=)
      optWarnings ws | null ws   = Nothing
                     | otherwise = Just ("warnings" .= ws)

instance FromJSON ToolResponse where
  parseJSON = withObject "ToolResponse" $ \o -> do
    s   <- o .:  "status"
    res <- o .:? "result"
    err <- o .:? "error"
    ws  <- o .:? "warnings" .!= []
    ns  <- o .:? "nextStep"
    m   <- o .:? "meta"
    let envelope = ToolResponse
          { reStatus   = s
          , reResult   = res
          , reError    = err
          , reWarnings = ws
          , reNextStep = ns
          , reMeta     = m
          }
    enforceShapeInvariant envelope

-- | Validates the §2 invariant from issue #90: result-bearing
-- statuses carry a result, error-bearing statuses carry an error.
-- Run as the last step of FromJSON so a malformed wire payload is
-- rejected at the parser, not at the consumer.
enforceShapeInvariant :: ToolResponse -> Parser ToolResponse
enforceShapeInvariant r =
  case reStatus r of
    StatusOk          -> requireResult r
    StatusPartial     -> requireResult r
    StatusNoMatch     -> requireResult r
    StatusRefused     -> requireError  r
    StatusFailed      -> requireError  r
    StatusTimeout     -> requireError  r
    StatusUnavailable -> requireError  r
  where
    requireResult x = case reResult x of
      Just _  -> pure x
      Nothing -> fail
        ("status " <> T.unpack (statusToText (reStatus x))
         <> " requires a 'result' field")
    requireError x = case reError x of
      Just _  -> pure x
      Nothing -> fail
        ("status " <> T.unpack (statusToText (reStatus x))
         <> " requires an 'error' field")

--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

-- | Tool ran cleanly, all gates green. Result is the canonical
-- payload. By construction, @status = ok@ implies @result = Just _@.
mkOk :: Value -> ToolResponse
mkOk r = (baseResponse StatusOk) { reResult = Just r }

-- | Tool ran with caveats. The caller MUST inspect 'reWarnings' and
-- the per-gate fields inside @result@ before assuming the artefact
-- is fit for purpose.
mkPartial :: Value -> ToolResponse
mkPartial r = (baseResponse StatusPartial) { reResult = Just r }

-- | Tool ran. Looked for the thing the caller asked about. Found
-- nothing. The @result@ carries diagnostic context (e.g. @{ name,
-- searched, candidates_considered }@) so an agent can disambiguate
-- *\"the question was well-formed and the answer is the empty
-- set\"* from *\"the question itself was malformed\"* (which would
-- be 'mkFailed').
mkNoMatch :: Value -> ToolResponse
mkNoMatch r = (baseResponse StatusNoMatch) { reResult = Just r }

-- | Tool refused for a *policy* reason the caller could have
-- predicted. The error MUST classify the refusal via
-- 'eeKind ∈ {PathTraversal, NewlineInjection, SentinelPoisoning,
-- OversizedInput, EmptyInput, …}'.
mkRefused :: ErrorEnvelope -> ToolResponse
mkRefused e = (baseResponse StatusRefused) { reError = Just e }

-- | Tool ran. The operation itself failed.
mkFailed :: ErrorEnvelope -> ToolResponse
mkFailed e = (baseResponse StatusFailed) { reError = Just e }

-- | Tool exceeded its inner per-call budget.
mkTimeout :: ErrorEnvelope -> ToolResponse
mkTimeout e = (baseResponse StatusTimeout) { reError = Just e }

-- | Tool depends on an environment binary that's not on PATH.
-- @eeRemediation@ should always be populated with the install
-- command.
mkUnavailable :: ErrorEnvelope -> ToolResponse
mkUnavailable e = (baseResponse StatusUnavailable) { reError = Just e }

baseResponse :: ToolStatus -> ToolResponse
baseResponse s = ToolResponse
  { reStatus   = s
  , reResult   = Nothing
  , reError    = Nothing
  , reWarnings = []
  , reNextStep = Nothing
  , reMeta     = Nothing
  }

--------------------------------------------------------------------------------
-- Optional decorators
--------------------------------------------------------------------------------

-- | Attach warnings to a response. Preserves any warnings already
-- present (appends to the right).
withWarnings :: [Warning] -> ToolResponse -> ToolResponse
withWarnings ws r = r { reWarnings = reWarnings r <> ws }

-- | Attach a 'NextStep' payload (kept as a 'Value' to avoid a
-- module-level dependency cycle).
withNextStep :: Value -> ToolResponse -> ToolResponse
withNextStep ns r = r { reNextStep = Just ns }

-- | Attach instrumentation metadata.
withMeta :: Meta -> ToolResponse -> ToolResponse
withMeta m r = r { reMeta = Just m }

--------------------------------------------------------------------------------
-- Wire-wrapper bridge
--------------------------------------------------------------------------------

-- | Convert a 'ToolResponse' into the MCP-protocol 'ToolResult'
-- wrapper (the @{content: [...], isError: bool}@ JSON-RPC payload).
--
-- The bridge encodes the envelope as a JSON string, packs it into a
-- single 'TextContent' block, and derives @isError@ from
-- 'isLegacySuccess' so existing JSON-RPC clients that key on the
-- transport-level error flag keep working unchanged through the
-- migration window.
--
-- Migrating a tool's response handling is therefore a 3-step move:
--
-- 1. Build a 'ToolResponse' via the smart constructors.
-- 2. Apply any decorators ('withWarnings', 'withNextStep', 'withMeta').
-- 3. Hand it to 'toolResponseToResult' at the very last step before
--    returning from the handler.
toolResponseToResult :: ToolResponse -> ToolResult
toolResponseToResult r =
  let body = TL.toStrict (TLE.decodeUtf8 (encode r))
  in ToolResult
       { trContent = [ TextContent body ]
       , trIsError = not (isLegacySuccess (reStatus r))
       }

--------------------------------------------------------------------------------
-- Cross-tool helpers
--------------------------------------------------------------------------------

-- | Translate a sanitize-layer 'CommandError' into the envelope's
-- 'StatusRefused' error shape. Newline / sentinel / oversize /
-- empty inputs are *policy* refusals — the agent could have
-- predicted them by inspecting the input itself, so they're
-- distinct from runtime failures (compile errors, exceptions).
--
-- Every tool that routes a user input through 'sanitizeExpression'
-- (ghc_eval, ghc_quickcheck, ghc_complete, ghc_goto, ghc_info,
-- ghc_doc, ghc_type, …) shares this mapping. The 'fieldName'
-- argument is the JSON-RPC field that carried the offending input
-- — so the consumer can pinpoint *which* argument tripped the
-- policy without parsing the message string.
sanitizeRejection :: Text -> San.CommandError -> ErrorEnvelope
sanitizeRejection fieldName = \case
  San.ContainsNewline ->
    (mkErrorEnvelope NewlineInjection
       (fieldName <> " must be a single line (no newline characters)"))
         { eeField = Just fieldName }
  San.ContainsSentinel ->
    (mkErrorEnvelope SentinelPoisoning
       (fieldName <> " contains the internal framing sentinel and was rejected"))
         { eeField = Just fieldName }
  San.EmptyInput ->
    (mkErrorEnvelope EmptyInput (fieldName <> " is empty"))
      { eeField = Just fieldName }
  San.InputTooLarge sz cap ->
    (mkErrorEnvelope OversizedInput
       (fieldName <> " is too large (" <> T.pack (show sz) <> " chars, cap is "
        <> T.pack (show cap) <> ")"))
      { eeField = Just fieldName
      , eeCause = Just (T.pack (show sz) <> "/" <> T.pack (show cap))
      }
