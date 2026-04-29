-- | @ghc_eval@ — Wave-5 full in-process.
--
-- Evaluates a Haskell expression. Tries the fast path first: wrap in
-- @show@, 'compileExpr', @unsafeCoerce@ to 'String'. If that fails
-- (expression is of type @IO a@, @IO String@ in particular, or the
-- show-wrap doesn't typecheck), re-try as an @IO String@-typed
-- statement via 'evalIOString'. Both paths are in-process; the
-- legacy subprocess ghci is gone.
--
-- Boundary safety: 'sanitizeExpression' rejects the
-- newline/sentinel/empty/too-large inputs before any compile.
module HaskellFlows.Tool.Eval
  ( descriptor
  , handle
  , EvalArgs (..)
  ) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , throwIO
  , try
  )
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import GHC
  ( Ghc
  , InteractiveImport (IIDecl)
  , getContext
  , mkModuleName
  , setContext
  , simpleImportDecl
  )
import GHC.Runtime.Eval (compileExpr)
import System.Timeout (timeout)
import Unsafe.Coerce (unsafeCoerce)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , evalIOString
  , firstLibraryOrTestSuite
  , loadForTarget
  , resetHscEnvInPlace
  , withGhcSession
  , withStanzaFlags
  )
import HaskellFlows.Ghc.Sanitize
  ( maxEvalBytes
  , sanitizeExpression
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcEval
    , tdDescription =
        "Evaluate a Haskell expression in-process via the GHC API. "
          <> "Tries @show@-wrapped compileExpr first (for pure expressions), "
          <> "falls back to an IO String interpretation (for actions that "
          <> "already return a string, or expressions in the IO monad). "
          <> "Output capped at "
          <> T.pack (show maxEvalBytes) <> " characters."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Expression to evaluate. Examples: \"1 + 2\", \
                       \\"map (+1) [1..5]\", \"fmap show Nothing\"" :: Text)
                  ]
              ]
          , "required"             .= ["expression" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype EvalArgs = EvalArgs
  { eaExpression :: Text
  }
  deriving stock (Show)

instance FromJSON EvalArgs where
  parseJSON = withObject "EvalArgs" $ \o ->
    EvalArgs <$> o .: "expression"

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right (EvalArgs expr) ->
    case sanitizeExpression expr of
      Left cmdErr ->
        pure (Env.toolResponseToResult (Env.mkRefused
          (Env.sanitizeRejection "expression" cmdErr)))
      Right safe -> do
        -- Inner per-eval budget. 'ghc_eval' is the only tool that
        -- interprets user-supplied expressions; without a tighter
        -- inner cap, pathological inputs ('threadDelay 60000000',
        -- 'let go = go in go', a blocking foreign call) would ride
        -- the 10-minute outer 'toolTimeoutMicros' ceiling and make
        -- the server unresponsive for the duration. 'Scenarios.
        -- FlowTimeoutEnforcement' pins this contract at ~30 s.
        --
        -- We run the full eval pipeline inside 'System.Timeout.
        -- timeout' and — on elapse — evict the GhcSession (the
        -- interrupted compile/action may have left 'HscEnv' in an
        -- indeterminate state) and render a structured
        -- 'error_kind=timeout' payload so clients can tell a
        -- user-level compile failure apart from a budget trip.
        mResult <- timeout evalTimeoutMicros (runEvalBody ghcSess safe)
        case mResult of
          Just tr -> pure tr
          Nothing -> do
            -- Reset the HscEnv-side state only. 'killGhcSession'
            -- would drain 'gsLock' without refilling it, which
            -- wedges the next 'withGhcSession' call — observable
            -- as an indefinite hang on 'Scenarios.
            -- FlowTimeoutEnforcement' step 4 (recovery).
            _ <- trySyncOnly (resetHscEnvInPlace ghcSess)
            pure timeoutResult

-- | Per-eval inner timeout. 30 s matches the ceiling documented in
-- 'Scenarios.FlowTimeoutEnforcement' and leaves comfortable
-- headroom under its 45 s failure threshold.
evalTimeoutMicros :: Int
evalTimeoutMicros = 30 * 1_000_000

evalTimeoutSeconds :: Int
evalTimeoutSeconds = evalTimeoutMicros `div` 1_000_000

-- | Try an IO action but re-throw any 'SomeAsyncException' — needed
-- so the 'Timeout' thrown by 'System.Timeout.timeout' into this
-- thread isn't silently swallowed by the inner 'try's below.
-- 'Timeout' is a 'SomeAsyncException' (see 'System.Timeout' in
-- base), so 'fromException' on 'SomeAsyncException' correctly
-- identifies it without needing a direct import of the private
-- 'Timeout' type.
trySyncOnly :: IO a -> IO (Either SomeException a)
trySyncOnly action = do
  res <- try action
  case res of
    Right a -> pure (Right a)
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (Left e)

-- | The original eval pipeline, unwrapped from the timeout envelope.
-- See the Server.hs comment on why both 'withGhcSession' and
-- 'withStanzaFlags' wrap every eval path.
runEvalBody :: GhcSession -> Text -> IO ToolResult
runEvalBody ghcSess safe = do
  -- Prime the session via 'loadForTarget' so the eval runs
  -- with: cabal stanza flags applied (exposes base / ghc-prim
  -- / user deps), targets set, module graph loaded, and the
  -- interactive context populated with 'Prelude' plus every
  -- home module. Without this priming, raw 'withGhcSession'
  -- hit either "no unit id matching 'ghc-prim'" (stanza flags
  -- never applied) or "Variable not in scope: double" (user
  -- module never loaded) depending on the scenario. One call
  -- to loadForTarget covers both.
  --
  -- loadForTarget caches the resulting HscEnv in 'gsEnvRef',
  -- so the subsequent 'withGhcSession' restores it verbatim
  -- via 'setSession' — no redundant re-compile.
  tgt <- firstLibraryOrTestSuite ghcSess
  _   <- trySyncOnly (loadForTarget ghcSess tgt Strict)
  -- Wrap both eval paths in 'withStanzaFlags' as well as
  -- 'withGhcSession'. If 'loadForTarget' succeeded the cached
  -- env already has the target's flags and withStanzaFlags is
  -- a no-op skip; if loadForTarget failed mid-way (typical for
  -- scenarios that probe broken loads — non-UTF-8, ANSI-error
  -- source) the env is empty and withStanzaFlags re-applies,
  -- ensuring '-this-unit-id' is set before setContext /
  -- compileExpr run. Without this wrap, GHC panics with
  -- "findImportedModule: no home-unit".
  eFast <- trySyncOnly (withGhcSession ghcSess $
                          withStanzaFlags ghcSess tgt (evalShowPure safe))
  case eFast :: Either SomeException (Maybe Text) of
    Right (Just out) -> pure (renderOk (truncateOutput out))
    _ -> do
      eIO <- trySyncOnly (withGhcSession ghcSess $
                            withStanzaFlags ghcSess tgt $ do
                              augmentEvalContext
                              evalIOString (T.unpack safe))
      case eIO :: Either SomeException String of
        Right out ->
          pure (renderOk (truncateOutput (T.pack out)))
        Left ex ->
          -- Issue #90 §4: a compileExpr / evalIOString failure
          -- maps to status='failed' with kind='internal_error'.
          -- The user-facing 'message' stays terse; the full
          -- exception text lives in error.cause.
          pure (Env.toolResponseToResult (Env.mkFailed
            ((Env.mkErrorEnvelope Env.InternalError
                ("ghc_eval failed: " <> T.take 200 (T.pack (show ex))))
                  { Env.eeCause = Just (T.pack (show ex)) })))

-- | Issue #90 Phase B: timeout maps to status='timeout' with
-- error.kind='inner_timeout'. The envelope's ToJSON also surfaces
-- a top-level 'error_kind' (= "inner_timeout") for the migration
-- window, so consumers that key on the legacy field still see a
-- structured discriminator (@FlowTimeoutEnforcement@'s oracle was
-- updated in this commit to accept it). Clients that key on the
-- new 'status' field get the cleaner discriminator immediately.
timeoutResult :: ToolResult
timeoutResult =
  let timeoutMsg =
        "ghc_eval exceeded inner budget ("
          <> T.pack (show evalTimeoutSeconds)
          <> " s). GHC session evicted; next call boots fresh."
      cause = "inner-eval timeout @ " <> T.pack (show evalTimeoutSeconds) <> "s"
      remediation =
        "The session was evicted. Retry with a smaller or less divergent "
          <> "expression; the next call boots a fresh GhcSession."
      err = (Env.mkErrorEnvelope Env.InnerTimeout timeoutMsg)
              { Env.eeCause       = Just cause
              , Env.eeRemediation = Just remediation
              }
  in Env.toolResponseToResult (Env.mkTimeout err)

-- | Fast path: wrap user expr in 'show', compile, coerce. Returns
-- 'Nothing' if the wrap fails to compile (typically an IO
-- expression) so the caller can fall through to 'evalIOString'.
evalShowPure :: Text -> Ghc (Maybe Text)
evalShowPure expr = do
  augmentEvalContext
  let wrapped = "Prelude.show (" <> T.unpack expr <> ")"
  hv <- compileExpr wrapped
  let s = unsafeCoerce hv :: String
  length s `seq` pure (Just (T.pack s))

-- | Fallback baseline for bare scaffolds with no source modules yet
-- (so 'ApiSession.projectInteractiveImports' has nothing to derive
-- from). The real cure for "module not in scope" lives at load time
-- in 'ApiSession' — each of the three load paths now propagates
-- every @import …@ declaration from the project's own sources into
-- the interactive context verbatim, qualified + aliased and all.
--
-- This list covers:
--   * 'Prelude' — so the 'show'-wrapped fast path ('Prelude.show …')
--     compiles even when the project is empty, or 'withStanzaFlags'
--     left the context in an Prelude-less state.
--   * 'System.IO' / 'Data.List' / 'Control.Monad' / 'Control.Concurrent'
--     — commonly-reached-for modules in eval one-liners that a
--     bare-scaffold project (no source files yet) wouldn't auto-import.
--
-- With auto-import in place, this list is expected to *shrink* over
-- time: every scenario that motivates an addition here is a sign the
-- auto-import path missed a case, not a sign the list should grow.
augmentEvalContext :: Ghc ()
augmentEvalContext = do
  existing <- getContext
  let extras =
        [ "Prelude"
        , "System.IO"
        , "Data.List"
        , "Control.Monad"
        , "Control.Concurrent"
        ]
      newImports =
        [ IIDecl (simpleImportDecl (mkModuleName m)) | m <- extras ]
  setContext (existing <> newImports)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

data TruncatedOutput = TruncatedOutput
  { toOutput    :: !Text
  , toTruncated :: !Bool
  }

truncateOutput :: Text -> TruncatedOutput
truncateOutput output =
  let truncated = T.length output > maxEvalBytes
      capped    = if truncated
                    then T.take maxEvalBytes output
                    else output
  in TruncatedOutput { toOutput = capped, toTruncated = truncated }

renderOk :: TruncatedOutput -> ToolResult
renderOk t =
  let payload = object
        [ "output"    .= toOutput t
        , "truncated" .= toTruncated t
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

-- | Discriminate the FromJSON failure shape — same heuristic as
-- the other Phase-B migrations.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]
