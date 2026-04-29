-- | @ghc_regression@ — Wave-3 full in-process.
--
-- Replay every persisted QuickCheck property as a regression suite.
-- For each stored property, pick the target that owns its recorded
-- @module@ (test-suite / library / …), compile-load that target, then
-- run the property via 'evalIOString'.
--
-- Compared to the legacy subprocess path, no more @:load@ / @:show
-- modules@ dance — 'loadForTarget' owns scope selection.
module HaskellFlows.Tool.Regression
  ( descriptor
  , handle
  , RegressionArgs (..)
  , Action (..)
  , Replay (..)
  , runOne
  , parseShowModulesPaths
    -- * Load-failure detection (#51)
  , classifyLoadFailure
  , summariseLoadError
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified HaskellFlows.Mcp.Envelope as Env
import System.Timeout (timeout)

import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  )
import HaskellFlows.Ghc.ApiSession (GhcSession, gsProject)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import qualified HaskellFlows.Tool.QuickCheck as QcTool

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcRegression
    , tdDescription =
        "Replay every persisted QuickCheck property as a regression "
          <> "suite. Actions: 'list' (inspect the store without running), "
          <> "'run' (execute all). Properties are auto-persisted by "
          <> "ghc_quickcheck on first pass."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["list", "run"] :: [Text])
                  , "description" .= ("Default: 'run'." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data Action = ActList | ActRun
  deriving stock (Eq, Show)

newtype RegressionArgs = RegressionArgs
  { raAction :: Action
  }
  deriving stock (Show)

instance FromJSON RegressionArgs where
  parseJSON = withObject "RegressionArgs" $ \o -> do
    mAct <- o .:? "action"
    a <- case mAct :: Maybe Text of
      Nothing     -> pure ActRun
      Just "list" -> pure ActList
      Just "run"  -> pure ActRun
      Just other  -> fail ("unknown action: " <> T.unpack other)
    pure (RegressionArgs a)

-- | 30 s per property replay, mirroring the ghc_quickcheck budget.
replayTimeoutMicros :: Int
replayTimeoutMicros = 30_000_000

handle :: Store -> GhcSession -> Value -> IO ToolResult
handle store ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (parseErrorResult parseError)
  Right (RegressionArgs a) -> do
    props <- loadAll store
    case a of
      ActList -> pure (listResult props)
      ActRun  -> do
        results <- mapM (runOne ghcSess) props
        pure (runResult results)

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
-- running
--------------------------------------------------------------------------------

-- | One replayed property's outcome.
--
-- 'rpLoadFailure' (issue #51) is 'Just msg' when the cabal-repl
-- vehicle could not even compile the property's load scope —
-- typically because the recorded 'spModule' no longer imports
-- the symbols the lambda body references. In that case
-- 'rpResult' is the (best-effort) parsed result — usually
-- 'QcUnparsed' — but the caller MUST treat the replay as
-- skipped, not regressed: a property whose module failed to
-- load was never actually evaluated.
data Replay = Replay
  { rpStored      :: !StoredProperty
  , rpResult      :: !QuickCheckResult
  , rpLoadFailure :: !(Maybe Text)
  }

runOne :: GhcSession -> StoredProperty -> IO Replay
runOne ghcSess sp = do
  let expr = spExpression sp
  -- Wave-6: route the replay through the same subprocess-cabal-repl
  -- vehicle as ghc_quickcheck. The in-process evalIOString path
  -- depended on the GHC-API stanza-flag replay, which misresolved
  -- @-package-id QckChck-…@. cabal repl does that resolution
  -- natively and works every time.
  (qr, mLoadFail) <- case sanitizeExpression expr of
    Left _ ->
      pure ( QcUnparsed expr
                "boundary-sanitiser rejected the stored expression"
           , Nothing )
    Right safe -> do
      mRes <- timeout replayTimeoutMicros $
        try $ QcTool.runQuickCheckViaCabalRepl
                (gsProject ghcSess) (spModule sp) safe
      case mRes of
        Nothing -> pure (QcException expr "timeout", Nothing)
        Just (Left (ex :: SomeException)) ->
          pure (QcException expr (T.pack (show ex)), Nothing)
        Just (Right (out, err)) ->
          let parsed = parseQuickCheckOutput expr out
          in pure (parsed, classifyLoadFailure parsed err)
  pure Replay { rpStored = sp, rpResult = qr, rpLoadFailure = mLoadFail }

-- | Issue #51: when @parseQuickCheckOutput@ returns 'QcUnparsed'
-- (raw output empty / unrecognised) AND cabal-repl's stderr
-- carries telltale GHC load-failure messages, classify the
-- replay as a load failure rather than a regression. The
-- difference matters: a real regression means the property's
-- semantics changed; a load failure means the property never
-- actually ran. Conflating the two erodes trust in the
-- regression gate.
--
-- The detection is intentionally permissive: any of the GHC
-- error markers below are sufficient. False positives here
-- (a property that mentions \"not in scope\" in a string lit
-- and happens to also fail) are tolerable because the response
-- still surfaces the captured stderr verbatim — the agent can
-- read it and decide.
classifyLoadFailure :: QuickCheckResult -> Text -> Maybe Text
classifyLoadFailure (QcUnparsed _ raw) errStr
  | T.null (T.strip raw) && hasLoadMarker errStr =
      Just (summariseLoadError errStr)
classifyLoadFailure _ _ = Nothing

-- | The stable subset of GHC error fragments that signal a
-- module/load failure rather than a property-runtime failure.
hasLoadMarker :: Text -> Bool
hasLoadMarker err =
  let lo = T.toLower err
  in any (`T.isInfixOf` lo)
       [ "could not find module"
       , "could not load module"
       , "cannot find module"
       , "variable not in scope"
       , "not in scope:"
       , "module" `T.append` " is not loaded"
       , "module ‘"  -- unicode opening quote precedes module names
       ]

-- | Trim cabal-repl stderr to a JSON-friendly summary. Keeps the
-- first 600 characters and strips surrounding whitespace; that
-- is enough for the agent to identify the failing identifier
-- without bloating the response.
summariseLoadError :: Text -> Text
summariseLoadError err =
  let body   = T.strip err
      cap    = 600
      capped = if T.length body > cap
                 then T.take cap body <> "…(truncated)"
                 else body
  in capped

--------------------------------------------------------------------------------
-- :show modules parser (kept for unit-test coverage even though the
-- Wave-3 code path no longer invokes ghci meta-commands)
--------------------------------------------------------------------------------

parseShowModulesPaths :: Text -> [Text]
parseShowModulesPaths raw =
  [ path
  | ln <- T.lines raw
  , Just path <- [pathFromLine ln]
  ]
  where
    pathFromLine ln =
      let (_, afterOpen) = T.breakOn "(" ln
      in if T.null afterOpen
           then Nothing
           else
             let inside          = T.strip (T.drop 1 afterOpen)
                 (rawPath, _)    = T.breakOn "," inside
                 pathStripped    = T.strip rawPath
             in if T.null pathStripped
                  then Nothing
                  else Just pathStripped

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: list view → status='ok'. Pure introspection
-- of the on-disk property store.
listResult :: [StoredProperty] -> ToolResult
listResult props =
  Env.toolResponseToResult (Env.mkOk (object
    [ "action"     .= ("list" :: Text)
    , "count"      .= length props
    , "properties" .= map renderStored props
    ]))

-- | Issue #90 Phase C: replay run → status='ok' iff every stored
-- property re-played green. Any regression OR any property whose
-- module failed to load → status='failed' kind='validation'.
-- Both buckets ('regressions', 'load_failed') stay under 'result'
-- so consumers branch on the structured outcome.
runResult :: [Replay] -> ToolResult
runResult replays =
  let total          = length replays
      -- Issue #51: properties whose recorded module failed to
      -- compile/load are NOT regressions — they were never actually
      -- evaluated. Partition first.
      (loadFailed, evaluated) =
        foldr (\r (lf, ev) ->
                  if isJust (rpLoadFailure r)
                    then (r : lf, ev)
                    else (lf, r : ev))
              ([], []) replays
      regressions    = filter (not . isPass . rpResult) evaluated
      regressed      = length regressions
      loadFailures   = length loadFailed
      passed         = total - regressed - loadFailures
      success        = regressed == 0 && loadFailures == 0
      payload =
        object
          [ "action"         .= ("run" :: Text)
          , "total"          .= total
          , "passed"         .= passed
          , "regressions"    .= map renderRegression regressions
          , "load_failed"    .= map renderLoadFailed  loadFailed
          , "summary"        .= summarise total regressed loadFailures
          ]
  in if success
       then Env.toolResponseToResult (Env.mkOk payload)
       else
         let envErr   = Env.mkErrorEnvelope Env.Validation
                          (summarise total regressed loadFailures)
             response = (Env.mkFailed envErr) { Env.reResult = Just payload }
         in Env.toolResponseToResult response

isPass :: QuickCheckResult -> Bool
isPass (QcPassed _ _) = True
isPass _              = False

-- | Issue #51: humans + agents need to distinguish three states
-- in the run summary, not two. \"M of N regressed\" used to fire
-- even when M of N actually never replayed because their load
-- scope was stale. The new wording calls that out.
summarise :: Int -> Int -> Int -> Text
summarise 0 _ _ =
  "No stored properties. Run ghc_quickcheck and it'll auto-persist on pass."
summarise total 0 0 =
  T.pack (show total) <> " / " <> T.pack (show total) <> " stored properties pass."
summarise total regressed 0 =
  T.pack (show regressed) <> " of " <> T.pack (show total) <> " stored \
  \properties regressed. Details in 'regressions'."
summarise total 0 lf =
  T.pack (show lf) <> " of " <> T.pack (show total) <> " stored properties \
  \could not replay (module failed to load). Details in 'load_failed'."
summarise total regressed lf =
  T.pack (show regressed) <> " regressed, " <> T.pack (show lf) <>
  " could not replay (module failed to load) of " <>
  T.pack (show total) <> " stored properties. Details in 'regressions' + \
  \'load_failed'."

renderStored :: StoredProperty -> Value
renderStored sp =
  object
    [ "expression" .= spExpression sp
    , "module"     .= spModule sp
    , "passed"     .= spPassed sp
    , "updated"    .= spUpdated sp
    ]

renderRegression :: Replay -> Value
renderRegression r =
  object
    [ "expression" .= spExpression (rpStored r)
    , "module"     .= spModule (rpStored r)
    , "outcome"    .= renderOutcome (rpResult r)
    ]

-- | Issue #51: distinct shape for replays that were skipped due
-- to a module load failure. The 'state' is "load_failed" (not
-- "unparsed") so callers can branch on it; 'error' carries the
-- summarised cabal-repl stderr.
renderLoadFailed :: Replay -> Value
renderLoadFailed r =
  object
    [ "expression" .= spExpression (rpStored r)
    , "module"     .= spModule (rpStored r)
    , "outcome"    .= object
        [ "state" .= ("load_failed" :: Text)
        , "error" .= fromMaybe ("(no captured stderr)" :: Text)
                       (rpLoadFailure r)
        ]
    ]

renderOutcome :: QuickCheckResult -> Value
renderOutcome = \case
  QcPassed _ n          -> object [ "state" .= ("passed" :: Text), "passed" .= n ]
  QcFailed _ n shr cex  -> object
                             [ "state"          .= ("failed" :: Text)
                             , "passed"         .= n
                             , "shrinks"        .= shr
                             , "counterexample" .= cex
                             ]
  QcException _ err     -> object [ "state" .= ("exception" :: Text), "error" .= err ]
  QcGaveUp _ n disc     -> object
                             [ "state"     .= ("gave_up" :: Text)
                             , "passed"    .= n
                             , "discarded" .= disc
                             ]
  QcUnparsed _ raw      -> object [ "state" .= ("unparsed" :: Text), "raw" .= raw ]

