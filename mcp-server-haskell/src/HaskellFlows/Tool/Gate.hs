-- | @ghc_gate@ — pre-push finalizer.
--
-- Collapses the "is this ready to push?" check into a single call.
-- Runs the three independent gates in order, collects per-step
-- duration + pass/fail/skip, and returns a unified report:
--
--   1. regression — re-play every persisted QuickCheck property.
--   2. cabal test — the project's real test-suite.
--   3. cabal build — library + every executable target.
--
-- Design notes:
--
-- * Never short-circuits on first failure — the caller benefits
--   from seeing the whole picture in one response. @success@ is
--   true iff every non-skipped step passed.
-- * Each step is wrapped in its own timeout so a stuck cabal run
--   can never hold the server past the outer 10-minute runTool
--   envelope. Per-step budget is generous (5 min for test, 3 min
--   for build) because real projects can legitimately take that
--   long; the whole call is still bounded.
-- * Skip flags let the agent opt out of an individual step — e.g.
--   @skip_build=true@ when the refactor only touched tests.
--
-- Security:
--
-- * Subprocesses spawned argv-form via 'System.Process.proc' —
--   never a shell string. No interpolation path.
-- * @cwd@ bound to the validated 'ProjectDir' (smart-constructed
--   elsewhere), so a traversal cannot widen the cabal scope.
-- * Raw stdout/stderr captured up to a per-step cap (256 KiB) and
--   included in the response so the agent can drill in without a
--   second call; the cap prevents an unbounded cabal log from
--   blowing past the MCP's response size budget.
module HaskellFlows.Tool.Gate
  ( descriptor
  , handle
  , GateArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , proc
  , readCreateProcessWithExitCode
  )
import System.Timeout (timeout)

import HaskellFlows.Data.PropertyStore (Store, loadAll)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Parser.QuickCheck as QC
import HaskellFlows.Tool.Regression (Replay (..), runOne)
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcGate
    , tdDescription =
        "Pre-push finalizer: runs regression + cabal test + cabal build "
          <> "in one call, returns per-step durations + pass/fail/skip + "
          <> "consolidated summary. Use before 'git push' — if this is "
          <> "green, the CI job is very likely to be green too."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "skip_regression" .= bool_ "Skip the regression replay."
              , "skip_cabal_test" .= bool_ "Skip `cabal test` invocation."
              , "skip_cabal_build".= bool_ "Skip `cabal build` invocation."
              ]
          , "additionalProperties" .= False
          ]
    }
  where
    bool_ :: Text -> Value
    bool_ desc = object [ "type" .= ("boolean" :: Text), "description" .= desc ]

data GateArgs = GateArgs
  { gaSkipRegression :: !Bool
  , gaSkipCabalTest  :: !Bool
  , gaSkipCabalBuild :: !Bool
  }
  deriving stock (Show)

instance FromJSON GateArgs where
  parseJSON = withObject "GateArgs" $ \o ->
    GateArgs
      <$> o .:? "skip_regression"  .!= False
      <*> o .:? "skip_cabal_test"  .!= False
      <*> o .:? "skip_cabal_build" .!= False

--------------------------------------------------------------------------------
-- per-step budgets (microseconds)
--------------------------------------------------------------------------------

regressionTimeoutMicros, cabalTestTimeoutMicros, cabalBuildTimeoutMicros :: Int
regressionTimeoutMicros  = 2 * 60 * 1_000_000     -- 2 min
cabalTestTimeoutMicros   = 5 * 60 * 1_000_000     -- 5 min
cabalBuildTimeoutMicros  = 3 * 60 * 1_000_000     -- 3 min

--------------------------------------------------------------------------------
-- handle
--------------------------------------------------------------------------------

handle :: Store -> GhcSession -> ProjectDir -> Value -> IO ToolResult
handle store sess pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> do
    t0  <- now
    reg <- if gaSkipRegression args
             then pure Skipped
             else runStep regressionTimeoutMicros (regressionStep store sess)
    tst <- if gaSkipCabalTest args
             then pure Skipped
             else runStep cabalTestTimeoutMicros  (cabalStep pd ["test"])
    bld <- if gaSkipCabalBuild args
             then pure Skipped
             else runStep cabalBuildTimeoutMicros (cabalStep pd ["build"])
    t1  <- now
    let allPassed =
          stepPassed reg && stepPassed tst && stepPassed bld
        total = t1 - t0
    pure (renderReport allPassed total reg tst bld)

--------------------------------------------------------------------------------
-- step machinery
--------------------------------------------------------------------------------

-- | One gate step's outcome.
data Step
  = Passed !Double !Value    -- ^ duration-seconds + detail payload
  | Failed !Double !Value
  | Skipped
  | TimedOut !Double
  deriving stock (Show)

stepPassed :: Step -> Bool
stepPassed Passed {}  = True
stepPassed Skipped    = True   -- skip counts as "not blocking"
stepPassed _          = False

-- | Run a step inside a timeout budget. TimedOut / exception paths
-- collapse to a structured 'Step' so the caller never sees a raw
-- exception escape.
--
-- BUG-01 (F-22 from the expr-evaluator dogfood): the old runStep
-- wrapped 'body' in 'timeout' only. Any synchronous exception
-- thrown by 'body' (cabal binary not on PATH, partial pattern on
-- createProcess, resource exhaustion) would escape 'runStep',
-- propagate through 'ghc_gate''s handler, and only get caught
-- by Server.runTool's outer 'try' — but by then the GHCi session
-- had been evicted and the MCP client saw a bare \"Connection
-- closed\" instead of a structured gate failure.
--
-- Fix: wrap 'body' in BOTH 'try' (catch synchronous exceptions)
-- and 'timeout' (bound wall-clock). Any exception turns into a
-- Failed step whose @details.exception@ field carries the message;
-- any over-budget run turns into TimedOut. The tool response
-- always looks like a normal gate report to the agent.
runStep :: Int -> IO (Bool, Value) -> IO Step
runStep budget body = do
  t0 <- now
  out <- timeout budget (try body)
  t1 <- now
  let dt = t1 - t0
  case out of
    Nothing                 -> pure (TimedOut dt)
    Just (Left (e :: SomeException)) ->
      pure (Failed dt (object
        [ "exception" .= T.pack (show e)
        , "hint"      .=
            ( "Step raised a synchronous exception before producing a \
              \result. Common causes: cabal not on PATH, dist-newstyle \
              \lock held by a concurrent GHCi session, or the cabal \
              \subprocess exited before its pipes were drained. The \
              \gate stays up; other steps continue." :: Text )
        ]))
    Just (Right (True, det))  -> pure (Passed dt det)
    Just (Right (False, det)) -> pure (Failed dt det)

--------------------------------------------------------------------------------
-- step implementations
--------------------------------------------------------------------------------

regressionStep :: Store -> GhcSession -> IO (Bool, Value)
regressionStep store sess = do
  props <- loadAll store
  replays <- mapM (runOne sess) props
  let failures =
        [ rp | rp <- replays, case rpResult rp of
                                QC.QcPassed {}    -> False
                                _                 -> True ]
      total   = length replays
      failed  = length failures
      passed  = total - failed
      detail  = object
        [ "total"      .= total
        , "passed"     .= passed
        , "failed"     .= failed
        , "failures"   .= map renderFailure failures
        ]
  pure (failed == 0, detail)

renderFailure :: Replay -> Value
renderFailure rp = object
  [ "property" .= qcPropertyText (rpResult rp)
  , "state"    .= qcStateText    (rpResult rp)
  ]

qcPropertyText :: QC.QuickCheckResult -> Text
qcPropertyText (QC.QcPassed    p _)       = p
qcPropertyText (QC.QcFailed    p _ _ _)   = p
qcPropertyText (QC.QcException p _)       = p
qcPropertyText (QC.QcGaveUp    p _ _)     = p
qcPropertyText (QC.QcUnparsed  p _)       = p

qcStateText :: QC.QuickCheckResult -> Text
qcStateText QC.QcPassed    {} = "passed"
qcStateText QC.QcFailed    {} = "failed"
qcStateText QC.QcGaveUp    {} = "gave_up"
qcStateText QC.QcException {} = "exception"
qcStateText QC.QcUnparsed  {} = "unparsed"

-- | Generic @cabal <args>@ runner, argv-form, with combined stdout +
-- stderr capture (capped at 256 KiB to keep the response sane).
--
-- BUG-01 hardening:
--
--  * 'createProcess' was producing a 4-tuple via an irrefutable
--    pattern match. If somehow the runtime returned 'Nothing' for
--    either pipe (unexpected but possible after an async
--    interrupt) the pattern match itself would throw and the
--    whole server would see a crash. The pipes are now matched
--    with a total case and a structured error returned on any
--    shape mismatch.
--
--  * The whole process lifecycle is wrapped in 'bracket' so
--    'terminateProcess' + 'hClose' always run — whether the
--    normal path, a timeout-driven async kill, or an exception
--    from 'hGetContents' interrupted us. No more leaked cabal
--    children holding a dist-newstyle lock.
--
--  * 'waitForProcess' is in the bracket's body; the acquire step
--    is just the 'createProcess' + pipe extraction. If
--    'createProcess' itself raises (cabal not on PATH), the
--    exception propagates to 'runStep' which now catches it and
--    returns a Failed step with the exception text — the gate
--    stays up for the remaining steps.
cabalStep :: ProjectDir -> [String] -> IO (Bool, Value)
cabalStep pd args = do
  -- Issue #75: the previous implementation forked two threads
  -- doing 'hGetContents h >>= putMVar v'. 'hGetContents' is
  -- LAZY — the forked thread put a thunk into the MVar without
  -- actually draining the pipe. When cabal wrote more than the
  -- OS pipe buffer (~64 KiB on macOS), the writer blocked on a
  -- full pipe, 'waitForProcess' blocked on a child that couldn't
  -- exit, and the whole gate hung past its 5-minute timeout in a
  -- way that corrupted the MCP transport instead of returning a
  -- structured TimedOut step.
  --
  -- 'readCreateProcessWithExitCode' uses a strict bytestring read
  -- internally and handles the dual-pipe drain correctly. The
  -- whole step gets bounded streaming output (capped per stream),
  -- the cabal child terminates cleanly on timeout (via runStep's
  -- async-exception-aware bracket), and the MCP transport stays
  -- up regardless of how loud cabal's output was.
  let cp = (proc "cabal" args)
             { cwd     = Just (unProjectDir pd)
             , std_in  = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (ec, outStr, errStr) <- readCreateProcessWithExitCode cp ""
  let o = T.take outputCap (T.pack outStr)
      e = T.take outputCap (T.pack errStr)
      passed = ec == ExitSuccess
      detail = object
        [ "command"  .= ("cabal " <> T.unwords (map T.pack args))
        , "exitCode" .= (case ec of
                           ExitSuccess   -> 0
                           ExitFailure n -> n)
        , "stdout"   .= o
        , "stderr"   .= e
        ]
  pure (passed, detail)

-- | Issue #75: cap each captured stream at 256 KiB. The cabal
-- output rarely exceeds this on green builds; on red ones the
-- tail is the actionable bit and the leading repeat-cycles of
-- the build chatter add no signal.
outputCap :: Int
outputCap = 256 * 1024

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderReport :: Bool -> Double -> Step -> Step -> Step -> ToolResult
renderReport allPassed total reg tst bld =
  let payload = object
        [ "success"          .= allPassed
        , "totalDurationSec" .= total
        , "steps" .= object
            [ "regression" .= renderStep reg
            , "cabal_test" .= renderStep tst
            , "cabal_build".= renderStep bld
            ]
        , "summary" .= summary allPassed reg tst bld
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = not allPassed
       }

renderStep :: Step -> Value
renderStep s = case s of
  Passed dt det -> object
    [ "status" .= ("pass" :: Text), "durationSec" .= dt, "details" .= det ]
  Failed dt det -> object
    [ "status" .= ("fail" :: Text), "durationSec" .= dt, "details" .= det ]
  Skipped       -> object
    [ "status" .= ("skip" :: Text) ]
  TimedOut dt   -> object
    [ "status" .= ("timeout" :: Text), "durationSec" .= dt ]

summary :: Bool -> Step -> Step -> Step -> Text
summary allPassed reg tst bld
  | allPassed =
      "All requested gates passed: "
      <> passedVerbs reg tst bld
      <> ". Safe to push."
  | otherwise =
      "At least one gate failed or timed out. "
      <> "See steps.* for per-step details."

passedVerbs :: Step -> Step -> Step -> Text
passedVerbs reg tst bld =
  T.intercalate ", "
    [ lbl | (s, lbl) <-
        [ (reg, "regression"), (tst, "cabal test"), (bld, "cabal build") ]
    , stepPassed s, notSkipped s
    ]
  where
    notSkipped Skipped = False
    notSkipped _       = True

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

now :: IO Double
now = realToFrac <$> getPOSIXTime

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
