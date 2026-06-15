-- | Unit tests for 'HaskellFlows.Util.Process.runArgv' (#288).
--
-- Three properties:
--   1. A fast command that exits 0 → Completed with srExit=ExitSuccess and
--      the expected stdout.
--   2. A command that takes longer than the budget → TimedOut (child killed).
--   3. A command that exits non-zero → Completed with ExitFailure.
module Spec.ProcessUnit
  ( testRunArgvCompletes
  , testRunArgvTimeout
  , testRunArgvNonZeroExit
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))

import HaskellFlows.Config (Micros (..))
import HaskellFlows.Util.Process
  ( SubprocessOutcome (..)
  , SubprocessResult (..)
  , runArgv
  )

-- | /bin/echo completes quickly, stdout matches the argument.
testRunArgvCompletes :: IO Bool
testRunArgvCompletes = do
  outcome <- runArgv (Micros 2_000_000) Nothing "/bin/echo" ["hello"]
  pure $ case outcome of
    TimedOut    -> False
    Completed r ->
      srExit r == ExitSuccess
        && T.strip (srStdout r) == ("hello" :: Text)

-- | /bin/sleep with a 2 s budget → TimedOut; the child is killed by
-- runArgv before returning so no zombie is left behind.
-- Budget 100 ms was too tight on loaded CI runners (exec overhead
-- consumed the window); 2 s >> exec overhead gives a reliable signal.
testRunArgvTimeout :: IO Bool
testRunArgvTimeout = do
  outcome <- runArgv (Micros 2_000_000) Nothing "/bin/sleep" ["60"]
  pure (outcome == TimedOut)

-- | /usr/bin/false exits 1 → Completed with ExitFailure.
testRunArgvNonZeroExit :: IO Bool
testRunArgvNonZeroExit = do
  outcome <- runArgv (Micros 2_000_000) Nothing "/usr/bin/false" []
  pure $ case outcome of
    TimedOut    -> False
    Completed r -> case srExit r of
      ExitFailure _ -> True
      ExitSuccess   -> False
