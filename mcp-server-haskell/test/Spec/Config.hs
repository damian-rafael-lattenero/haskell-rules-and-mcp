-- | Unit tests for issue #287 — 'HaskellFlows.Config'.
--
-- Tests the 'Micros' newtype (unit conversions), 'defaultLimits'
-- (values match the old per-module literals exactly), and 'loadLimits'
-- (env-var override surface: valid, absent, and invalid inputs).
module Spec.Config
  ( -- * Micros newtype
    testMicrosSeconds
  , testMicrosMinutes
  , testMicrosOrd
  , testMicrosEq
    -- * defaultLimits
  , testDefaultHoogleTimeout
  , testDefaultHlintTimeout
  , testDefaultFormatTimeout
  , testDefaultCabalCheckTimeout
  , testDefaultVersionTimeout
  , testDefaultEvalTimeout
  , testDefaultQuickCheckTimeout
  , testDefaultReplayTimeout
  , testDefaultOuterToolCeiling
  , testDefaultDeterminismMaxRuns
  , testDefaultQcMaxSuccess
  , testDefaultEvalOutputCap
  , testDefaultGateOutputCap
  , testDefaultCheckProjectTimeout
    -- * loadLimits
  , testLoadLimitsMissingEnvFallback
  , testLoadLimitsValidEnvOverride
  , testLoadLimitsInvalidEnvFallback
  , testLoadLimitsZeroEnvFallback
  , testLoadLimitsNegativeEnvFallback
  , testLoadLimitsCheckProjectOverride
  , testLoadLimitsOuterCeilingOverride
  , testLoadLimitsGhcSessionFieldsUnchanged
  ) where

import System.Environment (setEnv, unsetEnv)

import HaskellFlows.Config
  ( Limits (..)
  , Micros (..)
  , defaultLimits
  , loadLimits
  , minutes
  , seconds
  )

-- ---------------------------------------------------------------------------
-- Micros newtype
-- ---------------------------------------------------------------------------

-- | @seconds n@ lifts @n@ whole seconds to microseconds.
testMicrosSeconds :: IO Bool
testMicrosSeconds =
  pure $ unMicros (seconds 5) == 5_000_000
      && unMicros (seconds 0) == 0
      && unMicros (seconds 1) == 1_000_000

-- | @minutes n@ lifts @n@ whole minutes to microseconds.
testMicrosMinutes :: IO Bool
testMicrosMinutes =
  pure $ unMicros (minutes 1) == 60_000_000
      && unMicros (minutes 10) == 600_000_000

-- | 'Micros' derives 'Ord' — larger microsecond count is greater.
testMicrosOrd :: IO Bool
testMicrosOrd =
  pure $ seconds 5 > seconds 3
      && seconds 1 < seconds 2
      && (seconds 5 == seconds 5)

-- | 'Micros' derives 'Eq'.
testMicrosEq :: IO Bool
testMicrosEq =
  pure $ seconds 10 == Micros 10_000_000
      && seconds 10 /= seconds 9

-- ---------------------------------------------------------------------------
-- defaultLimits — values must match the old per-module literals exactly
-- so the centralisation is behaviour-preserving.
-- ---------------------------------------------------------------------------

-- | Hoogle subprocess timeout was 10_000000 (10 s, note the old
-- mis-grouped underscore that #287 fixes via the newtype).
testDefaultHoogleTimeout :: IO Bool
testDefaultHoogleTimeout =
  pure $ unMicros (hoogleTimeout defaultLimits) == 10_000_000

-- | HLint subprocess timeout was 60 * 1_000_000 = 60 s.
testDefaultHlintTimeout :: IO Bool
testDefaultHlintTimeout =
  pure $ unMicros (hlintTimeout defaultLimits) == 60_000_000

-- | Formatter subprocess timeout was 30 * 1_000_000 = 30 s.
testDefaultFormatTimeout :: IO Bool
testDefaultFormatTimeout =
  pure $ unMicros (formatTimeout defaultLimits) == 30_000_000

-- | cabal check timeout was 30 * 1_000_000 = 30 s.
testDefaultCabalCheckTimeout :: IO Bool
testDefaultCabalCheckTimeout =
  pure $ unMicros (cabalCheckTimeout defaultLimits) == 30_000_000

-- | Per-binary version probe timeout was 3_000_000 = 3 s.
testDefaultVersionTimeout :: IO Bool
testDefaultVersionTimeout =
  pure $ unMicros (versionTimeout defaultLimits) == 3_000_000

-- | ghc_eval inner budget was 30_000_000 = 30 s.
testDefaultEvalTimeout :: IO Bool
testDefaultEvalTimeout =
  pure $ unMicros (evalTimeout defaultLimits) == 30_000_000

-- | ghc_quickcheck inner budget was 30_000_000 = 30 s.
testDefaultQuickCheckTimeout :: IO Bool
testDefaultQuickCheckTimeout =
  pure $ unMicros (quickCheckTimeout defaultLimits) == 30_000_000

-- | Property replay budget was 30_000_000 = 30 s.
testDefaultReplayTimeout :: IO Bool
testDefaultReplayTimeout =
  pure $ unMicros (replayTimeout defaultLimits) == 30_000_000

-- | Outer tool ceiling is 10 minutes.
testDefaultOuterToolCeiling :: IO Bool
testDefaultOuterToolCeiling =
  pure $ outerToolCeiling defaultLimits == minutes 10

-- | Determinism max runs cap was 20.
testDefaultDeterminismMaxRuns :: IO Bool
testDefaultDeterminismMaxRuns =
  pure $ determinismMaxRuns defaultLimits == 20

-- | QuickCheck maxSuccess was 300.
testDefaultQcMaxSuccess :: IO Bool
testDefaultQcMaxSuccess =
  pure $ quickCheckMaxSuccess defaultLimits == 300

-- | Eval output cap was 64 KiB = 65536 bytes.
testDefaultEvalOutputCap :: IO Bool
testDefaultEvalOutputCap =
  pure $ evalOutputCapBytes defaultLimits == 64 * 1024

-- | Gate output cap was 256 KiB = 262144 bytes.
testDefaultGateOutputCap :: IO Bool
testDefaultGateOutputCap =
  pure $ gateOutputCapBytes defaultLimits == 256 * 1024

-- | ghc_check_project overall budget is 600 s (5× the old 120 s hardcoded default).
testDefaultCheckProjectTimeout :: IO Bool
testDefaultCheckProjectTimeout =
  pure $ unMicros (checkProjectTimeout defaultLimits) == 600_000_000

-- ---------------------------------------------------------------------------
-- loadLimits — env-var override surface
-- ---------------------------------------------------------------------------

-- | When the env vars are absent, 'loadLimits' returns 'defaultLimits'.
testLoadLimitsMissingEnvFallback :: IO Bool
testLoadLimitsMissingEnvFallback = do
  mapM_ unsetEnv envVars
  lim <- loadLimits
  pure $ hoogleTimeout lim == hoogleTimeout defaultLimits
      && hlintTimeout  lim == hlintTimeout  defaultLimits
      && formatTimeout lim == formatTimeout defaultLimits

-- | A valid positive-integer env var overrides the corresponding field.
testLoadLimitsValidEnvOverride :: IO Bool
testLoadLimitsValidEnvOverride = do
  mapM_ unsetEnv envVars
  setEnv "HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS" "42"
  lim <- loadLimits
  unsetEnv "HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS"
  pure $ unMicros (hoogleTimeout lim) == 42_000_000

-- | A non-numeric env var silently falls back to the default.
testLoadLimitsInvalidEnvFallback :: IO Bool
testLoadLimitsInvalidEnvFallback = do
  mapM_ unsetEnv envVars
  setEnv "HASKELL_FLOWS_HLINT_TIMEOUT_SECS" "notanumber"
  lim <- loadLimits
  unsetEnv "HASKELL_FLOWS_HLINT_TIMEOUT_SECS"
  pure $ hlintTimeout lim == hlintTimeout defaultLimits

-- | Zero is not a positive integer — falls back to default.
testLoadLimitsZeroEnvFallback :: IO Bool
testLoadLimitsZeroEnvFallback = do
  mapM_ unsetEnv envVars
  setEnv "HASKELL_FLOWS_FORMAT_TIMEOUT_SECS" "0"
  lim <- loadLimits
  unsetEnv "HASKELL_FLOWS_FORMAT_TIMEOUT_SECS"
  pure $ formatTimeout lim == formatTimeout defaultLimits

-- | A negative value is not positive — falls back to default.
testLoadLimitsNegativeEnvFallback :: IO Bool
testLoadLimitsNegativeEnvFallback = do
  mapM_ unsetEnv envVars
  setEnv "HASKELL_FLOWS_VERSION_TIMEOUT_SECS" "-5"
  lim <- loadLimits
  unsetEnv "HASKELL_FLOWS_VERSION_TIMEOUT_SECS"
  pure $ versionTimeout lim == versionTimeout defaultLimits

-- | HASKELL_FLOWS_CHECK_PROJECT_TIMEOUT_SECS overrides checkProjectTimeout.
testLoadLimitsCheckProjectOverride :: IO Bool
testLoadLimitsCheckProjectOverride = do
  mapM_ unsetEnv envVars
  setEnv "HASKELL_FLOWS_CHECK_PROJECT_TIMEOUT_SECS" "300"
  lim <- loadLimits
  unsetEnv "HASKELL_FLOWS_CHECK_PROJECT_TIMEOUT_SECS"
  pure $ unMicros (checkProjectTimeout lim) == 300_000_000

-- | HASKELL_FLOWS_OUTER_CEILING_MINS overrides outerToolCeiling.
testLoadLimitsOuterCeilingOverride :: IO Bool
testLoadLimitsOuterCeilingOverride = do
  mapM_ unsetEnv envVars
  setEnv "HASKELL_FLOWS_OUTER_CEILING_MINS" "20"
  lim <- loadLimits
  unsetEnv "HASKELL_FLOWS_OUTER_CEILING_MINS"
  pure $ outerToolCeiling lim == minutes 20

-- | GHC-session and cap fields (not env-overridable) are never changed.
testLoadLimitsGhcSessionFieldsUnchanged :: IO Bool
testLoadLimitsGhcSessionFieldsUnchanged = do
  mapM_ unsetEnv envVars
  setEnv "HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS" "99"
  lim <- loadLimits
  unsetEnv "HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS"
  pure $ evalTimeout          lim == evalTimeout          defaultLimits
      && quickCheckTimeout    lim == quickCheckTimeout    defaultLimits
      && replayTimeout        lim == replayTimeout        defaultLimits
      && determinismMaxRuns   lim == determinismMaxRuns   defaultLimits
      && quickCheckMaxSuccess lim == quickCheckMaxSuccess defaultLimits
      && evalOutputCapBytes   lim == evalOutputCapBytes   defaultLimits
      && gateOutputCapBytes   lim == gateOutputCapBytes   defaultLimits

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

envVars :: [String]
envVars =
  [ "HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS"
  , "HASKELL_FLOWS_HLINT_TIMEOUT_SECS"
  , "HASKELL_FLOWS_FORMAT_TIMEOUT_SECS"
  , "HASKELL_FLOWS_CABAL_CHECK_TIMEOUT_SECS"
  , "HASKELL_FLOWS_VERSION_TIMEOUT_SECS"
  , "HASKELL_FLOWS_CHECK_PROJECT_TIMEOUT_SECS"
  , "HASKELL_FLOWS_OUTER_CEILING_MINS"
  ]
