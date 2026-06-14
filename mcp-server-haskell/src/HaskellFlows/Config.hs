-- | Centralized operational limits for haskell-flows-mcp.
--
-- All subprocess timeouts, GHC-session budgets, run caps, and output
-- caps are collected here instead of being scattered as private
-- top-level literals across tool modules.  The 'Micros' newtype
-- prevents seconds/microseconds confusion at 'System.Timeout.timeout'
-- call sites; 'loadLimits' gives ops a runtime-override surface via
-- @HASKELL_FLOWS_*@ environment variables without recompiling.
--
-- Env-var override surface:
--
-- @HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS@          — default 10
-- @HASKELL_FLOWS_HLINT_TIMEOUT_SECS@           — default 60
-- @HASKELL_FLOWS_FORMAT_TIMEOUT_SECS@          — default 30
-- @HASKELL_FLOWS_CABAL_CHECK_TIMEOUT_SECS@     — default 30
-- @HASKELL_FLOWS_VERSION_TIMEOUT_SECS@         — default 3
-- @HASKELL_FLOWS_CHECK_PROJECT_TIMEOUT_SECS@   — default 600
-- @HASKELL_FLOWS_OUTER_CEILING_MINS@           — default 10
module HaskellFlows.Config
  ( -- * Microseconds newtype
    Micros (..)
  , seconds
  , minutes
    -- * Limits record
  , Limits (..)
  , defaultLimits
  , loadLimits
  ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Microseconds — the unit that 'System.Timeout.timeout' expects.
-- Wrapping it prevents the seconds↔micros confusion that bit us with
-- @hoogleTimeoutMicros = 10_000000@ (mis-grouped underscore in #287).
newtype Micros = Micros { unMicros :: Int }
  deriving stock (Eq, Ord, Show)

-- | Lift a whole-second count to 'Micros'.
seconds :: Int -> Micros
seconds n = Micros (n * 1_000_000)

-- | Lift a whole-minute count to 'Micros'.
minutes :: Int -> Micros
minutes n = Micros (n * 60 * 1_000_000)

-- | All operational limits for the MCP server in one place.
data Limits = Limits
  { -- Subprocess timeouts (env-var overridable via loadLimits)
    hoogleTimeout      :: !Micros
    -- ^ Budget for the @hoogle@ subprocess per query.
  , hlintTimeout       :: !Micros
    -- ^ Budget for the @hlint@ subprocess per invocation.
  , formatTimeout      :: !Micros
    -- ^ Budget for @fourmolu@/@ormolu@ per file.
  , cabalCheckTimeout  :: !Micros
    -- ^ Budget for @cabal check@ in ghc_project validate.
  , versionTimeout     :: !Micros
    -- ^ Per-binary probe budget inside ghc_toolchain status.
    -- GHC-session budgets (defaultLimits only — not env-overridable yet)
  , evalTimeout        :: !Micros
    -- ^ Inner budget for ghc_eval expressions.
  , quickCheckTimeout  :: !Micros
    -- ^ Inner budget per ghc_quickcheck run.
  , replayTimeout      :: !Micros
    -- ^ Per-property budget when replaying the persisted store.
    -- Caps
  , outerToolCeiling   :: !Micros
    -- ^ Hard ceiling in Server.runTool (defence-in-depth).
  , determinismMaxRuns :: !Int
    -- ^ Maximum @runs@ accepted by ghc_quickcheck.
  , quickCheckMaxSuccess :: !Int
    -- ^ @maxSuccess@ passed to QuickCheck in ghc_quickcheck.
  , evalOutputCapBytes :: !Int
    -- ^ Maximum characters returned by ghc_eval (64 KiB).
  , gateOutputCapBytes :: !Int
    -- ^ Maximum stdout/stderr captured per cabal step in ghc_gate.
  , checkProjectTimeout :: !Micros
    -- ^ Overall wall-clock budget for ghc_check_project across all modules.
  }
  deriving stock (Eq, Show)

-- | Canonical defaults — match the old per-module literals exactly
-- so this is a pure centralisation with no behaviour change.
defaultLimits :: Limits
defaultLimits = Limits
  { hoogleTimeout      = seconds 10
  , hlintTimeout       = seconds 60
  , formatTimeout      = seconds 30
  , cabalCheckTimeout  = seconds 30
  , versionTimeout     = seconds 3
  , evalTimeout        = seconds 30
  , quickCheckTimeout  = seconds 30
  , replayTimeout      = seconds 30
  , outerToolCeiling   = minutes 10
  , determinismMaxRuns = 20
  , quickCheckMaxSuccess = 300
  , evalOutputCapBytes   = 64  * 1024
  , gateOutputCapBytes   = 256 * 1024
  , checkProjectTimeout  = seconds 600   -- 10 min; old hardcoded default was 120 s
  }

-- | Read @HASKELL_FLOWS_*@ env vars and override the subprocess
-- timeouts in 'defaultLimits'.  Invalid or absent vars silently fall
-- back to the default — the server must not fail to start over a
-- bad env override.
loadLimits :: IO Limits
loadLimits = do
  hoogle       <- readSecs "HASKELL_FLOWS_HOOGLE_TIMEOUT_SECS"        (hoogleTimeout        defaultLimits)
  hlint        <- readSecs "HASKELL_FLOWS_HLINT_TIMEOUT_SECS"         (hlintTimeout         defaultLimits)
  fmt          <- readSecs "HASKELL_FLOWS_FORMAT_TIMEOUT_SECS"        (formatTimeout        defaultLimits)
  cabal        <- readSecs "HASKELL_FLOWS_CABAL_CHECK_TIMEOUT_SECS"   (cabalCheckTimeout    defaultLimits)
  ver          <- readSecs "HASKELL_FLOWS_VERSION_TIMEOUT_SECS"       (versionTimeout       defaultLimits)
  checkProj    <- readSecs "HASKELL_FLOWS_CHECK_PROJECT_TIMEOUT_SECS" (checkProjectTimeout  defaultLimits)
  outerCeil    <- readMins "HASKELL_FLOWS_OUTER_CEILING_MINS"         (outerToolCeiling     defaultLimits)
  pure defaultLimits
    { hoogleTimeout        = hoogle
    , hlintTimeout         = hlint
    , formatTimeout        = fmt
    , cabalCheckTimeout    = cabal
    , versionTimeout       = ver
    , checkProjectTimeout  = checkProj
    , outerToolCeiling     = outerCeil
    }

-- | Parse a positive-integer env var as seconds and lift to 'Micros'.
-- Falls back to @fallback@ when the var is absent, empty, or non-numeric.
readSecs :: String -> Micros -> IO Micros
readSecs var fallback = do
  mVal <- lookupEnv var
  pure $ case mVal >>= readMaybe of
    Just n | n > 0 -> seconds n
    _              -> fallback

-- | Parse a positive-integer env var as minutes and lift to 'Micros'.
-- Falls back to @fallback@ when the var is absent, empty, or non-numeric.
readMins :: String -> Micros -> IO Micros
readMins var fallback = do
  mVal <- lookupEnv var
  pure $ case mVal >>= readMaybe of
    Just n | n > 0 -> minutes n
    _              -> fallback
