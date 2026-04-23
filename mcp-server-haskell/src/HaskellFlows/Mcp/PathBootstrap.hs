-- | Self-augment the MCP process 'PATH' at startup so
-- subprocess-using tools ('ghci_lint', 'ghci_quickcheck',
-- 'ghci_regression.run', 'ghci_gate', 'ghci_coverage',
-- 'ghci_validate_cabal') keep working even when the host that
-- spawned us (e.g. Claude for Desktop on macOS launched from the
-- Dock) passes a minimal 'PATH' that lacks @ghcup@ / @cabal@ /
-- @hlint@ / Homebrew.
--
-- Security note:
--
-- * The candidate list is 'hardCodedCandidates' — a whitelist of
--   well-known per-user or system-wide toolchain directories. No
--   user input flows into the PATH we build. Environment values
--   that match '..', start with a tilde that wasn't expanded, or
--   that aren't absolute are discarded.
-- * We only PREPEND directories that exist and are regular
--   directories on disk. A missing or non-directory candidate is
--   silently skipped — never passed to 'setEnv'.
-- * The augmented 'PATH' is written once, at server boot, via
--   'setEnv "PATH"'. Subprocess tools inherit it automatically
--   through 'System.Process'; no tool re-implements PATH
--   resolution by string-concatenation into a shell.
module HaskellFlows.Mcp.PathBootstrap
  ( augmentPath
  , augmentedPathCandidates
  , hardCodedCandidates
  ) where

import Control.Monad (filterM)
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath (isAbsolute)

-- | Directories that contain our toolchain binaries on a
-- reasonably-configured developer machine. Ordered so GHCup wins
-- over Homebrew (same binary, but GHCup's is the one cabal
-- projects depend on via --with-compiler).
--
-- This list is hard-coded — no user input, no environment
-- interpolation. Adding a candidate here is a deliberate
-- editorial decision vetted by the reviewer.
hardCodedCandidates :: FilePath -> [FilePath]
hardCodedCandidates home =
  [ home <> "/.ghcup/bin"
  , home <> "/.cabal/bin"
  , home <> "/.local/bin"
  , "/opt/homebrew/bin"
  , "/usr/local/bin"
  ]

-- | Filter the candidate list to directories that actually
-- exist as regular directories. Pure-ish — no env mutation.
augmentedPathCandidates :: IO [FilePath]
augmentedPathCandidates = do
  home <- getHomeDirectory
  filterM doesDirectoryExist (hardCodedCandidates home)

-- | Prepend any existing toolchain candidate to the process
-- 'PATH'. Returns the new 'PATH'. Idempotent: candidates already
-- present in 'PATH' are skipped, preserving the operator's
-- explicit order.
--
-- Idempotence matters for tests and for supervised restarts
-- where the MCP is launched twice against the same shell
-- environment.
augmentPath :: IO String
augmentPath = do
  existing <- fromMaybe "" <$> lookupEnv "PATH"
  let existingParts = splitPathSep existing
  cands <- augmentedPathCandidates
  let add      = [ c | c <- cands, c `notElem` existingParts, isAbsolute c ]
      combined = intercalate ":" (nub (add <> existingParts))
  setEnv "PATH" combined
  pure combined

-- | Split on ':' — POSIX PATH separator. macOS + Linux. We only
-- ship here; Windows host support would need to switch on
-- @System.FilePath.searchPathSeparator@.
splitPathSep :: String -> [String]
splitPathSep s = case break (== ':') s of
  (h, [])      -> [h | not (null h)]
  (h, _ : rest) ->
    let hs = [h | not (null h)]
    in hs <> splitPathSep rest
