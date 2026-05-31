-- | Staleness detector: compare the on-disk binary's mtime with
-- the process's own boot time. If the binary is meaningfully
-- newer than the running process, surface a warning — the user
-- has rebuilt but hasn't restarted their MCP client yet.
--
-- Port of the TS MCP's @staleness.ts@. Useful because our dev
-- loop is "edit src -> cabal install -> Cmd+Q -> relaunch" and
-- forgetting the relaunch is a common dogfood papercut.
--
-- Security: read-only filesystem stat of a single absolute path.
-- No agent input. Cached for 60s to cap the stat rate.
module HaskellFlows.Mcp.Staleness
  ( StalenessReport (..)
  , checkStaleness
  , thresholdMinutes
  , binaryIdentityStale
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (canonicalizePath, getModificationTime)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName, (</>))

-- | Minimum binary-vs-boot age gap before we flag stale (minutes).
-- One minute covers clock skew + install-time noise without
-- spamming the user right after a legitimate rebuild.
thresholdMinutes :: Double
thresholdMinutes = 1.0

data StalenessReport = StalenessReport
  { srStale            :: !Bool
  , srBinaryOlderBySec :: !(Maybe Double)
    -- ^ how much newer the binary is than the boot time, in seconds.
    -- 'Nothing' when the stat failed.
  , srMessage          :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance ToJSON StalenessReport where
  toJSON r = object
    [ "stale"   .= srStale r
    , "newerBy" .= srBinaryOlderBySec r
    , "message" .= srMessage r
    ]

-- | Compare the binary's mtime with the reference boot timestamp.
-- The caller supplies both: we never assume which binary path
-- corresponds to @argv[0]@ — binary location is a deployment
-- detail best left to the caller.
checkStaleness
  :: FilePath   -- ^ path to the binary on disk (e.g. @~/.local/bin/haskell-flows-mcp@)
  -> Double     -- ^ process boot time, POSIX seconds
  -> IO StalenessReport
checkStaleness binaryPath bootPosix = do
  -- #280: the primary signal is binary IDENTITY, not mtime. The running
  -- process's own file (binaryPath = getExecutablePath, already symlink-
  -- resolved) never changes after boot, so its mtime is useless for detecting
  -- "a newer binary was installed". Instead, compare the running binary's real
  -- path against what the canonical cabal-install symlink points at NOW: if
  -- they differ, a fresh binary is installed and the client is on a stale
  -- subprocess (the exact dogfood papercut — running ~/.local/bin while
  -- ~/.cabal/bin was rebuilt).
  mIdentity <- checkBinaryIdentity binaryPath
  eMtime <- try (getModificationTime binaryPath)
              :: IO (Either SomeException UTCTime)
  now <- getCurrentTime
  let mtimeReport = case eMtime of
        Left _      -> (False, Nothing)
        Right mtime ->
          let bootUtc = posixSecondsToUTCTime (realToFrac bootPosix)
              deltaS  = realToFrac (diffUTCTime mtime bootUtc) :: Double
          in (deltaS >= thresholdMinutes * 60, Just deltaS)
      (mtimeStale, deltaMaybe) = mtimeReport
      stale = mtimeStale || isJust mIdentity
      msg
        | Just installed <- mIdentity =
            Just (T.pack
              ("Running process is a DIFFERENT binary than the one installed at "
               <> installed <> ". A fresh build is in place — restart your MCP "
               <> "client (quit + relaunch) to pick it up."))
        | mtimeStale =
            Just (T.pack
              ("Binary on disk is "
               <> show (round (maybe 0 (/ 60) deltaMaybe) :: Int)
               <> " min newer than the running process. "
               <> "Restart Claude Desktop (or your MCP client) "
               <> "to pick up the fresh build."))
        | otherwise = Nothing
      _ = now  -- reserved for future logging
  pure StalenessReport
    { srStale = stale
    , srBinaryOlderBySec = deltaMaybe
    , srMessage = msg
    }

-- | #280: compare the running binary's real path with the file the canonical
-- @~/.cabal/bin/<name>@ symlink currently resolves to. Returns @Just target@
-- when they differ (a newer binary is installed and the process is stale),
-- @Nothing@ when they match or the comparison can't be made (no HOME, stat
-- failure, canonical path absent). Best-effort and read-only.
checkBinaryIdentity :: FilePath -> IO (Maybe FilePath)
checkBinaryIdentity runningExe = do
  mHome <- lookupEnv "HOME"
  case mHome of
    Nothing   -> pure Nothing
    Just home -> do
      let canonical = home </> ".cabal" </> "bin" </> takeFileName runningExe
      eRunning   <- try (canonicalizePath runningExe) :: IO (Either SomeException FilePath)
      eInstalled <- try (canonicalizePath canonical)  :: IO (Either SomeException FilePath)
      pure $ case (eRunning, eInstalled) of
        (Right rp, Right ip) -> binaryIdentityStale rp ip
        _                    -> Nothing

-- | Pure core of the identity check (exported for tests): when the running
-- real path differs from the installed real path, the process is stale and we
-- return the installed path; otherwise 'Nothing'.
binaryIdentityStale :: FilePath -> FilePath -> Maybe FilePath
binaryIdentityStale running installed
  | running /= installed = Just installed
  | otherwise            = Nothing
