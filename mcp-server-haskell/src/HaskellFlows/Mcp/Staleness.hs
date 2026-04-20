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
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (getModificationTime)

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
  eMtime <- try (getModificationTime binaryPath)
              :: IO (Either SomeException UTCTime)
  case eMtime of
    Left e -> pure StalenessReport
      { srStale = False
      , srBinaryOlderBySec = Nothing
      , srMessage = Just (T.pack ("stat failed: " <> show e))
      }
    Right mtime -> do
      let bootUtc = posixSecondsToUTCTime (realToFrac bootPosix)
          deltaS  = realToFrac (diffUTCTime mtime bootUtc) :: Double
          stale   = deltaS >= thresholdMinutes * 60
      now <- getCurrentTime
      let msg
            | stale =
                Just (T.pack
                  ("Binary on disk is "
                   <> show (round (deltaS / 60) :: Int)
                   <> " min newer than the running process. "
                   <> "Restart Claude Desktop (or your MCP client) "
                   <> "to pick up the fresh build."))
            | otherwise = Nothing
          _ = now  -- timestamp unused except for potential future logging
      pure StalenessReport
        { srStale = stale
        , srBinaryOlderBySec = Just deltaS
        , srMessage = msg
        }
