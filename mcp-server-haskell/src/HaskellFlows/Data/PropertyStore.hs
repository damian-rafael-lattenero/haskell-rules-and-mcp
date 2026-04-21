-- | Persistent store for QuickCheck properties that have passed at least
-- once.
--
-- The store is a single JSON file under the project's
-- @.haskell-flows\/@ directory. Each entry records the property's source
-- text, the module it belongs to (so the regression tool can reload the
-- right context), the number of cases it has passed historically, and a
-- timestamp for the most recent successful run.
--
-- Concurrency model: two layers.
--
--   * Each 'Store' value carries an in-process 'MVar' ('sLock')
--     that serialises writers from a single Server.
--   * Every save/remove/loadAll ALSO takes a second lock
--     ('withGlobalStoreLock') that covers the two-Server-one-dir
--     case — a TOP-LEVEL MVar keyed on the path (in-process) AND
--     an exclusive flock on a sidecar @.lock@ file (cross-process).
--     Found-by: 'Scenarios.FlowPropertyStoreRace' in the e2e suite
--     (two McpClients, one lost its property on the concurrent save).
--
-- Security note: the store path is always derived from a validated
-- 'ProjectDir' — it cannot escape the project root. The JSON file is
-- never evaluated as code, only parsed via aeson.
module HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , openStore
  , save
  , loadAll
  , remove
  , storePath
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , eitherDecodeFileStrict'
  , encodeFile
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Handle.Lock (hLock, hUnlock, LockMode (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), openFile, hClose)
import System.IO.Unsafe (unsafePerformIO)

import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | In-process MVar that serialises every read-modify-write of the
-- property store across ALL Servers in this process. Paired with the
-- on-disk flock below, this closes the two-MCP-clients-one-project
-- race the flock alone cannot handle (two FDs from the same process
-- cannot reliably contend for a POSIX flock).
{-# NOINLINE inProcessStoreLock #-}
inProcessStoreLock :: MVar ()
inProcessStoreLock = unsafePerformIO (newMVar ())

-- | Hold both layers of the lock for the duration of @action@. The
-- cross-process flock lives on a @<store>.lock@ sidecar so it does
-- not conflict with the @encodeFile@/@eitherDecodeFileStrict'@ write
-- path on the store itself (GHC POSIX-flock-on-read-handle blocks
-- later writes on some configurations — see the same pattern in
-- 'HaskellFlows.Tool.Deps.withCabalLock').
withGlobalStoreLock :: FilePath -> IO a -> IO a
withGlobalStoreLock storeFile action =
  withMVar inProcessStoreLock $ \_ -> do
    createDirectoryIfMissing True (takeDirectory storeFile)
    let lockPath = storeFile <> ".lock"
    bracket
      (do h <- openFile lockPath AppendMode
          hLock h ExclusiveLock
          pure h)
      (\h -> hUnlock h >> hClose h)
      (const action)

-- | An in-memory handle to the on-disk store. Serialises concurrent
-- access through an MVar so concurrent 'save' calls don't race on the
-- read-modify-write cycle.
data Store = Store
  { sFile :: !FilePath
  , sLock :: !(MVar ())
  }

-- | One persisted property record.
--
-- 'spModule' is the source module the property was defined in (e.g.
-- @src/Foo.hs@), used by the regression runner to re-load the right
-- scope before evaluating. 'spPassed' tracks the cumulative pass count
-- across all historical runs — a proxy for \"this property is trusted\".
data StoredProperty = StoredProperty
  { spExpression :: !Text
  , spModule     :: !(Maybe Text)
  , spPassed     :: !Int
  , spUpdated    :: !Double  -- POSIX seconds
  }
  deriving stock (Eq, Show)

instance ToJSON StoredProperty where
  toJSON p =
    object
      [ "expression" .= spExpression p
      , "module"     .= spModule p
      , "passed"     .= spPassed p
      , "updated"    .= spUpdated p
      ]

instance FromJSON StoredProperty where
  parseJSON = withObject "StoredProperty" $ \o -> do
    e <- o .:  "expression"
    m <- o .:? "module"
    p <- o .:? "passed"  .!= 0
    u <- o .:? "updated" .!= 0
    pure StoredProperty
      { spExpression = e
      , spModule     = m
      , spPassed     = p
      , spUpdated    = u
      }

-- | The canonical on-disk path for a project's property store.
storePath :: ProjectDir -> FilePath
storePath pd = unProjectDir pd </> ".haskell-flows" </> "properties.json"

-- | Open the store for a project. Creates the directory on first use;
-- the JSON file is created lazily on the first 'save'.
openStore :: ProjectDir -> IO Store
openStore pd = do
  let file = storePath pd
      dir  = unProjectDir pd </> ".haskell-flows"
  createDirectoryIfMissing True dir
  lock <- newMVar ()
  pure Store { sFile = file, sLock = lock }

-- | Load every stored property. Returns @[]@ on a missing or corrupted
-- file rather than throwing — a fresh project has no store yet.
loadAll :: Store -> IO [StoredProperty]
loadAll s = withGlobalStoreLock (sFile s) $ withMVar (sLock s) $ \_ -> do
  exists <- doesFileExist (sFile s)
  if not exists
    then pure []
    else do
      res <- try (eitherDecodeFileStrict' (sFile s))
               :: IO (Either SomeException (Either String [StoredProperty]))
      case res of
        Left _           -> pure []
        Right (Left _)   -> pure []
        Right (Right ps) -> pure ps

-- | Insert or update an entry identified by @expression@ (+ optional
-- @module@). Increments the pass count and refreshes the timestamp.
--
-- BUG-04 defence-in-depth: re-assert the parent directory exists
-- before every write. @openStore@ creates it once at server boot,
-- but the ProjectDir may not have existed at boot time (scaffold
-- happens later), and external deletes (user @rm -rf@, stale
-- git-clean) can erase it between server start and the first
-- save. An unconditional @createDirectoryIfMissing True@ is
-- O(stat) + cheap on the happy path and turns a crash into a
-- silent no-op on the bad path.
save :: Store -> Text -> Maybe Text -> IO ()
save s expr mModule = withGlobalStoreLock (sFile s) $ withMVar (sLock s) $ \_ -> do
  now  <- realToFrac <$> getPOSIXTime
  curr <- loadCurrent
  let updated = upsert curr now
  createDirectoryIfMissing True (takeDirectory (sFile s))
  encodeFile (sFile s) updated
  where
    loadCurrent :: IO [StoredProperty]
    loadCurrent = do
      exists <- doesFileExist (sFile s)
      if not exists
        then pure []
        else do
          res <- try (eitherDecodeFileStrict' (sFile s))
                   :: IO (Either SomeException (Either String [StoredProperty]))
          case res of
            Right (Right ps) -> pure ps
            _                -> pure []

    keyMatches p =
      spExpression p == expr && spModule p == mModule

    upsert :: [StoredProperty] -> Double -> [StoredProperty]
    upsert curr now =
      case break keyMatches curr of
        (pre, p : suf) ->
          pre <> [ p { spPassed = spPassed p + 1, spUpdated = now } ] <> suf
        (pre, []) ->
          pre <> [ StoredProperty
                     { spExpression = expr
                     , spModule     = mModule
                     , spPassed     = 1
                     , spUpdated    = now
                     }
                 ]

-- | Delete an entry matching @expression@ + optional @module@. No-op if
-- the entry doesn't exist. BUG-04 defence mirror of 'save' — the
-- write path re-asserts the dir exists.
remove :: Store -> Text -> Maybe Text -> IO ()
remove s expr mModule = withGlobalStoreLock (sFile s) $ withMVar (sLock s) $ \_ -> do
  exists <- doesFileExist (sFile s)
  if not exists
    then pure ()
    else do
      res <- try (eitherDecodeFileStrict' (sFile s))
               :: IO (Either SomeException (Either String [StoredProperty]))
      case res of
        Right (Right ps) -> do
          let keep p = not (spExpression p == expr && spModule p == mModule)
              filtered = filter keep ps
          createDirectoryIfMissing True (takeDirectory (sFile s))
          encodeFile (sFile s) filtered
        _ -> pure ()

