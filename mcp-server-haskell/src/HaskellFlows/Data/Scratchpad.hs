-- | Persistent scratchpad for LLM-authored Haskell hypotheses.
--
-- Companion to 'HaskellFlows.Data.PropertyStore'. The property store
-- persists laws that have *passed* QuickCheck; the scratchpad persists
-- *in-flight* code the LLM is reasoning about — hypotheses to
-- type-check, refactor candidates to verify before promotion, and
-- snippets the user can read mid-session to see what the LLM is
-- thinking.
--
-- Storage: a single JSON file under the project's @.haskell-flows/@
-- directory (@scratchpad.json@). Same two-layer locking as the
-- property store — a process-wide MVar plus an on-disk flock on a
-- @<file>.lock@ sidecar — so two MCP clients editing the same project
-- cannot race on the read-modify-write cycle.
--
-- Security: the store path is always derived from a validated
-- 'ProjectDir' — it cannot escape the project root. The JSON file is
-- never evaluated as code, only parsed via aeson. The 'seCode' field
-- holds untrusted user-supplied Haskell; sanitisation lives in
-- 'HaskellFlows.Ghc.Sanitize' and is the consumer's responsibility,
-- not this module's.
module HaskellFlows.Data.Scratchpad
  ( Store
  , ScratchEntry (..)
  , ScratchKind (..)
  , ScratchStatus (..)
  , ScratchResult (..)
  , kindToText
  , textToKind
  , statusToText
  , textToStatus
  , openStore
  , save
  , loadAll
  , findById
  , remove
  , clearAll
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
  , withText
  , (.:)
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO.Handle.Lock (hLock, hUnlock, LockMode (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), openFile, hClose)
import System.IO.Unsafe (unsafePerformIO)

import HaskellFlows.Types (ProjectDir, unProjectDir)

--------------------------------------------------------------------------------
-- Locking (cloned from PropertyStore.hs:64–83)
--------------------------------------------------------------------------------

-- | In-process MVar that serialises every read-modify-write of the
-- scratchpad across ALL Servers in this process. Paired with the
-- on-disk flock below, this closes the two-MCP-clients-one-project
-- race the flock alone cannot handle (two FDs from the same process
-- cannot reliably contend for a POSIX flock).
--
-- Distinct from the PropertyStore lock so a property write does not
-- block a scratchpad write and vice versa.
{-# NOINLINE inProcessScratchLock #-}
inProcessScratchLock :: MVar ()
inProcessScratchLock = unsafePerformIO (newMVar ())

-- | Hold both layers of the lock for the duration of @action@. The
-- cross-process flock lives on a @<store>.lock@ sidecar so it does
-- not conflict with the @encodeFile@/@eitherDecodeFileStrict'@ write
-- path on the store itself (same pattern as PropertyStore).
withGlobalScratchLock :: FilePath -> IO a -> IO a
withGlobalScratchLock storeFile action =
  withMVar inProcessScratchLock $ \_ -> do
    createDirectoryIfMissing True (takeDirectory storeFile)
    let lockPath = storeFile <> ".lock"
    bracket
      (do h <- openFile lockPath AppendMode
          hLock h ExclusiveLock
          pure h)
      (\h -> hUnlock h >> hClose h)
      (const action)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Classification of what a scratchpad entry represents. Drives UI
-- presentation in @ghc_workflow(status)@ and the per-entry hint
-- @nextStep@ produces.
data ScratchKind
  = ScratchHypothesis
    -- ^ A "would this work?" snippet — primary use case. Default.
  | ScratchTypeCheck
    -- ^ A snippet primarily exercised for its type, not its value.
  | ScratchEval
    -- ^ A snippet intended to be run, not just type-checked.
  | ScratchNote
    -- ^ A code-shaped note (no compile intent) — for reasoning trails.
  deriving stock (Eq, Show)

kindToText :: ScratchKind -> Text
kindToText = \case
  ScratchHypothesis -> "hypothesis"
  ScratchTypeCheck  -> "type_check"
  ScratchEval       -> "eval"
  ScratchNote       -> "note"

textToKind :: Text -> Maybe ScratchKind
textToKind = \case
  "hypothesis" -> Just ScratchHypothesis
  "type_check" -> Just ScratchTypeCheck
  "eval"       -> Just ScratchEval
  "note"       -> Just ScratchNote
  _            -> Nothing

instance ToJSON ScratchKind where
  toJSON = toJSON . kindToText

instance FromJSON ScratchKind where
  parseJSON = withText "ScratchKind" $ \t ->
    case textToKind t of
      Just k  -> pure k
      Nothing -> fail ("unknown ScratchKind: " <> T.unpack t)

-- | Lifecycle state. Mostly informational; @check@ flips Open to
-- Verified on successful type-check, @promote@ flips Verified to
-- Promoted on successful splice + verify.
data ScratchStatus
  = ScratchOpen
    -- ^ Newly written, not yet verified. Default for @write@.
  | ScratchVerified
    -- ^ Last @check@ returned @srKind="type_ok"@.
  | ScratchPromoted
    -- ^ @promote@ spliced + verified into a real module.
  | ScratchAbandoned
    -- ^ Explicitly marked dead (last check failed and the LLM moved on).
  deriving stock (Eq, Show)

statusToText :: ScratchStatus -> Text
statusToText = \case
  ScratchOpen      -> "open"
  ScratchVerified  -> "verified"
  ScratchPromoted  -> "promoted"
  ScratchAbandoned -> "abandoned"

textToStatus :: Text -> Maybe ScratchStatus
textToStatus = \case
  "open"      -> Just ScratchOpen
  "verified"  -> Just ScratchVerified
  "promoted"  -> Just ScratchPromoted
  "abandoned" -> Just ScratchAbandoned
  _           -> Nothing

instance ToJSON ScratchStatus where
  toJSON = toJSON . statusToText

instance FromJSON ScratchStatus where
  parseJSON = withText "ScratchStatus" $ \t ->
    case textToStatus t of
      Just s  -> pure s
      Nothing -> fail ("unknown ScratchStatus: " <> T.unpack t)

-- | The result of the last @check@ / @eval@ / @promote@ action against
-- this entry. Kept as a single nested object so a future tool that
-- consumes the scratchpad (e.g. an export to test/Spec.hs) can branch
-- on @srKind@ without having to remember which top-level fields are
-- valid for which kind.
data ScratchResult = ScratchResult
  { srKind   :: !Text   -- ^ "type_ok" | "type_error" | "eval_ok" | "eval_error" | "promoted" | "rolled_back"
  , srDetail :: !Text   -- ^ inferred type, error text, eval output, etc.
  , srAt     :: !Double -- ^ POSIX seconds when this result was recorded
  }
  deriving stock (Eq, Show)

instance ToJSON ScratchResult where
  toJSON r =
    object
      [ "kind"   .= srKind r
      , "detail" .= srDetail r
      , "at"     .= srAt r
      ]

instance FromJSON ScratchResult where
  parseJSON = withObject "ScratchResult" $ \o -> do
    k <- o .:  "kind"
    d <- o .:  "detail"
    a <- o .:? "at" .!= 0
    pure ScratchResult { srKind = k, srDetail = d, srAt = a }

-- | One scratchpad entry. The complete unit the persistence layer
-- saves and loads.
--
-- 'seImports' is a per-entry import list spliced before type-checking;
-- it stays separate from the project's @import@s so a hypothesis can
-- pull in a stricter set than the host module would normally allow.
data ScratchEntry = ScratchEntry
  { seId      :: !Text
  , seKind    :: !ScratchKind
  , seCode    :: !Text
  , seModule  :: !(Maybe Text)
  , seImports :: ![Text]
  , seNote    :: !(Maybe Text)  -- natural-language annotation
  , seResult  :: !(Maybe ScratchResult)
  , seStatus  :: !ScratchStatus
  , seCreated :: !Double
  , seUpdated :: !Double
  }
  deriving stock (Eq, Show)

instance ToJSON ScratchEntry where
  toJSON e =
    object
      [ "id"       .= seId e
      , "kind"     .= seKind e
      , "code"     .= seCode e
      , "module"   .= seModule e
      , "imports"  .= seImports e
      , "note"     .= seNote e
      , "result"   .= seResult e
      , "status"   .= seStatus e
      , "created"  .= seCreated e
      , "updated"  .= seUpdated e
      ]

instance FromJSON ScratchEntry where
  parseJSON = withObject "ScratchEntry" $ \o -> do
    i  <- o .:  "id"
    k  <- o .:? "kind"    .!= ScratchHypothesis
    c  <- o .:  "code"
    m  <- o .:? "module"
    is <- o .:? "imports" .!= []
    nt <- o .:? "note"
    rs <- o .:? "result"
    s  <- o .:? "status"  .!= ScratchOpen
    cr <- o .:? "created" .!= 0
    up <- o .:? "updated" .!= 0
    pure ScratchEntry
      { seId       = i
      , seKind     = k
      , seCode     = c
      , seModule   = m
      , seImports  = is
      , seNote     = nt
      , seResult   = rs
      , seStatus   = s
      , seCreated  = cr
      , seUpdated  = up
      }

-- | An in-memory handle to the on-disk scratchpad. Serialises
-- concurrent access through an MVar so concurrent 'save' calls do not
-- race on the read-modify-write cycle.
data Store = Store
  { sFile :: !FilePath
  , sLock :: !(MVar ())
  }

--------------------------------------------------------------------------------
-- Persistence
--------------------------------------------------------------------------------

-- | The canonical on-disk path for a project's scratchpad.
storePath :: ProjectDir -> FilePath
storePath pd = unProjectDir pd </> ".haskell-flows" </> "scratchpad.json"

-- | Open the scratchpad for a project. Creates the directory on first
-- use; the JSON file is created lazily on the first 'save'.
openStore :: ProjectDir -> IO Store
openStore pd = do
  let file = storePath pd
      dir  = unProjectDir pd </> ".haskell-flows"
  createDirectoryIfMissing True dir
  lock <- newMVar ()
  pure Store { sFile = file, sLock = lock }

-- | Load every scratchpad entry. Returns @[]@ on a missing or
-- corrupted file rather than throwing — a fresh project has no
-- scratchpad yet.
loadAll :: Store -> IO [ScratchEntry]
loadAll s = withGlobalScratchLock (sFile s) $ withMVar (sLock s) $ \_ -> do
  exists <- doesFileExist (sFile s)
  if not exists
    then pure []
    else do
      res <- try (eitherDecodeFileStrict' (sFile s))
               :: IO (Either SomeException (Either String [ScratchEntry]))
      case res of
        Left _           -> pure []
        Right (Left _)   -> pure []
        Right (Right ps) -> pure ps

-- | Find an entry by id. 'Nothing' if no entry has that id.
findById :: Store -> Text -> IO (Maybe ScratchEntry)
findById s i = do
  entries <- loadAll s
  pure (lookupById i entries)

lookupById :: Text -> [ScratchEntry] -> Maybe ScratchEntry
lookupById i = go
  where
    go []     = Nothing
    go (e:es) = if seId e == i then Just e else go es

-- | Insert or update an entry. Replaces by 'seId' if present;
-- otherwise appends. Always refreshes 'seUpdated'.
--
-- BUG-04 defence-in-depth: re-assert the parent directory exists
-- before every write. 'openStore' creates it once at server boot,
-- but the ProjectDir may not have existed at boot time (scaffold
-- happens later), and external deletes (user @rm -rf@, stale
-- git-clean) can erase it between server start and the first
-- save. An unconditional @createDirectoryIfMissing True@ is
-- O(stat) + cheap on the happy path and turns a crash into a
-- silent no-op on the bad path.
save :: Store -> ScratchEntry -> IO ()
save s entry = withGlobalScratchLock (sFile s) $ withMVar (sLock s) $ \_ -> do
  now  <- realToFrac <$> getPOSIXTime
  curr <- loadCurrent
  let updated = upsert curr now
  createDirectoryIfMissing True (takeDirectory (sFile s))
  encodeFile (sFile s) updated
  where
    loadCurrent :: IO [ScratchEntry]
    loadCurrent = do
      exists <- doesFileExist (sFile s)
      if not exists
        then pure []
        else do
          res <- try (eitherDecodeFileStrict' (sFile s))
                   :: IO (Either SomeException (Either String [ScratchEntry]))
          case res of
            Right (Right ps) -> pure ps
            _                -> pure []

    upsert :: [ScratchEntry] -> Double -> [ScratchEntry]
    upsert curr now =
      case break (\e -> seId e == seId entry) curr of
        (pre, _ : suf) ->
          -- Preserve the original creation time; update everything else.
          let merged = entry { seUpdated = now }
          in pre <> [ merged ] <> suf
        (pre, []) ->
          let new = entry { seCreated = if seCreated entry == 0
                                          then now
                                          else seCreated entry
                          , seUpdated = now
                          }
          in pre <> [ new ]

-- | Delete an entry by id. No-op if it doesn't exist.
remove :: Store -> Text -> IO ()
remove s i = withGlobalScratchLock (sFile s) $ withMVar (sLock s) $ \_ -> do
  exists <- doesFileExist (sFile s)
  if not exists
    then pure ()
    else do
      res <- try (eitherDecodeFileStrict' (sFile s))
               :: IO (Either SomeException (Either String [ScratchEntry]))
      case res of
        Right (Right ps) -> do
          let keep e = seId e /= i
              filtered = filter keep ps
          createDirectoryIfMissing True (takeDirectory (sFile s))
          encodeFile (sFile s) filtered
        _ -> pure ()

-- | Truncate the entire scratchpad. The caller is expected to gate
-- this on an explicit @confirm=true@ flag at the tool layer; the data
-- layer trusts whoever called it.
clearAll :: Store -> IO ()
clearAll s = withGlobalScratchLock (sFile s) $ withMVar (sLock s) $ \_ -> do
  createDirectoryIfMissing True (takeDirectory (sFile s))
  encodeFile (sFile s) ([] :: [ScratchEntry])
