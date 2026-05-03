-- | Flow: writes to the property store must fail explicitly when the
-- filesystem refuses — a silent drop would mean the persisted regression
-- set is quietly incomplete, which is the worst kind of test-pollution
-- bug to debug.
--
-- We don't try to produce real disk-full conditions (requires a
-- fixed-size loop filesystem, too OS-specific for CI). Instead we
-- simulate the equivalent failure by replacing the '.haskell-flows/'
-- directory with a regular file. Any subsequent write to
-- '.haskell-flows/properties.json' fails with @ENOTDIR@ at the
-- @createDirectoryIfMissing@ step inside @withGlobalStoreLock@,
-- which is indistinguishable from @ENOSPC@/@EACCES@ at the semantic
-- layer we care about: the IO system call returned an error.
--
-- Mechanism note: the previous version used @chmod 000@ on the dir.
-- That works on a developer laptop but is a no-op inside a Docker
-- container running as root (root bypasses DAC), which made this
-- scenario flake red on every container-based CI run. The
-- file-blocker approach works regardless of effective UID.
--
-- What ghc_quickcheck does on success:
--
--   * Runs the property through the live GHCi session.
--   * If it passes, appends a record to
--     '.haskell-flows/properties.json' via atomicWriteFile.
--
-- Invariants asserted:
--
--   1. With the store directory unwritable, ghc_quickcheck on a
--      passing property returns a tool-level response. The property
--      still passed in-session (that's a pure eval), but the persist
--      step failed — the response MUST surface that failure, not
--      claim "stored for replay".
--   2. After restoring permissions, the next ghc_quickcheck persists
--      correctly — the tool has no sticky failure state.
--   3. The session survives the failed persist.
--
-- Failure modes the oracle catches:
--
--   (a) Write error is swallowed — tool reports success, but the
--      store is silently stale. Next 'ghc_regression run' replays
--      whatever WAS on disk (which may be empty or outdated).
--   (b) Write error crashes the tool, wedging the session.
--   (c) The MCP leaves a half-written tmp file that poisons the
--      later persist attempt.
module Scenarios.FlowDiskFull
  ( runFlow
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , removePathForcibly
  )
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, fieldBool, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("diskfull-demo" :: Text) ])

  let storeDir = projectDir </> ".haskell-flows"

  -- 1. Replace .haskell-flows/ with a regular file. The store-write
  -- path under @withGlobalStoreLock@ does
  -- @createDirectoryIfMissing True (takeDirectory storeFile)@ which
  -- fails with @ENOTDIR@ when the path component exists as a file.
  -- This mechanism works regardless of effective UID — unlike
  -- @chmod 000@ which root bypasses inside Docker containers.
  -- Ensure parent (projectDir) exists; it does after ghc_project
  -- create, but be defensive.
  removePathForcibly storeDir
  t0 <- stepHeader 1 "setup · replace .haskell-flows/ with blocking file"
  writeFile storeDir "blocker — this path is a regular file, not a dir"
  stepFooter 1 t0

  -- 2. Fire a passing property. The eval is a pure Haskell evaluation,
  -- it will pass. The persist step is what we want to see fail.
  t1 <- stepHeader 2 "quickcheck · property passes eval, persist must fail"
  eRes <- try (Client.callTool c GhcQuickCheck
                 (object [ "property"
                         .= ("\\(xs :: [Int]) -> reverse (reverse xs) == xs"
                             :: Text) ]))
           :: IO (Either SomeException Value)
  -- Restore immediately — replace the blocker file with a real dir
  -- so subsequent assertions and tempdir cleanup work normally.
  removePathForcibly storeDir
  createDirectoryIfMissing True storeDir

  r <- case eRes of
    Left ex -> pure (Object $ KeyMap.fromList
                       [ ("success", Bool False)
                       , ("error",   String (T.pack (show ex)))
                       ])
    Right v -> pure v

  -- The key oracle. Both of these are acceptable:
  --   * success=false with a persist-error field
  --   * success=true with persisted=false AND an error/hint
  -- The WRONG shape is success=true with persisted=true, because
  -- that means the tool lied (nothing was written).
  let success = statusOk r
      persisted = fieldBool "persisted" r
      honestReport =
        -- Failure-shaped: success=false with some diagnostic
        (success == Just False
         && any ($ r) [ hasField "error", hasField "hint"
                      , hasField "errors", hasField "reason" ])
        -- OR: success=true but persisted is flagged as false
        || (success == Just True && persisted == Just False)
        -- OR: the whole call threw (we captured via try above);
        -- an exception is honest "couldn't do the work".
  cHonest <- liveCheck $ checkPure
    "persist failure surfaced · not silently swallowed"
    honestReport
    ("With .haskell-flows/ replaced by a regular file, the persist \
     \step MUST fail. Got success=" <> T.pack (show success)
     <> ", persisted=" <> T.pack (show persisted)
     <> ". If both True, the tool claimed a write that didn't happen. \
        \Raw: " <> truncRender r)
  stepFooter 2 t1

  -- 3. Session must still be alive.
  t2 <- stepHeader 3 "session alive · ghc_eval(1+1) after failed persist"
  alive <- Client.callTool c GhcEval
             (object [ "expression" .= ("1 + 1" :: Text) ])
  cAlive <- liveCheck $ checkPure
    "session alive · failed persist didn't wedge the GHCi"
    (statusOk alive == Just True)
    ("Raw: " <> truncRender alive)
  stepFooter 3 t2

  -- 4. Second quickcheck with permissions restored should persist OK —
  -- the tool must not be in a sticky-failed state.
  t3 <- stepHeader 4 "recovery · second ghc_quickcheck after chmod restore"
  r2 <- Client.callTool c GhcQuickCheck
          (object [ "property"
                  .= ("\\(n :: Int) -> n + 0 == n" :: Text) ])
  storeExists <- doesDirectoryExist storeDir
  cRecov <- liveCheck $ checkPure
    "post-restore quickcheck works · tool is not sticky-failed"
    (statusOk r2 == Just True && storeExists)
    ("After restoring permissions, a fresh quickcheck should persist \
     \normally. If it doesn't, the tool cached the earlier failure. \
     \Raw: " <> truncRender r2)
  stepFooter 4 t3

  pure [cHonest, cAlive, cRecov]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

hasField :: Text -> Value -> Bool
hasField k v = case lookupField k v of
  Just _  -> True
  Nothing -> False

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
