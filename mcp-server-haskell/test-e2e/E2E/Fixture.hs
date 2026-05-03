-- | Fixture-based scenario startup.
--
-- Most E2E scenarios begin with the same boilerplate:
--
-- @
--   _ <- Client.callTool c GhcProject (object [ "action" .= "create", ... ])
--   _ <- Client.callTool c GhcModules (object [ "action" .= "add", ... ])
--   _ <- Client.callTool c GhcDeps    (object [ "action" .= "add", "package" .= "QuickCheck", ... ])
--   _ <- Client.callTool c GhcLoad    ...
-- @
--
-- The first @ghc_load@ on a freshly-scaffolded project triggers
-- @cabal v2-repl@ which (a) runs the dependency solver and (b)
-- compiles the project's library. Both steps are amortizable across
-- scenarios that share a dependency set: each fresh project
-- re-resolves the same QuickCheck/aeson closure.
--
-- This module exposes a fixture-copy helper so a scenario can
-- start from a pre-built project tree at
-- @test-e2e/Fixtures/<Name>/@ instead of scaffolding from scratch.
-- The CI step "Pre-warm fixtures" runs @cabal build@ once per
-- fixture so its dependency closure lands in
-- @~/.cabal/store/@ before the e2e test job starts. Scenarios
-- that copy the fixture see a warm store and skip the
-- solver+compile round-trip.
--
-- == Use
--
-- > import qualified E2E.Fixture as Fixture
-- >
-- > runFlow c projectDir = do
-- >   Fixture.copyBaselineInto projectDir
-- >   -- write scenario-specific source under src/Foo.hs
-- >   -- call ghc_load src/Foo.hs (warm)
--
-- == Non-goal
--
-- This is opt-in. Scenarios that exercise @ghc_project create@,
-- @ghc_deps add@, or @ghc_modules add@ as part of their assertion
-- surface MUST keep scaffolding from scratch — those tools are
-- under test.
module E2E.Fixture
  ( -- * Generic copy
    copyFixture
    -- * Convenience wrappers
  , copyBaselineInto
    -- * Path helpers
  , fixtureRoot
  ) where

import Control.Exception (IOException, throwIO, try)
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))

-- | Repository-relative root for fixture trees. Test-e2e Main.hs
-- runs with the package directory as CWD, so relative paths under
-- @test-e2e/Fixtures/@ resolve correctly when the cabal test
-- target invokes us. CI also uses the package directory as
-- working-directory, so this same path works there.
fixtureRoot :: FilePath
fixtureRoot = "test-e2e" </> "Fixtures"

-- | Recursively copy a fixture tree into the scenario's tmpdir.
--
-- The fixture is identified by name (the directory name under
-- 'fixtureRoot'). Every regular file and subdirectory is copied
-- preserving the relative layout. Symlinks and special files are
-- dereferenced; we don't expect those in fixtures.
--
-- Throws an 'IOError' with a human-readable message if the
-- fixture directory does not exist — the caller almost certainly
-- typoed the name and we want a loud failure rather than a silent
-- empty copy.
copyFixture
  :: FilePath  -- ^ fixture name (e.g. \"Baseline\")
  -> FilePath  -- ^ destination directory (must already exist)
  -> IO ()
copyFixture name dest = do
  let src = fixtureRoot </> name
  exists <- Dir.doesDirectoryExist src
  if not exists
    then throwIO . userError $
      "E2E.Fixture.copyFixture: fixture not found at " <> src
        <> ". Available fixtures: " <> show fixtureRoot
    else do
      Dir.createDirectoryIfMissing True dest
      copyDirectoryRecursive src dest

-- | Convenience: copy the standard baseline (lib + QuickCheck +
-- common deps) into the scenario's tmpdir. Most scenarios that
-- need a starting cabal layout will use this.
copyBaselineInto :: FilePath -> IO ()
copyBaselineInto = copyFixture "Baseline"

--------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------

-- | Naive recursive copy. We avoid the @directory >= 1.3.7@
-- 'Dir.copyFileWithMetadata' for portability; the older API
-- ('Dir.copyFile') is enough for fixtures (which never carry
-- mode bits or extended attributes that matter).
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  Dir.createDirectoryIfMissing True dst
  entries <- Dir.listDirectory src
  mapM_ (copyOne src dst) entries

copyOne :: FilePath -> FilePath -> FilePath -> IO ()
copyOne src dst entry = do
  let s = src </> entry
      d = dst </> entry
  isDir <- Dir.doesDirectoryExist s
  if isDir
    then copyDirectoryRecursive s d
    else do
      -- Best-effort copy — swallow IOException with a wrapped
      -- userError so the scenario reports something readable
      -- rather than a bare 'permission denied' from deep in the
      -- runtime.
      r <- try (Dir.copyFile s d) :: IO (Either IOException ())
      case r of
        Right () -> pure ()
        Left e   -> throwIO . userError $
          "E2E.Fixture: failed to copy " <> s <> " -> " <> d
            <> ": " <> T.unpack (T.pack (show e))
