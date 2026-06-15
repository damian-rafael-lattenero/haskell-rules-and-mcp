-- | Unit tests for 'Tool.SwitchProject' validation and handle-level
-- store/scratchpad reopening, plus 'parseCabalNameField' and
-- 'detectSelfProject'.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.SwitchProjectUnit
  ( testSwitchRejectsRelative
  , testSwitchRejectsMissing
  , testSwitchRejectsNoCabal
  , testSwitchAcceptsValid
  , testSwitchHandleSwaps
  , testSwitchHandleReopensStore
  , testSwitchHandleReopensScratchpad
  , testParseCabalNameField
  , testDetectSelfProjectPositive
  , testDetectSelfProjectNegative
  , testDetectSelfProjectMissing
  ) where

import Data.IORef (newIORef, readIORef)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.SelfProject as SelfProject
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import HaskellFlows.Types (mkProjectDir, PathError (..))
import HaskellFlows.Data.PropertyStore (openStore, loadAll, save)
import qualified HaskellFlows.Data.Scratchpad as SP
import HaskellFlows.Tool.SwitchProject (ValidationError (..), validateSwitchTarget)
import qualified HaskellFlows.Tool.SwitchProject as SwitchProject

import Control.Concurrent (newMVar, readMVar)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified HaskellFlows.Types

-- | Build a tempdir-scoped project with the given name and .cabal
-- file. Returns the absolute path. Caller owns cleanup.
scaffoldTmpProject :: String -> IO FilePath
scaffoldTmpProject tag = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-" <> tag <> "-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> (tag <> ".cabal")) $ T.unlines
    [ "cabal-version: 2.4"
    , "name: " <> T.pack tag
    , "version: 0.1.0.0"
    , ""
    , "library"
    , "  build-depends: base"
    , "  default-language: Haskell2010"
    ]
  pure dir

-- | Relative paths must be rejected before touching the filesystem —
-- 'mkProjectDir' is the guard; 'validateSwitchTarget' surfaces it as
-- 'VEPathError'.
testSwitchRejectsRelative :: IO Bool
testSwitchRejectsRelative = do
  res <- validateSwitchTarget "relative/path"
  pure $ case res of
    Left (VEPathError (PathNotAbsolute _)) -> True
    _                                      -> False

-- | Absolute but non-existent path → 'VENotADirectory'. Using a
-- time-stamped path guarantees we don't collide with a real dir on
-- the test machine.
testSwitchRejectsMissing :: IO Bool
testSwitchRejectsMissing = do
  ts <- getPOSIXTime
  let bogus = "/tmp/definitely-missing-"
                <> show (floor (ts * 1000000) :: Int)
  res <- validateSwitchTarget (T.pack bogus)
  pure $ case res of
    Left (VENotADirectory _) -> True
    _                        -> False

-- | Real dir with no .cabal file → 'VENoCabalFile'. Scaffolds a
-- bare tempdir, runs the validator, cleans up regardless.
testSwitchRejectsNoCabal :: IO Bool
testSwitchRejectsNoCabal = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("no-cabal-" <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  -- A NON-empty directory without a .cabal stays rejected — we
  -- don't want to accidentally point at @~/Downloads@ or similar
  -- and have subsequent tools treat its contents as sources.
  -- (Empty dirs are allowed post-BUG-PLUS-07; see
  -- 'testSwitchAcceptsEmpty'.)
  TIO.writeFile (dir </> "README.md") "not a cabal project\n"
  res <- validateSwitchTarget (T.pack dir)
  removePathForcibly dir
  pure $ case res of
    Left (VENoCabalFile _) -> True
    _                      -> False

-- | Happy path: a real cabal project returns 'Right ProjectDir'
-- pointing at the scaffolded dir.
testSwitchAcceptsValid :: IO Bool
testSwitchAcceptsValid = do
  dir <- scaffoldTmpProject "sp-happy"
  res <- validateSwitchTarget (T.pack dir)
  removePathForcibly dir
  pure $ case res of
    Right pd -> HaskellFlows.Types.unProjectDir pd == dir
    _        -> False

-- | End-to-end contract of the 'handle' function: after it returns
-- with success, the project-dir IORef points at the new path AND
-- the session MVar is emptied (Nothing) so the next
-- getOrStartGhcSession boots fresh.
testSwitchHandleSwaps :: IO Bool
testSwitchHandleSwaps = do
  dirA <- scaffoldTmpProject "from"
  dirB <- scaffoldTmpProject "to"
  case (mkProjectDir dirA, mkProjectDir dirB) of
    (Right pdA, Right pdB) -> do
      pdRef    <- newIORef pdA
      sessRef  <- newMVar Nothing
      storeA   <- openStore pdA
      storeRef <- newIORef storeA
      -- Prime the session so we can observe the kill semantics:
      -- handle must wipe whatever Session was there.
      primed   <- startGhcSession pdA
      _        <- readMVar sessRef
      sessRef' <- newMVar (Just primed)
      -- PR-4: SwitchProject.handle gained an IORef Bool for the
      -- self-project flag. Synthetic /tmp targets are never self.
      -- F-02: SwitchProject.handle also gained an IORef Scratchpad.Store.
      scratchA   <- SP.openStore pdA
      scratchRef <- newIORef scratchA
      selfRef    <- newIORef False
      let args = A.object [ "path" A..= T.pack dirB ]
      result  <- SwitchProject.handle pdRef sessRef' storeRef scratchRef selfRef args
      newPd   <- readIORef pdRef
      mSess   <- readMVar sessRef'
      removePathForcibly dirA
      removePathForcibly dirB
      pure
        ( Env.reStatus result == Env.StatusOk
            && HaskellFlows.Types.unProjectDir newPd ==
                 HaskellFlows.Types.unProjectDir pdB
            && isNothing mSess
        )
    _ -> do
      removePathForcibly dirA
      removePathForcibly dirB
      pure False

-- | Issue #39: a successful 'switch_project' must atomically
-- swap the property store ref so subsequent 'loadAll' goes
-- against the NEW project's @.haskell-flows/properties.json@.
-- Pre-fix the ref kept pointing at the boot-time store, so a
-- property saved into A leaked into B's regression list.
--
-- Setup:
--   * Project A scaffolded + a single property saved to its store.
--   * Project B scaffolded with NO properties.
--   * SwitchProject.handle (A → B).
-- Assertion:
--   * The post-switch storeRef opened against B reads as empty.
--   * The pre-switch storeRef (storeA, captured before the swap)
--     still reads A's property — proves we returned a NEW
--     'Store' instead of mutating the old one in place (which
--     would have been correct but fragile).
testSwitchHandleReopensStore :: IO Bool
testSwitchHandleReopensStore = do
  dirA <- scaffoldTmpProject "with-prop"
  dirB <- scaffoldTmpProject "no-prop"
  case (mkProjectDir dirA, mkProjectDir dirB) of
    (Right pdA, Right pdB) -> do
      pdRef    <- newIORef pdA
      sessRef  <- newMVar Nothing
      storeA   <- openStore pdA
      save storeA "\\x -> x == (x :: Int)" (Just "src/Foo.hs")
      preProps <- loadAll storeA
      storeRef <- newIORef storeA
      -- PR-4: SwitchProject.handle gained an IORef Bool for the
      -- self-project flag. Synthetic /tmp targets are never self.
      -- F-02: SwitchProject.handle also gained an IORef Scratchpad.Store.
      scratchA   <- SP.openStore pdA
      scratchRef <- newIORef scratchA
      selfRef    <- newIORef False
      let args = A.object [ "path" A..= T.pack dirB ]
      _ <- SwitchProject.handle pdRef sessRef storeRef scratchRef selfRef args
      storeAfter  <- readIORef storeRef
      postProps   <- loadAll storeAfter
      -- After the swap, the OLD Store handle should still point
      -- at A's file (immutability invariant) — read it again to
      -- confirm A's property wasn't somehow purged.
      stillInA    <- loadAll storeA
      removePathForcibly dirA
      removePathForcibly dirB
      pure
        ( length preProps  == 1
            && null postProps
            && length stillInA == 1
        )
    _ -> do
      removePathForcibly dirA
      removePathForcibly dirB
      pure False

-- | F-02 regression: a successful 'switch_project' must atomically
-- swap the scratchpad IORef so subsequent 'ghc_scratch' calls go
-- against the NEW project's @.haskell-flows/scratchpad.json@.
-- Pre-fix the ref kept pointing at the boot-time scratchpad, so
-- scratch entries saved into project A leaked into project B's list.
--
-- Setup:
--   * Project A scaffolded + a single scratch entry saved to its store.
--   * Project B scaffolded with NO scratch entries.
--   * SwitchProject.handle (A → B).
-- Assertion:
--   * The post-switch scratchRef opened against B reads as empty.
--   * The pre-switch scratchA handle still reads A's entry (immutability
--     invariant — we returned a new Store, not mutated the old one).
testSwitchHandleReopensScratchpad :: IO Bool
testSwitchHandleReopensScratchpad = do
  dirA <- scaffoldTmpProject "scratch-from"
  dirB <- scaffoldTmpProject "scratch-to"
  case (mkProjectDir dirA, mkProjectDir dirB) of
    (Right pdA, Right pdB) -> do
      pdRef      <- newIORef pdA
      sessRef    <- newMVar Nothing
      storeA     <- openStore pdA
      storeRef   <- newIORef storeA
      scratchA   <- SP.openStore pdA
      scratchRef <- newIORef scratchA
      selfRef    <- newIORef False
      -- Save one entry into project A's scratchpad
      let entry = SP.ScratchEntry
            { SP.seId      = "f02-probe"
            , SP.seKind    = SP.ScratchHypothesis
            , SP.seCode    = "1 + 1"
            , SP.seModule  = Nothing
            , SP.seImports = []
            , SP.seNote    = Nothing
            , SP.seResult  = Nothing
            , SP.seStatus  = SP.ScratchOpen
            , SP.seCreated = 0
            , SP.seUpdated = 0
            }
      SP.save scratchA entry
      preEntries <- SP.loadAll scratchA
      -- Switch to project B (use pdB-derived path to suppress unused-match)
      let args = A.object [ "path" A..= T.pack (HaskellFlows.Types.unProjectDir pdB) ]
      _ <- SwitchProject.handle pdRef sessRef storeRef scratchRef selfRef args
      -- Read the new scratchpad via the swapped ref
      scratchAfter  <- readIORef scratchRef
      postEntries   <- SP.loadAll scratchAfter
      -- A's scratch still intact (immutability invariant)
      stillInA      <- SP.loadAll scratchA
      removePathForcibly dirA
      removePathForcibly dirB
      pure
        ( length preEntries  == 1
            && null postEntries
            && length stillInA == 1
        )
    _ -> do
      removePathForcibly dirA
      removePathForcibly dirB
      pure False

--------------------------------------------------------------------------------
-- PR-4: SelfProject + dogfood-hint tests
--------------------------------------------------------------------------------

-- | Pure parser test: 'parseCabalNameField' handles canonical input,
-- whitespace, case-insensitive 'name', and ignores other fields.
testParseCabalNameField :: IO Bool
testParseCabalNameField = pure $ and
  [ SelfProject.parseCabalNameField "name: haskell-flows-mcp"
      == Just "haskell-flows-mcp"
  , SelfProject.parseCabalNameField "  name:   haskell-flows-mcp  "
      == Just "haskell-flows-mcp"
  , SelfProject.parseCabalNameField "Name: foo"
      == Just "foo"
  , SelfProject.parseCabalNameField "version: 0.1\nname: bar\nbuild-depends: base"
      == Just "bar"
  , isNothing (SelfProject.parseCabalNameField "no name here")
  , isNothing (SelfProject.parseCabalNameField "")
  ]

-- | Helper for SelfProject tests: write a cabal at the given path
-- with the given 'name:' value plus minimal valid metadata.
writeCabalNamed :: FilePath -> Text -> IO ()
writeCabalNamed path n = TIO.writeFile path $ T.unlines
  [ "cabal-version: 2.4"
  , "name: " <> n
  , "version: 0.1.0.0"
  , ""
  , "library"
  , "  build-depends: base"
  , "  default-language: Haskell2010"
  ]

-- | Positive: a tmp dir whose cabal file is named haskell-flows-mcp.cabal
-- AND declares 'name: haskell-flows-mcp' is recognised as self.
testDetectSelfProjectPositive :: IO Bool
testDetectSelfProjectPositive = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-self-pos-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  writeCabalNamed (dir </> "haskell-flows-mcp.cabal") SelfProject.selfCabalName
  result <- case mkProjectDir dir of
    Right pd -> SelfProject.detectSelfProject pd
    Left _   -> pure False
  removePathForcibly dir
  pure result

-- | Negative: a tmp dir whose cabal declares a different name returns
-- False — alt-named forks are "not self" by design.
testDetectSelfProjectNegative :: IO Bool
testDetectSelfProjectNegative = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-self-neg-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  writeCabalNamed (dir </> "haskell-flows-mcp.cabal") "other-package"
  result <- case mkProjectDir dir of
    Right pd -> SelfProject.detectSelfProject pd
    Left _   -> pure True   -- treat as failed setup
  removePathForcibly dir
  pure (not result)

-- | Missing cabal file collapses to False — the detector must err on
-- the side of "not self" so the dogfood prompt doesn't fire on a
-- random / empty / unrelated project.
testDetectSelfProjectMissing :: IO Bool
testDetectSelfProjectMissing = do
  base <- getTemporaryDirectory
  ts   <- getPOSIXTime
  let dir = base </> ("sp-self-missing-"
                       <> show (floor (ts * 1000000) :: Int))
  createDirectoryIfMissing True dir
  -- no cabal file written — empty dir
  result <- case mkProjectDir dir of
    Right pd -> SelfProject.detectSelfProject pd
    Left _   -> pure True
  removePathForcibly dir
  pure (not result)
