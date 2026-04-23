-- | Flow: @ghci_switch_project@ — runtime project switching.
--
-- Prior to this tool, 'HASKELL_PROJECT_DIR' was locked at server
-- boot. Every path-bound tool ('ghci_lint', 'ghci_load', …) refused
-- siblings with @"target path escapes project directory"@. The only
-- way to work on two projects in one conversation was to restart
-- the host — real friction in multi-project dogfood flows.
--
-- Contract asserted here
-- ----------------------
--
--   1. Switch succeeds for an absolute path to an existing cabal
--      project — response carries @previous@ + @current@.
--   2. Error paths (relative path, missing dir, dir without
--      @.cabal@) return @success: false@ with a readable @error@
--      and leave the server's project-dir untouched.
--   3. After a successful switch, subsequent tools operate on the
--      NEW project: 'ghci_add_modules' writes to the new
--      @.cabal@, not the old one.
--   4. Switching back restores the prior scope: the original
--      project's @.cabal@ still carries the modules we added
--      before the detour.
--
-- Non-goals
-- ---------
--
--   * We do NOT assert anything about the in-process GhcSession
--     being rebuilt — that's 'testSwitchHandleSwaps' in the unit
--     suite (at this layer the MVar is internal state).
module Scenarios.FlowSwitchProject
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

--------------------------------------------------------------------------------
-- sidecar project scaffolding
--------------------------------------------------------------------------------

-- | Minimal .cabal text — enough that 'validateSwitchTarget' sees a
-- cabal file and 'mkProjectDir' accepts the root. We don't need
-- the project to build; we just need 'ghci_add_modules' to be able
-- to find + rewrite the .cabal.
minimalCabal :: Text -> Text
minimalCabal pkg = T.unlines
  [ "cabal-version: 2.4"
  , "name: " <> pkg
  , "version: 0.1.0.0"
  , ""
  , "library"
  , "  hs-source-dirs:   src"
  , "  exposed-modules:"
  , "  build-depends:    base"
  , "  default-language: Haskell2010"
  ]

--------------------------------------------------------------------------------
-- flow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) scaffold project A in the scenario's own tempdir, add Foo
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold · project A (the scenario's own dir) + Foo"
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("switch-a" :: Text) ])
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Foo"] :: [Text]) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) switch errors — relative path, missing dir, no .cabal
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "errors · relative path / missing / no .cabal"
  relErr <- Client.callTool c "ghci_switch_project"
              (object [ "path" .= ("relative/path" :: Text) ])
  cErr1 <- liveCheck $ checkJsonField
    "relative path → success=false"
    relErr "success" (Bool False)
  cErr1Msg <- liveCheck $ checkJsonFieldMatches
    "relative path · error mentions 'absolute'"
    relErr "error" (containsCI "absolute")
    "The error payload should name the failure mode so a caller \
    \knows to send an absolute path on retry."

  missErr <- Client.callTool c "ghci_switch_project"
               (object
                 [ "path" .= ("/tmp/definitely-does-not-exist-xxxyyy" :: Text)
                 ])
  cErr2 <- liveCheck $ checkJsonField
    "missing dir → success=false"
    missErr "success" (Bool False)

  -- Create a bare dir (no .cabal inside) to exercise VENoCabalFile.
  let bareDir = projectDir </> "bare-no-cabal"
  createDirectoryIfMissing True bareDir
  bareErr <- Client.callTool c "ghci_switch_project"
               (object [ "path" .= T.pack bareDir ])
  cErr3 <- liveCheck $ checkJsonField
    "dir without .cabal → success=false"
    bareErr "success" (Bool False)
  cErr3Msg <- liveCheck $ checkJsonFieldMatches
    "no-cabal · error mentions '.cabal'"
    bareErr "error" (containsCI ".cabal")
    "Error must explain the failure — 'no .cabal file found' — so \
    \callers don't guess at what went wrong."
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) scaffold project B as a sibling of A
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "sidecar · create project B in a sibling dir"
  let projB = projectDir </> "project-b"
  createDirectoryIfMissing True projB
  TIO.writeFile (projB </> "switch-b.cabal") (minimalCabal "switch-b")
  TIO.writeFile (projB </> "cabal.project") "packages: .\n"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (4) switch TO B — verify previous/current + workflow status
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "switch · A → B"
  switchAB <- Client.callTool c "ghci_switch_project"
                (object [ "path" .= T.pack projB ])
  cSwitchOk <- liveCheck $ checkJsonField
    "switch A→B · success=true"
    switchAB "success" (Bool True)
  cSwitchPrev <- liveCheck $ checkJsonFieldMatches
    "switch A→B · previous points at the scenario dir"
    switchAB "previous" (pathEq projectDir)
    "The 'previous' field should carry the old projectDir so the \
    \caller can roll back or log the swap."
  cSwitchCur <- liveCheck $ checkJsonFieldMatches
    "switch A→B · current points at project-b"
    switchAB "current" (pathEq projB)
    "The 'current' field should carry the new projectDir verbatim \
    \so the caller has an anchor for subsequent tool calls."
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- (5) after switch: operations hit project B, not A
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "post-switch · ghci_add_modules targets project-b"
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Bar"] :: [Text]) ])
  bCabal <- TIO.readFile (projB </> "switch-b.cabal")
  cBHasBar <- liveCheck $ checkPure
    "project-b's .cabal now lists Bar in exposed-modules"
    ("Bar" `T.isInfixOf` bCabal)
    ("expected 'Bar' in project-b's .cabal, got:\n" <> bCabal)

  -- And A's .cabal must be untouched — the add targeted B.
  aCabal <- TIO.readFile (projectDir </> "switch-a.cabal")
  cANoBar <- liveCheck $ checkPure
    "project-a's .cabal does NOT list Bar (mutation stayed in B)"
    (not ("Bar" `T.isInfixOf` aCabal))
    ("Bar leaked into project-a's .cabal — switch didn't actually \
     \isolate state. Contents:\n" <> aCabal)
  stepFooter 5 t4

  ----------------------------------------------------------------
  -- (6) switch BACK to A — original state survives
  ----------------------------------------------------------------
  t5 <- stepHeader 6 "switch · B → A and verify Foo still registered"
  switchBA <- Client.callTool c "ghci_switch_project"
                (object [ "path" .= T.pack projectDir ])
  cSwitchBackOk <- liveCheck $ checkJsonField
    "switch B→A · success=true"
    switchBA "success" (Bool True)
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Baz"] :: [Text]) ])
  aCabal2 <- TIO.readFile (projectDir </> "switch-a.cabal")
  cAHasBaz <- liveCheck $ checkPure
    "project-a is writable again · Baz lands here, not in B"
    ("Baz" `T.isInfixOf` aCabal2)
    ("expected 'Baz' in project-a's .cabal after switching back. \
     \Contents:\n" <> aCabal2)
  bCabal2 <- TIO.readFile (projB </> "switch-b.cabal")
  cBNoBaz <- liveCheck $ checkPure
    "project-b's .cabal does NOT list Baz (switch back was effective)"
    (not ("Baz" `T.isInfixOf` bCabal2))
    ("Baz leaked into project-b — the back-switch didn't take. \
     \Contents:\n" <> bCabal2)
  stepFooter 6 t5

  pure
    [ cErr1, cErr1Msg, cErr2, cErr3, cErr3Msg
    , cSwitchOk, cSwitchPrev, cSwitchCur
    , cBHasBar, cANoBar
    , cSwitchBackOk, cAHasBaz, cBNoBaz
    ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Case-insensitive substring match — keeps error-message
-- assertions tolerant of exact wording.
containsCI :: Text -> Value -> Bool
containsCI needle (String s) =
  T.toLower needle `T.isInfixOf` T.toLower s
containsCI _ _ = False

-- | Path equality that tolerates macOS's @/var@ ↔ @/private/var@
-- symlink aliasing. We compare the suffix after dropping that
-- prefix on either side, which is enough to assert "same
-- directory" without booting a real 'canonicalizePath'.
pathEq :: FilePath -> Value -> Bool
pathEq expected (String actual) =
  let e = normalize (T.pack expected)
      a = normalize actual
  in e == a
  where
    normalize t
      | "/private/var/" `T.isPrefixOf` t = T.drop 8 t
      | "/private/tmp/" `T.isPrefixOf` t = T.drop 8 t
      | otherwise                        = t
pathEq _ _ = False
