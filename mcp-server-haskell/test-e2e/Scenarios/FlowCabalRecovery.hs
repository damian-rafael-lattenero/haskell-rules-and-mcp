-- | Flow: external @.cabal@ edit-and-restore is picked up by
-- non-load tools without an intervening @ghc_load@ (#49).
--
-- Pre-fix behaviour
-- -----------------
-- 'ensureStanzaFlags' was only invoked from 'loadForTarget'.
-- Tools that took the bare 'withGhcSession' path
-- ('ghc_type', 'ghc_eval', 'ghc_info', 'ghc_complete', …) kept
-- serving the boot-time stanza flags after an external @.cabal@
-- edit, so a corruption-and-restore via the host's filesystem
-- left the session stuck in a stale state until the operator
-- happened to call 'ghc_load'.
--
-- New contract
-- ------------
-- 'withGhcSession' calls 'ensureStanzaFlags' on entry. The
-- mtime check inside short-circuits when nothing changed (one
-- stat + one IORef read) and re-bootstraps when the file moved.
-- Every tool benefits.
module Scenarios.FlowCabalRecovery
  ( runFlow
  ) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold a real cabal project so we have a
  -- well-formed .cabal to corrupt + restore.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("cabal-recover" :: Text) ])

  -- Step 2 — exercise a non-load tool (ghc_eval) BEFORE the
  -- corruption so we know the boot-time bootstrap finished.
  t0 <- stepHeader 1 "baseline · ghc_eval succeeds before tampering"
  base <- Client.callTool c GhcEval
            (object [ "expression" .= ("1 + 1" :: Text) ])
  cBase <- liveCheck $ checkPure
    "baseline ghc_eval(1+1) succeeds"
    (statusOk base == Just True)
    ("Expected baseline eval to succeed; got: " <> truncRender base)
  stepFooter 1 t0

  -- Step 3 — locate the project's .cabal file (we don't know
  -- the package name's exact filename, but conventionally it
  -- matches @<name>.cabal@) and overwrite it externally with
  -- garbage. Pre-#49 this poisoned the stanza flags for any
  -- subsequent non-load tool.
  cabalPath <- findCabalFile projectDir
  origBody  <- TIO.readFile cabalPath

  -- Sleep > 1 s so the on-disk mtime strictly advances on the
  -- next write; macOS HFS / APFS clock-resolves to 1 second.
  threadDelay 1_100_000
  TIO.writeFile cabalPath "garbage line that is not a valid cabal stanza\n"

  -- Step 4 — restore the original cabal body so the project
  -- is buildable again. mtime advances again.
  threadDelay 1_100_000
  TIO.writeFile cabalPath origBody

  -- Step 5 — call ghc_eval (NOT ghc_load). Pre-#49 this would
  -- fail with stale "module not loaded" errors because the
  -- session still held the boot-time flags + the failed-load
  -- state from the garbage version. Post-fix, ensureStanzaFlags
  -- runs at withGhcSession entry, picks up the latest mtime,
  -- re-bootstraps cleanly.
  t1 <- stepHeader 2 "recovery · ghc_eval after corrupt+restore (#49)"
  recovered <- Client.callTool c GhcEval
                 (object [ "expression" .= ("2 + 2" :: Text) ])
  let okShape = statusOk recovered == Just True
      hasFour = case lookupField "output" recovered of
                  Just (String s) -> "4" `T.isInfixOf` s
                  _               -> False
  cRecover <- liveCheck $ checkPure
    "ghc_eval(2+2) succeeds AND returns '4' after corrupt+restore"
    (okShape && hasFour)
    ( "Expected: success=true, output contains '4'. \
      \Got: " <> truncRender recovered )
  stepFooter 2 t1

  pure [cBase, cRecover]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

findCabalFile :: FilePath -> IO FilePath
findCabalFile root = do
  entries <- listDirectory root
  case [ root </> e | e <- entries, takeExtension e == ".cabal" ] of
    (p : _) -> pure p
    []      -> error ("FlowCabalRecovery: no .cabal in " <> root)

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
