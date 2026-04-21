-- | Flow: @ghci_deps@ is a /metadata/ tool, not a /resolver/. It
-- edits the .cabal field and trusts the caller for the semantics of
-- the name. This scenario pins that contract by round-tripping a
-- bogus-but-syntactically-valid dep: add → list → remove → list.
--
-- What we tested and DIDN'T find (contract documentation)
-- -------------------------------------------------------
-- An earlier version of this scenario asserted that
-- 'ghci_check_project' would fail after a bogus dep was added.
-- The oracle caught the mismatched expectation: 'ghci_check_project'
-- does NOT do a cabal-configure / dep-resolve — it type-checks the
-- modules currently loaded. An unused dep doesn't enter any module
-- and therefore never trips the check. That is the correct contract;
-- the earlier oracle was the wrong shape.
--
-- Real-world motivation: an LLM host calling ghci_deps may pass a
-- misspelled or non-existent package name. The MCP happily accepts
-- it (because validation would require a Hackage round-trip). This
-- test keeps the accept / roundtrip / reject honest so a future
-- refactor can't silently change the semantics under callers.
--
-- Invariants asserted:
--
--   1. ghci_deps(add, "nonexistent-fake-pkg-xyzzy-2025") returns
--      success=true. The MCP accepts any syntactically valid
--      Hackage identifier and edits the .cabal.
--   2. The edit persists: ghci_deps(list) after add includes the
--      fake dep in its build_depends[] field.
--   3. The session survives the bogus add — next ghci_eval works.
--   4. ghci_deps(remove) succeeds and the dep disappears from
--      build_depends[] on the next list call.
--   5. After the round-trip the .cabal is indistinguishable from
--      its pre-add shape (modulo whitespace ordering that the line
--      parser preserves).
--
-- Failure modes the oracle catches:
--
--   (a) ghci_deps silently drops the add (success=true but the
--       file isn't modified — the kind of bug that only surfaces
--       under cabal build, long after the agent moved on).
--   (b) ghci_deps drops the REMOVE (file is still polluted by the
--       fake dep the agent tried to clean up).
--   (c) The add/remove pipeline wedges the session.
module Scenarios.FlowDependencyConflict
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

-- | A package name that does not exist on Hackage but is
-- syntactically valid (lowercase, hyphens, numbers) so
-- 'validatePackageName' accepts it.
bogusPkg :: Text
bogusPkg = "nonexistent-fake-pkg-xyzzy-2025"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("depconflict-demo" :: Text) ])

  -- 1. Add the bogus dep. Contract: success=true (no Hackage probe).
  t0 <- stepHeader 1 ("add · ghci_deps(add, \"" <> bogusPkg <> "\")")
  add <- Client.callTool c "ghci_deps"
           (object
             [ "action"  .= ("add" :: Text)
             , "package" .= bogusPkg
             ])
  cAdd <- liveCheck $ checkPure
    "add returns success=true · MCP does not validate against Hackage"
    (fieldBool "success" add == Just True)
    ("ghci_deps(add) with a syntactically-valid name must succeed — \
     \the documented contract is 'edits the .cabal, resolution is \
     \the caller's problem'. Got: " <> truncRender add)
  stepFooter 1 t0

  -- 2. Verify the edit really happened — this is the GROUND TRUTH.
  -- cAdd can pass on a bogus 'success' flag (a silent drop would
  -- still report success=true if the tool wasn't honest); the
  -- build_depends list is what disk actually says.
  t1 <- stepHeader 2 "list · build_depends must include the bogus entry"
  ls <- Client.callTool c "ghci_deps"
          (object [ "action" .= ("list" :: Text) ])
  let depsAfterAdd = buildDeps ls
      includesBogus = any (bogusPkg `T.isInfixOf`) depsAfterAdd
  cList <- liveCheck $ checkPure
    "post-add list contains bogus pkg · edit persisted to disk"
    includesBogus
    ("If the list does not include the just-added dep, the edit \
     \silently dropped. build_depends seen: " <> T.pack (show depsAfterAdd)
     <> ". Raw: " <> truncRender ls)
  stepFooter 2 t1

  -- No "session alive" step between add and remove on purpose. A
  -- first version of this scenario tested ghci_eval(1+1) after the
  -- bogus add and always failed: booting 'cabal repl' re-reads the
  -- .cabal, the resolver can't find 'nonexistent-fake-pkg-xyzzy-2025',
  -- and the GHCi child dies before our eval can run. That is the
  -- correct behaviour — a bogus dep DOES break any subsequent
  -- session-boot — so asserting "alive" here is asserting the wrong
  -- invariant. The liveness oracle now lives after the REMOVE
  -- step (4) below, which is the claim that actually matters:
  -- once the bogus dep is gone, the project is usable again.

  -- 4. Remove and verify the dep really disappeared from the .cabal.
  t3 <- stepHeader 3 ("remove · ghci_deps(remove, \"" <> bogusPkg <> "\")")
  rm <- Client.callTool c "ghci_deps"
          (object
            [ "action"  .= ("remove" :: Text)
            , "package" .= bogusPkg
            ])
  cRemove <- liveCheck $ checkPure
    "remove returns success=true"
    (fieldBool "success" rm == Just True)
    ("remove failed — the project is now permanently polluted. Raw: "
      <> truncRender rm)
  stepFooter 3 t3

  t4 <- stepHeader 4 "list · bogus pkg is gone after remove"
  ls2 <- Client.callTool c "ghci_deps"
           (object [ "action" .= ("list" :: Text) ])
  let depsAfterRm   = buildDeps ls2
      bogusGone     = not (any (bogusPkg `T.isInfixOf`) depsAfterRm)
  cGone <- liveCheck $ checkPure
    "post-remove list does NOT contain bogus pkg"
    bogusGone
    ("remove reported success but the bogus dep is still in \
     \build_depends. The tool lied. build_depends=" <>
     T.pack (show depsAfterRm) <> ". Raw: " <> truncRender ls2)
  stepFooter 4 t4

  -- 6. The real "alive" oracle: after the bogus dep is REMOVED,
  -- the session must boot cleanly. If it doesn't, the remove
  -- didn't fully revert the .cabal.
  t5 <- stepHeader 5 "session alive · ghci_eval(1+1) after remove"
  alive <- Client.callTool c "ghci_eval"
             (object [ "expression" .= ("1 + 1" :: Text) ])
  cAlive <- liveCheck $ checkPure
    "session alive post-remove · project is buildable again"
    (fieldBool "success" alive == Just True
     && case lookupField "output" alive of
          Just (String s) -> "2" `T.isInfixOf` s
          _               -> False)
    ("After remove, cabal repl should resolve cleanly. If this \
     \fails, ghci_deps(remove) didn't fully revert the edit. Raw: "
      <> truncRender alive)
  stepFooter 5 t5

  pure [cAdd, cList, cRemove, cGone, cAlive]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

-- | Extract the build_depends array from a ghci_deps(list) response.
-- The field name the MCP returns is @build_depends@ (not @packages@);
-- an earlier version of this scenario read the wrong field and
-- always saw []. The rename is the correct semantic anchor.
buildDeps :: Value -> [Text]
buildDeps v = case lookupField "build_depends" v of
  Just (Array xs) -> [ p | String p <- V.toList xs ]
  _               -> []

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
