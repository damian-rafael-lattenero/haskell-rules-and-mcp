-- | Flow: @ghc_deps(add)@ verifies the new dep set with @cabal
-- v2-build --dry-run --only-dependencies@ before persisting (#48).
-- The scenario pins both halves of the contract:
--
--   * /Bogus/ package (syntactically valid identifier, not on
--     Hackage) → tool returns @success=false@ with
--     @error_kind=\"unresolvable_dep\"@, the @.cabal@ is rolled
--     back to its pre-edit state, the session boots cleanly on
--     the next call.
--   * /Valid/ package (boot library, always resolvable) → tool
--     returns @success=true@, the dep lands in @build_depends@,
--     subsequent @ghc_deps(remove)@ cleans it up.
--
-- Pre-#48 behaviour
-- -----------------
-- The previous version of this scenario asserted that
-- @ghc_deps(add)@ on a non-existent package returned @success=true@
-- (the historical contract: \"edits the .cabal, resolution is the
-- caller's problem\"). Issue #48 flipped that contract: agents
-- treat @success=true@ as \"this dep is usable now\", so reporting
-- success on an unresolvable add was an API lie that wasted
-- downstream round-trips.
--
-- The new contract is enforced by 'verifyAndCommit' in
-- 'HaskellFlows.Tool.Deps' which spawns
-- @cabal v2-build all --dry-run --only-dependencies@ after every
-- @add@ and rolls back the @.cabal@ on solver failure.
--
-- Failure modes the oracle catches
-- --------------------------------
--   (a) verifyAndCommit silently skips the verify step → bogus
--       add returns success=true again (regression).
--   (b) Verify runs but rollback doesn't write → @.cabal@ is left
--       polluted; the post-reject @list@ still contains the bogus
--       pkg.
--   (c) Verify runs, rollback works, but @error_kind@ is missing
--       from the response → agents that match on the kind to
--       distinguish unresolvable-dep from generic edit failure
--       can't.
--   (d) Verify rejects a /valid/ package (false positive in the
--       cabal output parser) → happy-path add of a boot library
--       fails.
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
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | A package name that is syntactically valid (lowercase, hyphens,
-- digits) so 'validatePackageName' accepts it but does not exist
-- on Hackage. The cabal solver is the only authority that can
-- detect this.
bogusPkg :: Text
bogusPkg = "nonexistent-fake-pkg-xyzzy-2025"

-- | A package name guaranteed to resolve under any modern GHC +
-- cabal install. @mtl@ ships with every GHC bindist in the boot
-- library set, so this leg of the test never depends on Hackage
-- network access.
validPkg :: Text
validPkg = "mtl"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("depconflict-demo" :: Text) ])

  -- 1. Bogus add must be REJECTED with rollback (#48).
  t0 <- stepHeader 1
          ("reject · ghc_deps(add, \"" <> bogusPkg <> "\") rolled back")
  add <- Client.callTool c GhcDeps
           (object
             [ "action"  .= ("add" :: Text)
             , "package" .= bogusPkg
             ])
  let rejected      = fieldBool "success" add == Just False
      kindIsUnres   = fieldText "error_kind" add == Just "unresolvable_dep"
      rolledBack    = fieldBool "rolled_back" add == Just True
  cReject <- liveCheck $ checkPure
    "bogus add returns success=false with error_kind=unresolvable_dep"
    (rejected && kindIsUnres && rolledBack)
    ( "Expected: success=false, error_kind=\"unresolvable_dep\", \
      \rolled_back=true. Got: success="
      <> T.pack (show (fieldBool "success" add))
      <> ", error_kind=" <> T.pack (show (fieldText "error_kind" add))
      <> ", rolled_back=" <> T.pack (show (fieldBool "rolled_back" add))
      <> ". Raw: " <> truncRender add )
  stepFooter 1 t0

  -- 2. Ground truth: the .cabal must NOT contain the bogus pkg
  -- after the rejected add. ghc_deps(list) reads from disk.
  t1 <- stepHeader 2 "rollback · build_depends does NOT contain bogus pkg"
  ls <- Client.callTool c GhcDeps
          (object [ "action" .= ("list" :: Text) ])
  let depsAfterReject = buildDeps ls
      bogusAbsent     = not (any (bogusPkg `T.isInfixOf`) depsAfterReject)
  cRollback <- liveCheck $ checkPure
    "post-reject list does NOT contain bogus pkg · rollback wrote disk"
    bogusAbsent
    ( "If the list still contains the bogus pkg, the rollback step \
      \failed — the .cabal is in the post-edit state but cabal \
      \rejected it, leaving the project broken. build_depends="
      <> T.pack (show depsAfterReject)
      <> ". Raw: " <> truncRender ls )
  stepFooter 2 t1

  -- 3. The session must boot cleanly after the rejected add. If
  -- rollback worked, cabal repl resolves the (unchanged) dep set
  -- and ghc_eval succeeds. If rollback didn't fully revert, the
  -- session boot would fail with the same solver error verifyAndCommit
  -- saw.
  t2 <- stepHeader 3 "session alive · ghc_eval(1+1) after rejected add"
  alive <- Client.callTool c GhcEval
             (object [ "expression" .= ("1 + 1" :: Text) ])
  let aliveOk = fieldBool "success" alive == Just True
             && case lookupField "output" alive of
                  Just (String s) -> "2" `T.isInfixOf` s
                  _               -> False
  cAlive <- liveCheck $ checkPure
    "session alive post-reject · project remains buildable"
    aliveOk
    ( "After a rejected add the project must boot the same as before. \
      \If this fails, rollback didn't fully revert the .cabal. Raw: "
      <> truncRender alive )
  stepFooter 3 t2

  -- 4. Happy path: add a boot-library package; verify must accept.
  -- This is the symmetric oracle — without it, a verify step that
  -- ALWAYS rejects would still pass step 1 but break every real add.
  t3 <- stepHeader 4
          ("accept · ghc_deps(add, \"" <> validPkg <> "\") commits")
  addOk <- Client.callTool c GhcDeps
             (object
               [ "action"  .= ("add" :: Text)
               , "package" .= validPkg
               ])
  cAccept <- liveCheck $ checkPure
    "valid boot-library add returns success=true · verify accepts"
    (fieldBool "success" addOk == Just True)
    ( "Boot library 'mtl' should always resolve under modern cabal. \
      \If this fails, the verify step is over-rejecting (false \
      \positive in extractErrorSummary or the dry-run failed for an \
      \unrelated reason). Raw: " <> truncRender addOk )
  stepFooter 4 t3

  t4 <- stepHeader 5 "list · build_depends contains valid pkg after add"
  ls2 <- Client.callTool c GhcDeps
           (object [ "action" .= ("list" :: Text) ])
  let depsAfterAccept = buildDeps ls2
      validPresent    = any (validPkg `T.isInfixOf`) depsAfterAccept
  cAcceptList <- liveCheck $ checkPure
    "post-accept list contains valid pkg · edit persisted to disk"
    validPresent
    ( "After accepted add the dep should be in build_depends. \
      \build_depends=" <> T.pack (show depsAfterAccept)
      <> ". Raw: " <> truncRender ls2 )
  stepFooter 5 t4

  -- 5. Cleanup: remove the valid pkg so the project state is
  -- unchanged when the scenario exits.
  t5 <- stepHeader 6 ("cleanup · ghc_deps(remove, \"" <> validPkg <> "\")")
  _ <- Client.callTool c GhcDeps
         (object
           [ "action"  .= ("remove" :: Text)
           , "package" .= validPkg
           ])
  stepFooter 6 t5

  pure [cReject, cRollback, cAlive, cAccept, cAcceptList]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldText :: Text -> Value -> Maybe Text
fieldText k v = case lookupField k v of
  Just (String t) -> Just t
  _               -> Nothing

-- | Extract the build_depends array from a ghc_deps(list) response.
-- The field name the MCP returns is @build_depends@ (not @packages@).
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
