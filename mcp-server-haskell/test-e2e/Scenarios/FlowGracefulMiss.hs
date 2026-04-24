-- | Flow: graceful failure on user-side mistakes.
--
-- Real user flow — every /miss/ is something a dev actually does. The
-- question is not whether the MCP returns a perfect error, but whether
-- it returns a /structured/ error (success=false + a legible field)
-- instead of succeeding silently or crashing the transport.
--
-- Covered misses:
--
--   (1) @ghc_deps(action="remove", package="not-a-dep")@ on a scaffold
--       that does not depend on it. A silent success would lie about
--       having removed nothing; a structured refusal (or an explicit
--       no-op with @removed=0@) lets the agent course-correct.
--
--   (2) @ghc_hole@ on a module that has no holes. The tool must
--       return @hole_count: 0@ cleanly, not an error — a dev running
--       the tool optimistically after a patch should not get a
--       spurious failure just because the holes are already gone.
--
--   (3) @ghc_quickcheck(property="42")@ — the property is not a
--       predicate. GHCi would refuse to run @quickCheck (42)@ because
--       @42@ doesn't satisfy @Testable@. The MCP must surface that as
--       a structured failure, not let the GHCi panic bubble up as a
--       transport error or freeze the session.
--
-- Each failure mode is a real UX hazard: silent passes, crashes, or
-- protocol-level errors all lose information the agent could have
-- used to retry or ask the user.
module Scenarios.FlowGracefulMiss
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
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

-- | Simple module with no holes — used by the no-holes miss test.
noHolesSrc :: Text
noHolesSrc = T.unlines
  [ "module Whole where"
  , ""
  , "one :: Int"
  , "one = 1"
  , ""
  , "two :: Int"
  , "two = 2"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Whole (a module with NO holes)"
  _ <- Client.callTool c "ghc_create_project"
         (object [ "name" .= ("gracefulmiss-demo" :: Text) ])
  _ <- Client.callTool c "ghc_add_modules"
         (object [ "modules" .= (["Whole"] :: [Text]) ])
  _ <- Client.callTool c "ghc_deps" (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Whole.hs") noHolesSrc
  _ <- Client.callTool c "ghc_load"
         (object [ "module_path" .= ("src/Whole.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) remove a package the project never had
  --
  -- Correct answers (either is acceptable):
  --   a) success=false with an explanatory error (preferred).
  --   b) success=true + the package_removed field absent / empty
  --      AND an explicit hint the dep wasn't present.
  -- WRONG:
  --   success=true with no signal — the agent thinks it worked.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_deps(remove, 'not-a-real-dep') — package never existed"
  r1 <- Client.callTool c "ghc_deps" (object
    [ "action"  .= ("remove" :: Text)
    , "package" .= ("not-a-real-dep" :: Text)
    , "stanza"  .= ("library" :: Text)
    ])
  let succ1 = fieldBool "success" r1
      refused = succ1 == Just False
      -- 'note' is the explainer emitted by 'Tool.Deps.unchangedResult'
      -- for idempotent add/remove no-ops (verb-specific: "already
      -- present" / "not listed in target stanza"). Accepting it here
      -- keeps the "no silent success" oracle honest without
      -- requiring remove-on-absent to be a structured FAILURE —
      -- which was a remove-shaped error being reported on what's
      -- actually a semantically-well-defined idempotent call.
      hinted  = hasField "hint"    r1
             || hasField "error"   r1
             || hasField "errors"  r1
             || hasField "message" r1
             || hasField "note"    r1
      honest  = refused || hinted
  cMiss1 <- liveCheck $ checkPure
    "deps remove of absent package · structured signal (not silent)"
    honest
    ("The MCP must tell the caller the dep wasn't there. Either \
     \success=false, OR success=true with a 'note'/'hint'/'error' \
     \that explains the no-op. Silent success would let a stale \
     \removal request look like it worked. Raw: "
     <> truncRender r1)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (2) ghc_hole on a module that has no holes
  --
  -- Correct: success=true with hole_count=0 (or holes=[]).
  -- WRONG: success=false or hole_count missing — the agent has
  -- no way to distinguish "no holes" from "tool broke".
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_hole on Whole.hs (no holes present)"
  r2 <- Client.callTool c "ghc_hole"
          (object [ "module_path" .= ("src/Whole.hs" :: Text) ])
  let succ2   = fieldBool "success" r2 == Just True
      countOk = fieldInt   "hole_count" r2 == Just 0
  cMiss2 <- liveCheck $ checkPure
    "hole on a hole-free module · success=true + hole_count=0"
    (succ2 && countOk)
    ("A hole-free module is the desired end state. The tool must \
     \report 0 holes cleanly; an error on this input would make \
     \'run hole after every patch' an unviable loop. Raw: "
     <> truncRender r2)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (3) quickcheck of a non-property (@42@ is not Testable)
  --
  -- Correct: success=false with an error mentioning the type
  -- rejection, OR state=\"gave_up\"/\"failed\" with a structured
  -- payload. Important: the session MUST survive so subsequent
  -- tools still work.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_quickcheck('42') — not a predicate"
  r3 <- Client.callTool c "ghc_quickcheck" (object
    [ "property" .= ("42" :: Text)
    , "module"   .= ("src/Whole.hs" :: Text)
    ])
  let stateField   = lookupField "state" r3
      passedField  = lookupField "passed" r3
      -- The bug-finder invariant: the tool MUST NOT claim the
      -- non-predicate "passed". Anything else — state=failed,
      -- state=gave_up, timeout shape, success=false, an error
      -- field — is acceptable. A false positive (passed=true) is
      -- the one answer that would ship broken code past the gate.
      claimsPassed = stateField  == Just (String "passed")
                  || passedField == Just (Bool True)
      -- Minimum floor: still a structured Object, never a null
      -- or raw transport error bubble.
      structured = case r3 of Object _ -> True; _ -> False
  cMiss3 <- liveCheck $ checkPure
    "quickcheck of non-predicate · not a false positive (state != passed)"
    (structured && not claimsPassed)
    ("A property that does not satisfy Testable CANNOT come back as \
     \'passed'. Anything else (failed, gave_up, error, timeout) is \
     \the MCP behaving correctly. If this test flips to FAIL, the \
     \quickcheck tool is silently greenlighting non-predicates. \
     \Raw: " <> truncRender r3)

  -- Critical liveness assert: session must still be alive after
  -- the bad call. A follow-up ghc_eval should respond quickly.
  r4 <- Client.callTool c "ghc_eval"
          (object [ "expression" .= ("1 + 1" :: Text) ])
  let sessionAlive = fieldBool "success" r4 == Just True
  cLive <- liveCheck $ checkPure
    "session survives · ghc_eval(1+1) still works after the failed QC"
    sessionAlive
    ("If this fails, the failed QuickCheck took the session down with \
     \it — an agent that hits a bad property could not recover. \
     \Raw: " <> truncRender r4)
  stepFooter 4 t3

  pure [cMiss1, cMiss2, cMiss3, cLive]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldInt :: Text -> Value -> Maybe Int
fieldInt k v = case lookupField k v of
  Just (Number n) -> Just (round n)
  _               -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

hasField :: Text -> Value -> Bool
hasField k (Object o) = KeyMap.member (Key.fromText k) o
hasField _ _          = False

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
