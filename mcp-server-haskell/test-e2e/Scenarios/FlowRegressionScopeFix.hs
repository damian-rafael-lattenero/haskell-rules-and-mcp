-- | Flow: the UX fixes to @ghc_quickcheck@ + @ghc_regression@ that
-- came out of the expr-evaluator dogfood.
--
-- Two bugs, one scenario:
--
--   (a) @ghc_quickcheck(property="prop_x", module="src/Foo.hs")@
--       used to persist /src\/Foo.hs/ verbatim, even when the
--       property actually lives in @test/Spec.hs@. The fix consults
--       @:info prop_x@ and stores the resolved path. The oracle here
--       calls quickcheck with a deliberately wrong hint and asserts
--       @ghc_regression list@ shows the corrected path.
--
--   (b) @ghc_regression run@ used to leave the caller in whichever
--       module it touched last — a second @ghc_eval@ from the
--       caller would then fail with "Variable not in scope". The
--       fix snapshots @:show modules@ before the replay loop and
--       re-loads the same set afterwards. The oracle loads a module,
--       does something unrelated, runs regression (which reloads a
--       different file to bring its property into scope), and then
--       asserts the original module's definitions are still live.
--
-- Both fixes together mean the common dogfood ritual — quickcheck a
-- named property, later run regression — just works even when the
-- caller passed the "wrong" module hint or had loaded something
-- different between the two calls.
module Scenarios.FlowRegressionScopeFix
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
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
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- sources
--------------------------------------------------------------------------------

-- | A library module with a simple value we can check in scope post-
-- regression. Purposefully trivial: the test is about scope, not
-- about the value.
fooSrc :: Text
fooSrc = T.unlines
  [ "module Foo (answer) where"
  , ""
  , "answer :: Int"
  , "answer = 42"
  ]

-- | A test-suite Main with one named property. The property is an
-- identifier (not a lambda) so the @:info@-based resolution kicks
-- in when @ghc_quickcheck@ persists it.
specSrc :: Text
specSrc = T.unlines
  [ "module Main where"
  , ""
  , "import Test.QuickCheck (quickCheck)"
  , ""
  , "prop_trivial :: Bool"
  , "prop_trivial = True"
  , ""
  , "main :: IO ()"
  , "main = quickCheck prop_trivial"
  ]

--------------------------------------------------------------------------------
-- flow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) scaffold + deps + Foo + Spec
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold · project + QuickCheck dep + Foo + Spec"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("scope-fix-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Foo"] :: [Text]) ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Foo.hs") fooSrc
  TIO.writeFile (projectDir </> "test" </> "Spec.hs") specSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("test/Spec.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) quickcheck with a DELIBERATELY WRONG module hint
  --
  -- The property lives in test/Spec.hs. We pass src/Foo.hs — the
  -- kind of natural mistake (passing the "module under test") that
  -- used to silently poison the regression store.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "quickcheck · prop_trivial with module=\"src/Foo.hs\" (wrong!)"
  qcR <- Client.callTool c GhcQuickCheck (object
    [ "property" .= ("prop_trivial" :: Text)
    , "module"   .= ("src/Foo.hs"    :: Text)  -- the BUG input
    ])
  let qcOk = fieldString "state" qcR == Just "passed"
  cQc <- liveCheck $ checkPure
    "quickcheck · prop_trivial passes (sanity before checking resolution)"
    qcOk
    ("If this fails the scenario is miswired — prop_trivial is \
     \literally True. Raw: " <> truncRender qcR)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) FIX #1 ORACLE — regression list must show the RESOLVED
  --     path (test/Spec.hs), not the wrong hint the caller passed
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "fix #1 · ghc_regression list reports resolved module"
  listR <- Client.callTool c GhcRegression
             (object [ "action" .= ("list" :: Text) ])
  -- 'ghc_quickcheck' auto-resolves via ':info prop_trivial', which
  -- returns the ABSOLUTE path to test/Spec.hs. Both the relative
  -- "test/Spec.hs" (as the caller might have hoped for) and the
  -- absolute "/tmp/.../test/Spec.hs" (what GHCi actually reports)
  -- are acceptable shapes — the oracle is "ends in test/Spec.hs AND
  -- is NOT the wrong src/Foo.hs hint the caller passed".
  let storedModule = firstPropertyModule listR
      endsInSpec   = case storedModule of
        Just m  -> "test/Spec.hs" `T.isSuffixOf` m
        Nothing -> False
      notWrongHint = case storedModule of
        Just m  -> not ("src/Foo.hs" `T.isSuffixOf` m)
        Nothing -> False
  cResolve <- liveCheck $ checkPure
    "resolved · stored module ends in test/Spec.hs (NOT src/Foo.hs)"
    (endsInSpec && notWrongHint)
    ("The store must keep the file where the property is DEFINED, \
     \not the file the caller hinted at. If this shows 'src/Foo.hs', \
     \Tool/QuickCheck.hs regressed to the pre-fix behaviour and \
     \regression replay will fail downstream. Raw: "
     <> truncRender listR)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (4) knock the caller's scope off test/Spec.hs on purpose
  --
  -- Load Foo.hs so the active scope is 'Foo', not 'Main'. Any
  -- subsequent 'ghc_eval' of prop_trivial would fail at this
  -- point — that is the pre-fix state of affairs for a caller
  -- who loaded something else between quickcheck and regression.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "scope shift · ghc_load src/Foo.hs (displaces Main)"
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Foo.hs" :: Text) ])
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- (5) FIX #2 PART A — regression run must work even with
  --     Main knocked out of scope, because the runner auto-loads
  --     the stored module before each property
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "fix #2a · ghc_regression run passes 1/1 despite scope shift"
  runR <- Client.callTool c GhcRegression
            (object [ "action" .= ("run" :: Text) ])
  let runPassed      = fieldInt "passed" runR == Just 1
      runTotal       = fieldInt "total"  runR == Just 1
      runRegressions = fieldArrayLen "regressions" runR == Just 0
  cRun <- liveCheck $ checkPure
    "regression run · 1/1 replay (auto-loaded test/Spec.hs on the fly)"
    (runPassed && runTotal && runRegressions)
    ("If this FAILS with 'Variable not in scope: prop_trivial', the \
     \per-property ':load' in Tool/Regression.runOne regressed. If \
     \it fails any other way, runProperty or the store got broken \
     \independently. Raw: " <> truncRender runR)
  stepFooter 5 t4

  ----------------------------------------------------------------
  -- (6) FIX #2 PART B — scope restoration: after regression, Foo's
  --     'answer' must still be in scope (we loaded src/Foo.hs in
  --     step 4, regression reloaded test/Spec.hs in step 5, then
  --     should have restored the pre-run module set)
  ----------------------------------------------------------------
  t5 <- stepHeader 6 "fix #2b · post-regression, 'answer' from Foo is still live"
  evalR <- Client.callTool c GhcEval
             (object [ "expression" .= ("answer" :: Text) ])
  let answerOk = fieldBool "success" evalR == Just True
              && case fieldString "output" evalR of
                   Just s  -> "42" `T.isInfixOf` s
                   Nothing -> False
  cRestore <- liveCheck $ checkPure
    "restored · ghc_eval 'answer' returns 42 (Foo still loaded)"
    answerOk
    ("If this fails with 'Variable not in scope: answer', regression \
     \run did not restore the caller's pre-run :show modules state. \
     \The snapshot/restore pair in Tool/Regression.handle regressed. \
     \Raw: " <> truncRender evalR)
  stepFooter 6 t5

  pure [cQc, cResolve, cRun, cRestore]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Pick the 'module' field of the FIRST entry in a 'ghc_regression list'
-- response. The response shape is:
--   { "properties": [ { "expression": ..., "module": ..., ... }, ... ] }
firstPropertyModule :: Value -> Maybe Text
firstPropertyModule v = case lookupField "properties" v of
  Just (Array arr) | not (V.null arr) -> case V.head arr of
    Object kv -> case KeyMap.lookup (Key.fromText "module") kv of
      Just (String s) -> Just s
      _               -> Nothing
    _ -> Nothing
  _ -> Nothing

fieldString :: Text -> Value -> Maybe Text
fieldString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldInt :: Text -> Value -> Maybe Int
fieldInt k v = case lookupField k v of
  Just (Number n) -> Just (round n)
  _               -> Nothing

fieldArrayLen :: Text -> Value -> Maybe Int
fieldArrayLen k v = case lookupField k v of
  Just (Array a) -> Just (V.length a)
  _              -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
