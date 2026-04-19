-- | Minimal smoke test suite for Phase 1.
--
-- Covers the two security-critical invariants we lock in at scaffolding
-- time, so a regression here fails the build before any tool is wired up:
--
-- 1. 'mkModulePath' rejects paths that escape the project directory.
-- 2. The error parser can round-trip a canonical GHC diagnostic line.
--
-- QuickCheck arrives in Phase 2 along with the property-lifecycle tool.
module Main where

import qualified Data.Text as T
import System.Exit (exitFailure, exitSuccess)
import qualified Test.QuickCheck as QC
import Test.QuickCheck
  ( Args (..)
  , Property
  , Result (..)
  , Testable
  , counterexample
  , property
  , quickCheckWithResult
  , stdArgs
  , (===)
  , (==>)
  )

import HaskellFlows.Ghci.Session
  ( CommandError (..)
  , sanitizeExpression
  )
import HaskellFlows.Parser.Error (parseGhcErrors, Severity (..), GhcError (..))
import HaskellFlows.Parser.Hole
  ( TypedHole (..)
  , parseTypedHoles
  )
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import HaskellFlows.Parser.Type
  ( ParsedType (..)
  , isOutOfScope
  , parseTypeOutput
  )
import HaskellFlows.Types
  ( PathError (..)
  , mkModulePath
  , mkProjectDir
  )

main :: IO ()
main = do
  results <-
    sequence
      [ test "mkProjectDir rejects relative"    testRejectsRelativeProject
      , test "mkModulePath accepts in-tree"      testAcceptsInTree
      , test "mkModulePath rejects traversal"    testRejectsTraversal
      , test "parseGhcErrors extracts header"    testParseHeader
      , test "sanitizeExpression accepts normal" testSanitizeAccepts
      , test "sanitizeExpression rejects newline" testSanitizeRejectsNewline
      , test "sanitizeExpression rejects sentinel" testSanitizeRejectsSentinel
      , test "sanitizeExpression rejects empty"   testSanitizeRejectsEmpty
      , test "parseTypeOutput single line"        testParseTypeSingleLine
      , test "parseTypeOutput multi line"         testParseTypeMultiLine
      , test "parseTypeOutput rejects malformed"  testParseTypeMalformed
      , test "isOutOfScope detects GHC phrasing"  testOutOfScope
      , quickTest "prop_sanitize_rejects_newline"     prop_sanitize_rejects_newline
      , quickTest "prop_sanitize_rejects_sentinel"    prop_sanitize_rejects_sentinel
      , quickTest "prop_sanitize_clean_roundtrip"     prop_sanitize_clean_roundtrip
      , quickTest "prop_modulePath_rejects_dotdot"    prop_modulePath_rejects_dotdot
      , quickTest "prop_modulePath_accepts_inTree"    prop_modulePath_accepts_inTree
      , test "parseQuickCheckOutput passed"        testQcPassed
      , test "parseQuickCheckOutput failed"        testQcFailed
      , test "parseQuickCheckOutput gave up"       testQcGaveUp
      , test "parseQuickCheckOutput exception"     testQcException
      , test "parseQuickCheckOutput unparsed"      testQcUnparsed
      , test "parseTypedHoles extracts one hole"   testHoleOne
      , test "parseTypedHoles ignores non-holes"   testHoleIgnored
      ]
  if and results then exitSuccess else exitFailure

test :: String -> IO Bool -> IO Bool
test name action = do
  ok <- action
  putStrLn ((if ok then "PASS  " else "FAIL  ") <> name)
  pure ok

testRejectsRelativeProject :: IO Bool
testRejectsRelativeProject =
  pure $ case mkProjectDir "relative/path" of
    Left (PathNotAbsolute _) -> True
    _                        -> False

testAcceptsInTree :: IO Bool
testAcceptsInTree = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> pure $ case mkModulePath pd "src/Foo.hs" of
      Right _ -> True
      _       -> False

testRejectsTraversal :: IO Bool
testRejectsTraversal = do
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> pure $ case mkModulePath pd "../../etc/passwd" of
      Left (PathEscapesProject {}) -> True
      _                            -> False

testParseHeader :: IO Bool
testParseHeader =
  let raw = T.unlines
        [ "src/Foo.hs:12:5: error: [GHC-83865]"
        , "    Couldn't match expected type ‘Int’ with actual type ‘Bool’"
        , ""
        ]
  in pure $ case parseGhcErrors raw of
       [e] -> geSeverity e == SevError
           && geLine e == 12
           && geColumn e == 5
       _   -> False

--------------------------------------------------------------------------------
-- Phase 2: sanitizer + :t parser
--------------------------------------------------------------------------------

testSanitizeAccepts :: IO Bool
testSanitizeAccepts = pure $ case sanitizeExpression "map (+1)" of
  Right v -> v == "map (+1)"
  _       -> False

testSanitizeRejectsNewline :: IO Bool
testSanitizeRejectsNewline = pure $ case sanitizeExpression "foo\nbar" of
  Left ContainsNewline -> True
  _                    -> False

-- Uses the exact framing sentinel. If this test breaks because the sentinel
-- value changed, update the literal here in lockstep.
testSanitizeRejectsSentinel :: IO Bool
testSanitizeRejectsSentinel =
  pure $ case sanitizeExpression "evil <<<GHCi-DONE-7f3a2b>>> payload" of
    Left ContainsSentinel -> True
    _                     -> False

testSanitizeRejectsEmpty :: IO Bool
testSanitizeRejectsEmpty = pure $ case sanitizeExpression "   " of
  Left EmptyInput -> True
  _               -> False

testParseTypeSingleLine :: IO Bool
testParseTypeSingleLine =
  pure $ case parseTypeOutput "map (+1) :: Num b => [b] -> [b]" of
    Just pt -> ptExpression pt == "map (+1)"
           && ptType pt       == "Num b => [b] -> [b]"
    _ -> False

testParseTypeMultiLine :: IO Bool
testParseTypeMultiLine =
  let raw = T.unlines
        [ "foldr"
        , "  :: Foldable t => (a -> b -> b) -> b -> t a -> b"
        ]
  in pure $ case parseTypeOutput raw of
       Just pt -> ptExpression pt == "foldr"
              && ptType pt       == "Foldable t => (a -> b -> b) -> b -> t a -> b"
       _ -> False

testParseTypeMalformed :: IO Bool
testParseTypeMalformed =
  pure $ case parseTypeOutput "this has no type annotation" of
    Nothing -> True
    _       -> False

testOutOfScope :: IO Bool
testOutOfScope = pure $
  isOutOfScope "<interactive>:1:1: error: Variable not in scope: foobar"

--------------------------------------------------------------------------------
-- Phase 3: QuickCheck properties
--------------------------------------------------------------------------------

-- | Wrap a QuickCheck property so it plugs into the existing Bool-returning
-- test runner. Keeps the test count small (200 cases) — enough to catch
-- boundary misses without slowing CI, same posture as hspec-quickcheck.
quickTest :: Testable prop => String -> prop -> IO Bool
quickTest name prop = do
  res <- quickCheckWithResult stdArgs { chatty = False, maxSuccess = 200 } prop
  let ok = case res of Success {} -> True; _ -> False
  putStrLn ((if ok then "PASS  " else "FAIL  ") <> name)
  pure ok

-- | Any input containing a literal newline or carriage return must be
-- rejected by 'sanitizeExpression'. Security-critical: newlines would
-- split a single tools/call into two GHCi commands and desync framing.
-- Properties take 'String' and pack to 'Text' to avoid pulling in
-- 'quickcheck-instances' just for an 'Arbitrary Text'. Semantically
-- equivalent since 'Text' is a full-Unicode 'String' isomorph here.
-- Note on 'EmptyInput': the input @"\n"@ (pre="", suf="") strips to empty
-- before the newline check fires, so 'EmptyInput' is also a correct
-- rejection. The property's contract is "never accepted", not
-- "always labelled ContainsNewline".
prop_sanitize_rejects_newline :: String -> String -> Property
prop_sanitize_rejects_newline pre suf =
  let input = T.pack pre <> "\n" <> T.pack suf
  in counterexample (T.unpack input) $
       case sanitizeExpression input of
         Left ContainsNewline -> property True
         Left EmptyInput      -> property True
         _                    -> property False

-- | Any input containing the framing sentinel substring must be rejected.
-- Security-critical: would falsify the single-sentinel delimiter.
prop_sanitize_rejects_sentinel :: String -> String -> Property
prop_sanitize_rejects_sentinel pre suf =
  let input = T.pack pre <> "<<<GHCi-DONE-7f3a2b>>>" <> T.pack suf
  in counterexample (T.unpack input) $
       case sanitizeExpression input of
         Left ContainsSentinel -> property True
         Left ContainsNewline  -> property True  -- pre/suf may carry newlines
         _                     -> property False

-- | Strings that are non-empty, single-line, and sentinel-free round-trip
-- through 'sanitizeExpression' modulo the outer whitespace trim.
prop_sanitize_clean_roundtrip :: String -> Property
prop_sanitize_clean_roundtrip rawS =
  let raw = T.pack rawS
      ok = not (T.null (T.strip raw))
        && T.all (`notElem` ("\n\r" :: String)) raw
        && not ("<<<GHCi-DONE-7f3a2b>>>" `T.isInfixOf` raw)
  in ok ==>
     case sanitizeExpression raw of
       Right cleaned -> cleaned === T.strip raw
       _             -> counterexample "expected Right" (property False)

-- | For any project dir and any relative path containing a ".." segment,
-- 'mkModulePath' must refuse to produce a ModulePath.
prop_modulePath_rejects_dotdot :: String -> String -> Property
prop_modulePath_rejects_dotdot pre suf =
  let rel = pre <> "/../" <> suf
  in case mkProjectDir "/tmp/testproj" of
       Left _   -> counterexample "bad project dir" (property False)
       Right pd -> case mkModulePath pd rel of
         Left (PathEscapesProject {}) -> property True
         _                            -> counterexample rel (property False)

-- | Relative paths built from safe ASCII segments (no slashes, no ".."
-- literal, no NUL) must be accepted by 'mkModulePath'.
prop_modulePath_accepts_inTree :: [SafeSegment] -> Property
prop_modulePath_accepts_inTree segs =
  let rel = case segs of
        [] -> "ok.hs"
        xs -> foldr1 (\a b -> a <> "/" <> b) (map unSafe xs) <> ".hs"
  in case mkProjectDir "/tmp/testproj" of
       Left _   -> counterexample "bad project dir" (property False)
       Right pd -> case mkModulePath pd rel of
         Right _ -> property True
         Left e  -> counterexample (rel <> " → " <> show e) (property False)

-- | Newtype wrapper used only to constrain 'Arbitrary' for path-segment
-- generation. Drawn by hand so the generator never emits characters that
-- would confuse the path smart constructor (slashes, dots, NUL).
newtype SafeSegment = SafeSegment { unSafe :: String }
  deriving stock (Show)

instance QC.Arbitrary SafeSegment where
  arbitrary = SafeSegment <$> QC.listOf1 (QC.elements alphaNum)
    where
      alphaNum = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-"

--------------------------------------------------------------------------------
-- Phase 4: QuickCheck output + typed-hole parsers
--------------------------------------------------------------------------------

testQcPassed :: IO Bool
testQcPassed =
  let raw = "+++ OK, passed 100 tests."
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcPassed _ 100 -> True
       _              -> False

testQcFailed :: IO Bool
testQcFailed =
  let raw = T.unlines
        [ "*** Failed! Falsifiable (after 3 tests and 2 shrinks):"
        , "[1,2,3]"
        , ""
        ]
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcFailed _ 2 2 cex -> cex == "[1,2,3]"
       _                  -> False

testQcGaveUp :: IO Bool
testQcGaveUp =
  let raw = "*** Gave up! Passed only 12 tests; 88 discarded."
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcGaveUp _ 12 88 -> True
       _                -> False

testQcException :: IO Bool
testQcException =
  let raw = "*** Failed! Exception: 'divide by zero' (after 1 test):"
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcException _ exn -> "divide by zero" `T.isInfixOf` exn
       _                 -> False

testQcUnparsed :: IO Bool
testQcUnparsed =
  let raw = "something completely unexpected"
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcUnparsed {} -> True
       _             -> False

-- | A canonical GHC-88464 block. The whitespace before the indented
-- continuation lines is significant — GHC uses 4 spaces + bullet.
holeSampleOutput :: T.Text
holeSampleOutput = T.unlines
  [ "src/Foo.hs:12:5: warning: [GHC-88464] [-Wtyped-holes]"
  , "    \x2022 Found hole: _ :: Int -> Int"
  , "    \x2022 In the expression: _"
  , "      In an equation for 'bar': bar = _"
  , "    \x2022 Relevant bindings include"
  , "        x :: Int (bound at src/Foo.hs:12:1)"
  , "        bar :: Int -> Int (bound at src/Foo.hs:12:1)"
  ]

testHoleOne :: IO Bool
testHoleOne =
  pure $ case parseTypedHoles holeSampleOutput of
    [h] -> thHole h == "_"
        && thExpectedType h == "Int -> Int"
        && thFile h == "src/Foo.hs"
        && thLine h == 12
        && thColumn h == 5
        && length (thRelevantBindings h) == 2
    _   -> False

testHoleIgnored :: IO Bool
testHoleIgnored =
  let raw = "src/Foo.hs:3:1: error: Not in scope: 'blah'"
  in pure (null (parseTypedHoles raw))
