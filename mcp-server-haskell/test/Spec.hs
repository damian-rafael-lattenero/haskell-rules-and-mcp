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

import qualified Data.Aeson as A
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)
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
  , sessionCabalArgs
  )
import HaskellFlows.Parser.Error (parseGhcErrors, Severity (..), GhcError (..))
import HaskellFlows.Parser.Hole
  ( HoleFit (..)
  , TypedHole (..)
  , parseTypedHoles
  , extractValidFits
  )
import HaskellFlows.Parser.TypeSignature
  ( ParsedSig (..)
  , SigType (..)
  , parseSignature
  , isSameTypeThroughout
  )
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , Suggestion (..)
  , applyRules
  )
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNames)
import HaskellFlows.Mcp.Protocol (ToolCall (..))
import HaskellFlows.Tool.Batch (BatchArgs (..))
import HaskellFlows.Tool.CheckProject (parseExposedModules)
import HaskellFlows.Tool.Lint (parseHlintJson)
import qualified HaskellFlows.Tool.Lint as LintTool
import qualified HaskellFlows.Tool.ValidateCabal as VC
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import HaskellFlows.Parser.Type
  ( ParsedType (..)
  , isOutOfScope
  , parseTypeOutput
  )
import HaskellFlows.Tool.Arbitrary
  ( Constructor (..)
  , parseConstructors
  , renderTemplate
  )
import HaskellFlows.Data.PropertyStore
  ( StoredProperty (..)
  , loadAll
  , openStore
  , save
  )
import HaskellFlows.Parser.Coverage
  ( CoverageReport (..)
  , Metric (..)
  , parseCoverage
  )
import HaskellFlows.Tool.Deps
  ( addDep
  , parseStanzaSelector
  , removeDep
  , validatePackageName
  , validateVersionConstraint
  )
import HaskellFlows.Refactor.Extract
  ( ExtractResult (..)
  , extractBinding
  )
import HaskellFlows.Refactor.Rename
  ( RenameResult (..)
  , renameInScope
  , validateIdentifier
  )
import HaskellFlows.Tool.Goto
  ( Location (..)
  , parseDefinedAt
  )
import HaskellFlows.Tool.Hoogle
  ( HoogleHit (..)
  , parseHoogleLine
  )
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import HaskellFlows.Types
  ( PathError (..)
  , ProjectDir
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
      , test "parseConstructors inline form"       testCtorsInline
      , test "parseConstructors multiline form"    testCtorsMultiline
      , test "parseConstructors rejects synonym"   testCtorsSynonym
      , test "renderTemplate 3 ctors"              testTemplate3
      , test "parseHoogleLine normal hit"          testHoogleHit
      , test "parseHoogleLine no-results line"    testHoogleEmpty
      , test "parseCoverage full report"           testCoverageFull
      , test "parseCoverage ignores banner"        testCoverageBanner
      , test "PropertyStore save+load roundtrip"   testStoreRoundtrip
      , test "PropertyStore increments pass count" testStoreIncrement
      , test "validatePackageName accepts normal"  testPkgAccepts
      , test "validatePackageName rejects symbol"  testPkgRejectsSymbol
      , test "validatePackageName rejects empty"   testPkgRejectsEmpty
      , test "validateVersionConstraint accepts"   testVerAccepts
      , test "validateVersionConstraint rejects"   testVerRejects
      , test "parseDefinedAt file location"        testDefinedAtFile
      , test "parseDefinedAt module location"      testDefinedAtModule
      , test "parseDefinedAt ignores noise"        testDefinedAtNone
      , test "rename respects word boundaries"     testRenameWordBoundary
      , test "rename ignores line comments"        testRenameIgnoresComments
      , test "rename ignores string literals"      testRenameIgnoresStrings
      , test "rename scoped to line range"         testRenameScoped
      , test "rename same name is rejected"        testRenameSameName
      , test "validateIdentifier rejects keyword"  testIdentifierKeyword
      , test "validateIdentifier rejects symbol"   testIdentifierSymbol
      , test "validateIdentifier rejects upper"    testIdentifierUpper
      , test "extractBinding wraps block"          testExtractBinding
      , test "extractBinding rejects empty range"  testExtractEmpty
      , test "parseHlintJson parses list"          testHlintJson
      , test "validateCabal flags duplicate deps"  testDuplicateDeps
      , test "validateCabal flags missing synopsis" testMissingSynopsis
      , test "parseExposedModules reads modules"   testParseModules
      , test "extractValidFits parses fits"        testValidFits
      , test "parseSignature simple a -> a"         testSigSimple
      , test "parseSignature with constraint"       testSigConstraint
      , test "parseSignature list"                  testSigList
      , test "suggest matches involutive on a->a"   testSuggestInvolutive
      , test "suggest matches associative on a->a->a" testSuggestAssoc
      , test "suggest skips unmatched shapes"       testSuggestNoMatch
      , test "batch parses documented {tool,args}"  testBatchParsesToolArgs
      , test "batch accepts MCP {name,arguments}"   testBatchParsesNameArgs
      , test "suggest reverse Idempotent is Low"    testSuggestReverseIdempotentLow
      , test "suggest normalize Idempotent Medium"  testSuggestNormalizeIdempotentMedium
      , test "workflow tool names match tools/list" testWorkflowToolsParity
      , test "deps add indents deeper than field"   testDepsAddIndentsForCabal
      , test "deps add scaffold shape has no top-comma" testDepsAddNoTopComma
      , test "deps add targets stanza: test-suite"  testDepsAddTargetsTestSuite
      , test "parseStanzaSelector accepts common"   testParseStanzaAccepts
      , test "parseStanzaSelector rejects garbage"  testParseStanzaRejects
      , test "suggest [a]->[Run a] skips list rules" testSuggestEncodeShapeSkipsListRules
      , test "parseCtors record strict w/ kind header" testCtorsRecordStrictWithKindHeader
      , test "parseCtors inline record 2 fields"    testCtorsInlineRecord2Fields
      , test "session spawns with QuickCheck dep"   testSessionIncludesQuickCheck
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

--------------------------------------------------------------------------------
-- Phase 5: Arbitrary + Hoogle parsers
--------------------------------------------------------------------------------

-- | Inline @data T = A | B Int | C Int String@ should produce three
-- constructors with arities 0, 1, 2.
testCtorsInline :: IO Bool
testCtorsInline =
  let raw = T.unlines
        [ "data Foo = Bar | Baz Int | Qux Int String"
        , "  \t-- Defined at src/Foo.hs:5:1"
        ]
  in pure $ case parseConstructors raw of
       [a, b, c] -> cName a == "Bar" && null (cArgs a)
                 && cName b == "Baz" && length (cArgs b) == 1
                 && cName c == "Qux" && length (cArgs c) == 2
       _         -> False

-- | GHCi's multi-line form (one @|@ per line) must parse to the same
-- three constructors.
testCtorsMultiline :: IO Bool
testCtorsMultiline =
  let raw = T.unlines
        [ "data Foo"
        , "  = Bar"
        , "  | Baz Int"
        , "  | Qux Int String"
        , "  \t-- Defined at src/Foo.hs:5:1"
        ]
  in pure $ case parseConstructors raw of
       [a, b, c] -> cName a == "Bar" && cName b == "Baz" && cName c == "Qux"
                 && length (cArgs c) == 2
       _         -> False

-- | Type synonyms have no @=@ constructor list; parser must return an
-- empty list rather than invent ctors.
testCtorsSynonym :: IO Bool
testCtorsSynonym =
  let raw = "type Alias = Int"
  in pure (null (parseConstructors raw))

testTemplate3 :: IO Bool
testTemplate3 =
  let ctors = [ Constructor "Bar" []
              , Constructor "Baz" ["Int"]
              , Constructor "Qux" ["Int", "String"]
              ]
      out   = renderTemplate "Foo" ctors
  in pure $
       "instance Arbitrary Foo where"              `T.isInfixOf` out
    && "pure Bar"                                  `T.isInfixOf` out
    && "Baz <$> arbitrary"                         `T.isInfixOf` out
    && "Qux <$> arbitrary <*> arbitrary"           `T.isInfixOf` out

testHoogleHit :: IO Bool
testHoogleHit =
  let line = "Prelude filter :: (a -> Bool) -> [a] -> [a]"
  in pure $ case parseHoogleLine line of
       Just h -> hhSignature h == "(a -> Bool) -> [a] -> [a]"
              && hhModule h    == Just "Prelude"
       _      -> False

testHoogleEmpty :: IO Bool
testHoogleEmpty =
  pure (isNothing (parseHoogleLine "No results found"))

--------------------------------------------------------------------------------
-- Phase 6: Coverage parser + PropertyStore roundtrip
--------------------------------------------------------------------------------

testCoverageFull :: IO Bool
testCoverageFull =
  let raw = T.unlines
        [ "100% expressions used (12/12)"
        , " 66% alternatives used (2/3)"
        , " 75% local declarations used (3/4)"
        , "100% top-level declarations used (5/5)"
        ]
  in pure $ case crMetrics (parseCoverage raw) of
       [a, b, c, d] ->
            mPercent a == 100 && mTotal a == 12
         && mPercent b == 66  && mCovered b == 2 && mTotal b == 3
         && mPercent c == 75
         && mPercent d == 100 && mLabel d == "top-level declarations used"
       _ -> False

testCoverageBanner :: IO Bool
testCoverageBanner =
  let raw = T.unlines
        [ "Cabal version 3.12 — banner without fraction"
        , "100% expressions used (1/1)"
        , ""
        ]
  in pure (length (crMetrics (parseCoverage raw)) == 1)

-- | Round-trip a property through the on-disk store. Uses a unique
-- temp project dir to keep repeated test runs independent.
testStoreRoundtrip :: IO Bool
testStoreRoundtrip = withTempProject $ \pd -> do
  store <- openStore pd
  save store "\\(xs :: [Int]) -> reverse (reverse xs) == xs" (Just "src/Foo.hs")
  props <- loadAll store
  pure $ case props of
    [p] -> spExpression p == "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
        && spModule p == Just "src/Foo.hs"
        && spPassed p == 1
    _   -> False

testStoreIncrement :: IO Bool
testStoreIncrement = withTempProject $ \pd -> do
  store <- openStore pd
  save store "prop_foo" Nothing
  save store "prop_foo" Nothing
  save store "prop_foo" Nothing
  props <- loadAll store
  pure $ case props of
    [p] -> spPassed p == 3
    _   -> False

--------------------------------------------------------------------------------
-- Phase 7: Deps validators + Goto parser
--------------------------------------------------------------------------------

testPkgAccepts :: IO Bool
testPkgAccepts = pure $ case validatePackageName "haskell-flows-mcp" of
  Right _ -> True
  _       -> False

testPkgRejectsSymbol :: IO Bool
testPkgRejectsSymbol = pure $ case validatePackageName "foo; rm -rf /" of
  Left _ -> True
  _      -> False

testPkgRejectsEmpty :: IO Bool
testPkgRejectsEmpty = pure $ case validatePackageName "" of
  Left _ -> True
  _      -> False

testVerAccepts :: IO Bool
testVerAccepts = pure $ case validateVersionConstraint ">= 2.14 && < 2.16" of
  Right _ -> True
  _       -> False

testVerRejects :: IO Bool
testVerRejects = pure $ case validateVersionConstraint "; rm -rf" of
  Left _ -> True
  _      -> False

testDefinedAtFile :: IO Bool
testDefinedAtFile =
  let raw = T.unlines
        [ "foo :: Int -> Int"
        , "  \t-- Defined at src/Foo.hs:12:5"
        ]
  in pure $ case parseDefinedAt raw of
       Just (InFile f l c) -> f == "src/Foo.hs" && l == 12 && c == 5
       _                   -> False

testDefinedAtModule :: IO Bool
testDefinedAtModule =
  let raw = "map :: (a -> b) -> [a] -> [b]\n  \t-- Defined in \x2018Prelude\x2019\n"
  in pure $ case parseDefinedAt raw of
       Just (InModule m) -> m == "Prelude"
       _                 -> False

testDefinedAtNone :: IO Bool
testDefinedAtNone =
  pure (isNothing (parseDefinedAt "just some text"))

--------------------------------------------------------------------------------
-- Phase 8: Refactor engines
--------------------------------------------------------------------------------

-- | The rename must rewrite @foo@ as a whole token, not substrings
-- inside @foobar@ or @myfoo@.
testRenameWordBoundary :: IO Bool
testRenameWordBoundary =
  let src = T.unlines
        [ "foo x = x + 1"
        , "foobar y = y"
        , "baz foo = foo + myfoo"
        ]
  in case renameInScope "foo" "quux" 1 10 src of
       Right rr ->
         pure $ rrOccurrences rr == 3   -- foo on lines 1, 3, 3
             && "foobar" `T.isInfixOf` rrNewContent rr   -- untouched
             && "myfoo"  `T.isInfixOf` rrNewContent rr   -- untouched
             && "quux"   `T.isInfixOf` rrNewContent rr
       _ -> pure False

testRenameIgnoresComments :: IO Bool
testRenameIgnoresComments =
  let src = T.unlines
        [ "-- here is foo in a comment"
        , "foo = 1"
        ]
  in case renameInScope "foo" "bar" 1 10 src of
       Right rr ->
         pure $ rrOccurrences rr == 1   -- only the binding
             && "foo in a comment" `T.isInfixOf` rrNewContent rr
       _ -> pure False

testRenameIgnoresStrings :: IO Bool
testRenameIgnoresStrings =
  let src = T.unlines
        [ "msg = \"the foo is here\""
        , "foo = 1"
        ]
  in case renameInScope "foo" "bar" 1 10 src of
       Right rr ->
         pure $ rrOccurrences rr == 1
             && "\"the foo is here\"" `T.isInfixOf` rrNewContent rr
       _ -> pure False

testRenameScoped :: IO Bool
testRenameScoped =
  let src = T.unlines
        [ "foo = 1"    -- line 1 — outside scope
        , "foo = 2"    -- line 2 — inside scope
        , "foo = 3"    -- line 3 — outside scope
        ]
  in case renameInScope "foo" "bar" 2 2 src of
       Right rr ->
         pure $ rrOccurrences rr == 1
             && rrTouchedLines rr == [2]
       _ -> pure False

testRenameSameName :: IO Bool
testRenameSameName =
  pure $ case renameInScope "foo" "foo" 1 10 "foo = 1" of
    Left _ -> True
    _      -> False

testIdentifierKeyword :: IO Bool
testIdentifierKeyword = pure $ case validateIdentifier "where" of
  Left _ -> True
  _      -> False

testIdentifierSymbol :: IO Bool
testIdentifierSymbol = pure $ case validateIdentifier "foo; rm" of
  Left _ -> True
  _      -> False

testIdentifierUpper :: IO Bool
testIdentifierUpper = pure $ case validateIdentifier "Foo" of
  Left _ -> True
  _      -> False

testExtractBinding :: IO Bool
testExtractBinding =
  let src = T.unlines
        [ "main = do"
        , "  let x = 1 + 2 + 3"
        , "  print x"
        ]
  in case extractBinding "sumSmall" 2 2 src of
       Right er ->
         pure $ "sumSmall" `T.isInfixOf` erNewContent er
             && "sumSmall =" `T.isInfixOf` erBindingTxt er
       _ -> pure False

testExtractEmpty :: IO Bool
testExtractEmpty =
  pure $ case extractBinding "foo" 5 4 "body" of
    Left _ -> True
    _      -> False

--------------------------------------------------------------------------------
-- Phase 9: Lint parser + Cabal validator + check_project + hole fits
--------------------------------------------------------------------------------

testHlintJson :: IO Bool
testHlintJson =
  let raw = T.pack
        "[{\"severity\":\"Warning\",\"hint\":\"Use isNothing\",\
        \\"file\":\"src/Foo.hs\",\"startLine\":10,\"startColumn\":5,\
        \\"from\":\"x == Nothing\",\"to\":\"isNothing x\"}]"
  in pure $ case parseHlintJson raw of
       [s] -> LintTool.sSeverity s == "Warning"
           && LintTool.sHint s     == "Use isNothing"
           && LintTool.sStartLine s == 10
       _ -> False

testDuplicateDeps :: IO Bool
testDuplicateDeps =
  let cabalBody = T.unlines
        [ "library"
        , "    build-depends: base, text, base"
        ]
  in pure $ any (("duplicate-dep" ==) . VC.iKind) (VC.scanCabalText cabalBody)

testMissingSynopsis :: IO Bool
testMissingSynopsis =
  let cabalBody = "cabal-version: 3.0\nname: foo\nversion: 0.1.0.0"
      issues    = VC.scanCabalText cabalBody
  in pure $ any (("missing-synopsis" ==) . VC.iKind) issues
         && any ((VC.CabalSevWarn ==) . VC.iSeverity) issues

testParseModules :: IO Bool
testParseModules =
  let cabalBody = T.unlines
        [ "library"
        , "  exposed-modules:  Foo.Bar"
        , "                    Foo.Baz"
        , "  other-modules:    Foo.Internal"
        , "  build-depends:    base"
        ]
      mods = parseExposedModules cabalBody
  in pure $ "Foo.Bar"      `elem` mods
         && "Foo.Baz"      `elem` mods
         && "Foo.Internal" `elem` mods

testValidFits :: IO Bool
testValidFits =
  let block = T.lines $ T.unlines
        [ "src/Foo.hs:5:5: warning: [GHC-88464]"
        , "    • Found hole: _ :: Int -> Int"
        , "    • Valid hole fits include"
        , "        id :: forall a. a -> a"
        , "        negate :: forall a. Num a => a -> a"
        ]
  in pure $ case extractValidFits block of
       [a, b] -> hfName a == "id"
              && hfName b == "negate"
              && "Num a" `T.isInfixOf` hfType b
       _      -> False

--------------------------------------------------------------------------------
-- Phase 10b: TypeSignature parser + rules catalog
--------------------------------------------------------------------------------

testSigSimple :: IO Bool
testSigSimple = pure $ case parseSignature "a -> a" of
  Just sig -> argCountOf sig == 1
           && isSameTypeThroughout sig
           && null (psConstraints sig)
  _        -> False
  where argCountOf = length . psArgs

testSigConstraint :: IO Bool
testSigConstraint =
  pure $ case parseSignature "Eq a => a -> a -> Bool" of
    Just sig -> psConstraints sig == ["Eq a"]
             && length (psArgs sig) == 2
             && psReturn sig == TyCon "Bool"
    _ -> False

testSigList :: IO Bool
testSigList =
  pure $ case parseSignature "[a] -> [a]" of
    Just sig -> psArgs sig == [TyList (TyVar "a")]
             && psReturn sig == TyList (TyVar "a")
             && isSameTypeThroughout sig
    _ -> False

testSuggestInvolutive :: IO Bool
testSuggestInvolutive =
  case parseSignature "a -> a" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "foo" sig
      in pure (any ((== "Involutive") . sLaw) suggestions
             && any ((== "Idempotent") . sLaw) suggestions)

testSuggestAssoc :: IO Bool
testSuggestAssoc =
  case parseSignature "a -> a -> a" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "op" sig
      in pure (any ((== "Associative") . sLaw) suggestions
             && any ((== "Commutative") . sLaw) suggestions)

testSuggestNoMatch :: IO Bool
testSuggestNoMatch =
  case parseSignature "Int -> String" of
    Nothing  -> pure False
    Just sig ->
      let suggestions = applyRules "foo" sig
      in pure (null suggestions)

--------------------------------------------------------------------------------
-- Phase 12 regression tests: dogfood findings #22 / #23 / #24
--------------------------------------------------------------------------------

-- | Issue #22: @ghci_batch@ advertises @{tool, args}@ via its
-- @inputSchema@ — parsing must accept that shape. This pins the
-- documented contract; a future regression flips this red instead
-- of silently misleading agents following the tool's own schema.
testBatchParsesToolArgs :: IO Bool
testBatchParsesToolArgs =
  let raw = object
        [ "actions" .=
            [ object
                [ "tool" .= ("ghci_type" :: Text)
                , "args" .= object [ "expression" .= ("reverse" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure
           ( tcName tc == "ghci_type"
           && tcArguments tc
                == object [ "expression" .= ("reverse" :: Text) ]
           )
         _ -> pure False
       A.Error _ -> pure False

-- | Issue #22 continued: clients that were relying on the MCP-native
-- shape @{name, arguments}@ (what @tools/call@ uses) keep working.
-- Accepting both shapes costs nothing — each routes through the
-- same dispatcher and per-tool validator downstream.
testBatchParsesNameArgs :: IO Bool
testBatchParsesNameArgs =
  let raw = object
        [ "actions" .=
            [ object
                [ "name"      .= ("ghci_eval" :: Text)
                , "arguments" .= object [ "expression" .= ("1+1" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure (tcName tc == "ghci_eval")
         _    -> pure False
       A.Error _ -> pure False

-- | Issue #23: @reverse :: [a] -> [a]@ fits the @a -> a@ shape that
-- 'ruleIdempotent' used to blindly promote to 'Medium'. Dampened
-- heuristic should either skip it or mark it 'Low' for a name with
-- no canonicalisation hint. Must never emit 'Medium' or 'High'.
testSuggestReverseIdempotentLow :: IO Bool
testSuggestReverseIdempotentLow =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let sugg = [ s | s <- applyRules "reverse" sig, sLaw s == "Idempotent" ]
      in pure $ case sugg of
           []  -> True
           [s] -> sConfidence s == Low
           _   -> False

-- | Issue #23: a name like @normalize@ — a strong canonicalisation
-- hint — should still surface Idempotent at 'Medium' even when
-- the shape is @[a] -> [a]@.
testSuggestNormalizeIdempotentMedium :: IO Bool
testSuggestNormalizeIdempotentMedium =
  case parseSignature "[a] -> [a]" of
    Nothing  -> pure False
    Just sig ->
      let sugg = [ s | s <- applyRules "normalize" sig, sLaw s == "Idempotent" ]
      in pure $ case sugg of
           [s] -> sConfidence s == Medium
           _   -> False

-- | Issue #24: @toolsActive@ in 'ghci_workflow' must enumerate the
-- same set of tools as @tools/list@. The two used to drift because
-- the list was hand-maintained in two places. Paranoia check: also
-- confirm every name is non-empty and the server registers more
-- than the 9-tool Phase-5 baseline.
testWorkflowToolsParity :: IO Bool
testWorkflowToolsParity = pure $
     length allToolNames == length allToolDescriptors
  && not (any T.null allToolNames)
  && length allToolNames >= 20

--------------------------------------------------------------------------------
-- Phase 11b regressions: ghci_deps F-01 / F-02 / F-03 fixes.
--------------------------------------------------------------------------------

-- | F-01: @ghci_deps add@ previously wrote @,@-prefixed continuation
-- lines at the same column as the @build-depends:@ field. Cabal 3.0
-- rejects that as a new field header and the file becomes
-- unparseable. Pin the invariant: after add, the inserted line's
-- leading whitespace must strictly exceed the header's indent.
testDepsAddIndentsForCabal :: IO Bool
testDepsAddIndentsForCabal =
  let body = T.unlines
        [ "library"
        , "    build-depends:    base >= 4.20 && < 5"
        ]
      newBody = addDep Nothing "QuickCheck" body
      isContComma ln = "," `T.isPrefixOf` T.stripStart ln
      commaLines = filter isContComma (T.lines newBody)
      headerIndent = 4  -- 4 spaces before "build-depends:"
  in pure $ case commaLines of
       [ln] ->
         let leading = T.length (T.takeWhile (== ' ') ln)
         in leading > headerIndent
       _ -> False

-- | F-02 (same root as F-01, framed positively): after @addDep@ on a
-- pristine scaffold, the entire file must not contain any line whose
-- first non-whitespace char is \",\" at column <= field indent.
-- This guards against future parser-confusing shapes.
testDepsAddNoTopComma :: IO Bool
testDepsAddNoTopComma =
  let body = T.unlines
        [ "cabal-version: 3.0"
        , "name: foo"
        , ""
        , "library"
        , "    build-depends:    base >= 4.20 && < 5"
        ]
      newBody  = addDep (Just ">= 2.14") "QuickCheck" body
      offenderLines =
        [ ln
        | ln <- T.lines newBody
        , let ws = T.length (T.takeWhile (== ' ') ln)
        , "," `T.isPrefixOf` T.stripStart ln
        , ws <= 4
        ]
  in pure (null offenderLines)

-- | F-03: with a stanza selector, @addDep@ must land in the
-- requested stanza, not the first @build-depends:@ it finds.
testDepsAddTargetsTestSuite :: IO Bool
testDepsAddTargetsTestSuite =
  let body = T.unlines
        [ "library"
        , "    build-depends:    base"
        , ""
        , "test-suite foo-test"
        , "    build-depends:    base"
        , "                    , foo"
        ]
      -- Scope everything through resolveStanza-style slicing by using
      -- applyWithinStanza through addDep at the tool layer; here we
      -- simulate via the exported primitive.
      newBody = case parseStanzaSelector "test-suite" of
        Left _ -> body
        Right sel ->
          let lns = T.lines body
              -- In-file-tool uses applyWithinStanza; mirror that with a
              -- direct slice call via the public API (parseStanzaSelector
              -- + addDep on the slice).
              (pre, stanzaLns, post) = sliceOrEmpty sel lns
              inner  = T.unlines stanzaLns
              inner' = addDep Nothing "QuickCheck" inner
          in T.unlines (pre <> T.lines inner' <> post)
      libDeps       = scopedDeps "library" newBody
      testSuiteDeps = scopedDeps "test-suite" newBody
  in pure $ "QuickCheck" `elem` testSuiteDeps
         && "QuickCheck" `notElem` libDeps
  where
    sliceOrEmpty sel lns =
      Data.Maybe.fromMaybe ([], lns, []) (lookupStanza sel lns)
    -- Tiny reimpl of the MCP's stanza slice, used only by this test.
    lookupStanza (kind, mName) lns =
      let match ln
            | not (T.null (T.takeWhile (== ' ') ln)) = False
            | otherwise =
                let s = T.strip ln
                    w = T.takeWhile (/= ' ') s
                    r = T.strip (T.dropWhile (/= ' ') s)
                in w == kind
                && case mName of
                     Nothing
                       | kind == "library" -> T.null r
                       | otherwise         -> True
                     Just name -> r == name
          isTop ln =
            T.null (T.takeWhile (== ' ') ln)
            && not (T.null (T.strip ln))
            && not (":" `T.isInfixOf` T.strip ln)
            && T.takeWhile (/= ' ') (T.strip ln)
                 `elem` topStanzaKinds
          topStanzaKinds =
            [ "library", "executable", "test-suite", "benchmark"
            , "foreign-library", "common", "flag", "source-repository"
            ]
      in case break match lns of
           (_,   [])     -> Nothing
           (pre, h : tl) ->
             let (body', post) = break isTop tl
             in Just (pre, h : body', post)
    scopedDeps kind body' =
      case parseStanzaSelector kind of
        Left _    -> []
        Right sel -> case lookupStanza sel (T.lines body') of
          Nothing       -> []
          Just (_, l, _) -> stanzaDeps (T.unlines l)
    stanzaDeps body' =
      -- same line-oriented parser used by the tool; inline-enough for
      -- the test by slicing the build-depends: line + continuations.
      let ls = T.lines body'
          rest = dropWhile (not . startsBuildDepends) ls
      in case rest of
           []     -> []
           (h:tl) ->
             let tailVal = T.strip (T.drop (T.length "build-depends:")
                                            (T.strip h))
                 cont    = takeWhile isContLine tl
                 joined  = T.intercalate " " (tailVal : map T.strip cont)
             in [ T.strip (T.takeWhile
                             (\c -> c /= ' ' && c /= '>' && c /= '<'
                                 && c /= '=' && c /= '^' && c /= '&')
                             (T.strip tok))
                | tok <- T.splitOn "," joined
                , not (T.null (T.strip tok))
                ]
    startsBuildDepends ln =
      "build-depends:" `T.isPrefixOf` T.stripStart (T.toLower ln)
    isContLine ln =
      not (T.null (T.takeWhile (== ' ') ln))
      && not (T.null (T.strip ln))

-- | Phase 11b: ensure the stanza selector parser accepts the forms
-- we advertise in the descriptor and rejects obvious garbage.
testParseStanzaAccepts :: IO Bool
testParseStanzaAccepts = pure $
     accepts "library"          ("library", Nothing)
  && accepts "test-suite"       ("test-suite", Nothing)
  && accepts "test-suite:foo"   ("test-suite", Just "foo")
  && accepts "executable:bar"   ("executable", Just "bar")
  where
    accepts raw expected = case parseStanzaSelector raw of
      Right got -> got == expected
      Left  _   -> False

testParseStanzaRejects :: IO Bool
testParseStanzaRejects = pure $
     rejects "foo-suite"          -- unknown kind
  && rejects "library:"           -- empty name after colon
  && rejects "test-suite:bad name" -- space in name
  && rejects "test-suite/$(id)"   -- shell metacharacters
  && rejects ""                   -- empty string
  where
    rejects raw = case parseStanzaSelector raw of
      Left  _ -> True
      Right _ -> False

-- | Phase 11b F-06: @cabal repl@ was spawned with no extra deps so
-- the GHCi session never had @Test.QuickCheck@ on its load path.
-- Calling @ghci_quickcheck@ against a scratch project that only
-- listed QuickCheck as a test-suite dep therefore failed with
-- \"Variable not in scope: quickCheck\". Fix attaches QuickCheck
-- via @--build-depends@ at session spawn time so the tool called
-- @ghci_quickcheck@ can actually… quickCheck. Pin the argv shape.
testSessionIncludesQuickCheck :: IO Bool
testSessionIncludesQuickCheck = pure $
     "repl"             `elem` sessionCabalArgs
  && "--build-depends"  `elem` sessionCabalArgs
  && "QuickCheck"       `elem` sessionCabalArgs

-- | Phase 11b F-04 part A: GHC 9.x emits a kind-signature line
-- (@type Run :: * -> *@) BEFORE the data decl in @:i@ output.
-- 'parseConstructors' previously bailed because @hasCtorHeader@
-- only checked the collapsed string's prefix. Pin the GHC 9.x layout
-- plus a record constructor with strict fields — must parse into a
-- 2-arg Constructor.
testCtorsRecordStrictWithKindHeader :: IO Bool
testCtorsRecordStrictWithKindHeader =
  let raw = T.unlines
        [ "type Run :: * -> *"
        , "data Run a = Run {runLen :: !Int, runVal :: !a}"
        , "  \t-- Defined at src/DogfoodRle.hs:20:1"
        ]
  in case parseConstructors raw of
       [c] -> pure (cName c == "Run" && length (cArgs c) == 2)
       _   -> pure False

-- | Phase 11b F-04 part B: even absent the kind header, a record
-- constructor @Ctor {f1 :: T1, f2 :: T2}@ used to be mis-tokenised
-- because @groupTokens@ didn't treat @{}@ as grouping — fields got
-- split on every internal space, inflating 'cArgs' to 6 tokens.
testCtorsInlineRecord2Fields :: IO Bool
testCtorsInlineRecord2Fields =
  let raw = "data Run a = Run {runLen :: !Int, runVal :: !a}"
  in case parseConstructors raw of
       [c] -> pure (cName c == "Run" && length (cArgs c) == 2)
       _   -> pure False

-- | Phase 11b F-05: @ghci_suggest@ used to emit false laws for
-- @encode :: [a] -> [Run a]@ because @ruleListLengthPreserving@ and
-- @ruleListRoundtrip@ matched @([TyList _], TyList _)@ without
-- checking the inner types. Both @Self-inverse on lists@ and
-- @Length preserving@ are nonsense (don't even type-check) when
-- arg and return lists carry different element types. Pin the
-- invariant: for @[a] -> [SomeOther a]@, neither rule fires.
testSuggestEncodeShapeSkipsListRules :: IO Bool
testSuggestEncodeShapeSkipsListRules =
  case parseSignature "[a] -> [Run a]" of
    Nothing  -> pure False
    Just sig ->
      let laws = map sLaw (applyRules "encode" sig)
      in pure $ "Self-inverse on lists" `notElem` laws
             && "Length preserving / non-extending" `notElem` laws

-- | Helper: create a fresh temp directory and delete it after the test.
-- Passes a validated 'ProjectDir' (absolute + normalised) to the body.
withTempProject :: (ProjectDir -> IO Bool) -> IO Bool
withTempProject k = do
  tmp <- getTemporaryDirectory
  ts  <- show <$> getTestTimestamp
  let dir = tmp </> ("haskell-flows-test-" <> ts)
  createDirectoryIfMissing True dir
  res <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> k pd
  removePathForcibly dir
  pure res

getTestTimestamp :: IO Int
getTestTimestamp = do
  t <- getPOSIXTime
  pure (floor (t * 1_000_000))
