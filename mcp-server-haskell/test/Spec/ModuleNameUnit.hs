-- | Unit tests for 'Parser.ModuleName': validateModuleName, bulk
-- validation, reserved keywords, and error rendering. All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.ModuleNameUnit
  ( testValidModuleNameSingle
  , testValidModuleNameDotted
  , testValidModuleNameUnderscore
  , testValidModuleNameApostrophe
  , testValidModuleNameDigits
  , testValidModuleNameTrim
  , testInvalidLowercaseModule
  , testInvalidReservedBare
  , testInvalidReservedSecond
  , testInvalidEmpty
  , testInvalidWhitespace
  , testInvalidTrailingDot
  , testInvalidLeadingDot
  , testInvalidDoubleDot
  , testInvalidLeadingDigit
  , testInvalidHyphen
  , testInvalidSpace
  , testValidateBulkOrderPreserved
  , testValidateBulkAllGood
  , testValidateBulkAllBad
  , testValidateBulkTrimsAccepted
  , testReservedKeywordsAllRejected
  , testReservedKeywordsCoverIssueList
  , testReservedKeywordsCaseSensitive
  , testRenderErrorActionable
  , testRenderErrorReservedSuggests
  , testRenderErrorEmptySegment
  , testRenderErrorInvalidChar
  , testRenderErrorAllNonEmpty
  ) where

import qualified Data.Text as T
import qualified Data.Set as Set

import HaskellFlows.Parser.ModuleName
  ( ModuleNameError (..)
  , isReservedKeyword
  , renderModuleNameError
  , reservedKeywords
  , validateModuleName
  , validateModuleNames
  )

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

testValidModuleNameSingle :: IO Bool
testValidModuleNameSingle = pure $
  validateModuleName "Foo" == Right "Foo"

-- | Happy path: dotted multi-segment name.
testValidModuleNameDotted :: IO Bool
testValidModuleNameDotted = pure $
  validateModuleName "Foo.Bar.Baz" == Right "Foo.Bar.Baz"

-- | Happy path: underscores AFTER the first letter are legal.
testValidModuleNameUnderscore :: IO Bool
testValidModuleNameUnderscore = pure $
  validateModuleName "Foo_Bar.Baz_Qux" == Right "Foo_Bar.Baz_Qux"

-- | Happy path: apostrophes (Haskell prime convention) are legal.
testValidModuleNameApostrophe :: IO Bool
testValidModuleNameApostrophe = pure $
  validateModuleName "Foo'.Bar''" == Right "Foo'.Bar''"

-- | Happy path: digits AFTER the first character are legal.
testValidModuleNameDigits :: IO Bool
testValidModuleNameDigits = pure $
  validateModuleName "Foo123.B4r" == Right "Foo123.B4r"

-- | The validator trims surrounding whitespace and returns the
-- canonicalised form — the handler uses the returned 'Text', so a
-- trailing space can't survive into the @.cabal@.
testValidModuleNameTrim :: IO Bool
testValidModuleNameTrim = pure $
  validateModuleName "  Foo.Bar  " == Right "Foo.Bar"

-- | The exact bug from issue #47: lowercase first segment leaks
-- through line-based handlers and parse-corrupts the @.cabal@.
-- The first failure encountered is the lowercase first segment;
-- the second segment ('module', a reserved keyword) is also bad
-- but the validator stops at the first error, which is the more
-- actionable diagnostic for the LLM.
testInvalidLowercaseModule :: IO Bool
testInvalidLowercaseModule = case validateModuleName "lowercase.module" of
  Left (MNESegmentLeadingNotUpper raw seg) ->
    pure (raw == "lowercase.module" && seg == "lowercase")
  _ -> pure False

-- | Reserved-keyword rejection: 'module' as a bare name fires the
-- keyword check, NOT the lowercase-leading check (the keyword check
-- is intentionally first so the agent sees the more actionable
-- error message).
testInvalidReservedBare :: IO Bool
testInvalidReservedBare = case validateModuleName "module" of
  Left (MNESegmentReserved raw seg) ->
    pure (raw == "module" && seg == "module")
  _ -> pure False

-- | Reserved keyword in the SECOND segment ('Foo.module') — the
-- canonical second-segment-keyword case the issue calls out.
testInvalidReservedSecond :: IO Bool
testInvalidReservedSecond = case validateModuleName "Foo.module" of
  Left (MNESegmentReserved raw seg) ->
    pure (raw == "Foo.module" && seg == "module")
  _ -> pure False

-- | Empty input.
testInvalidEmpty :: IO Bool
testInvalidEmpty = pure (validateModuleName "" == Left MNEEmpty)

-- | Whitespace-only input — same behaviour as empty (after strip).
testInvalidWhitespace :: IO Bool
testInvalidWhitespace = pure (validateModuleName "   \t\n  " == Left MNEEmpty)

-- | Trailing dot produces an empty segment.
testInvalidTrailingDot :: IO Bool
testInvalidTrailingDot = case validateModuleName "Foo." of
  Left (MNESegmentEmpty raw) -> pure (raw == "Foo.")
  _                          -> pure False

-- | Leading dot produces an empty segment.
testInvalidLeadingDot :: IO Bool
testInvalidLeadingDot = case validateModuleName ".Foo" of
  Left (MNESegmentEmpty raw) -> pure (raw == ".Foo")
  _                          -> pure False

-- | Doubled dot produces an empty segment in the middle.
testInvalidDoubleDot :: IO Bool
testInvalidDoubleDot = case validateModuleName "Foo..Bar" of
  Left (MNESegmentEmpty raw) -> pure (raw == "Foo..Bar")
  _                          -> pure False

-- | Leading digit on the first segment.
testInvalidLeadingDigit :: IO Bool
testInvalidLeadingDigit = case validateModuleName "1Foo" of
  Left (MNESegmentLeadingDigit raw seg) ->
    pure (raw == "1Foo" && seg == "1Foo")
  _ -> pure False

-- | Hyphen in name — common mistake porting Cabal package names
-- (which DO use hyphens) into module names (which don't).
testInvalidHyphen :: IO Bool
testInvalidHyphen = case validateModuleName "Foo-Bar" of
  Left (MNESegmentInvalidChar raw seg c) ->
    pure (raw == "Foo-Bar" && seg == "Foo-Bar" && c == '-')
  _ -> pure False

-- | Space in name — almost always a copy-paste accident.
testInvalidSpace :: IO Bool
testInvalidSpace = case validateModuleName "Foo Bar" of
  Left (MNESegmentInvalidChar raw _ c) ->
    pure (raw == "Foo Bar" && c == ' ')
  _ -> pure False

-- | Bulk validator preserves order in BOTH partitions.
testValidateBulkOrderPreserved :: IO Bool
testValidateBulkOrderPreserved =
  let (rejected, accepted) = validateModuleNames
        ["A", "lowercase", "B", "Foo.module", "C"]
      rejectedNames = map fst rejected
  in pure
       (  accepted == ["A", "B", "C"]
       && rejectedNames == ["lowercase", "Foo.module"]
       )

-- | Bulk validator on all-good input yields no rejections.
testValidateBulkAllGood :: IO Bool
testValidateBulkAllGood =
  let (rejected, accepted) = validateModuleNames ["Foo", "Foo.Bar", "Baz"]
  in pure (null rejected && accepted == ["Foo", "Foo.Bar", "Baz"])

-- | Bulk validator on all-bad input yields no acceptances.
testValidateBulkAllBad :: IO Bool
testValidateBulkAllBad =
  let (rejected, accepted) = validateModuleNames ["1Foo", "lowercase", ""]
      rejectedNames        = map fst rejected
  in pure
       (  null accepted
       && rejectedNames == ["1Foo", "lowercase", ""]
       )

-- | Bulk validator preserves trim-canonicalisation on accepted entries.
testValidateBulkTrimsAccepted :: IO Bool
testValidateBulkTrimsAccepted =
  let (rejected, accepted) = validateModuleNames ["  Foo  ", " Bar "]
  in pure (null rejected && accepted == ["Foo", "Bar"])

-- | Every keyword in 'reservedKeywords' is rejected when used as a
-- bare single-segment name. Pins the keyword set: a future change
-- that adds (e.g.) 'forall' must also extend this assertion.
testReservedKeywordsAllRejected :: IO Bool
testReservedKeywordsAllRejected =
  pure $ all rejected (Set.toList reservedKeywords)
  where
    rejected kw = case validateModuleName kw of
      Left (MNESegmentReserved _ seg) -> seg == kw
      _ -> False

-- | The keyword list specifically covers the names called out in
-- issue #47. We pin them explicitly so a refactor that drops one
-- (e.g. dropping 'instance' by accident) is caught here, not in
-- production via a corrupted .cabal.
testReservedKeywordsCoverIssueList :: IO Bool
testReservedKeywordsCoverIssueList =
  pure $ all isReservedKeyword
    [ "module", "where", "let", "case", "do", "if", "then", "else"
    , "class", "instance", "data", "type", "newtype", "default"
    , "deriving", "import", "infix", "infixl", "infixr"
    ]

-- | Predicate: 'isReservedKeyword' is case-sensitive — uppercase
-- 'Module' is a legal module name (and indeed common for utility
-- modules).
testReservedKeywordsCaseSensitive :: IO Bool
testReservedKeywordsCaseSensitive = pure $
     not (isReservedKeyword "Module")
  && not (isReservedKeyword "Where")
  &&     isReservedKeyword "module"

-- | Rendered error mentions the offending input + a suggested fix
-- so the LLM can self-correct without another round-trip.
testRenderErrorActionable :: IO Bool
testRenderErrorActionable =
  let msg = renderModuleNameError
              (MNESegmentLeadingNotUpper "lowercase.module" "lowercase")
  in pure
       (  T.isInfixOf "lowercase.module" msg
       && T.isInfixOf "lowercase"        msg
       && (T.isInfixOf "Did you mean"     msg
           || T.isInfixOf "uppercase"      msg)
       )

-- | Rendered keyword error names the keyword AND offers a renamed
-- suggestion (e.g. 'moduleMod') so the agent has a concrete fix.
testRenderErrorReservedSuggests :: IO Bool
testRenderErrorReservedSuggests =
  let msg = renderModuleNameError (MNESegmentReserved "Foo.module" "module")
  in pure
       (  T.isInfixOf "module"          msg
       && T.isInfixOf "reserved"        msg
       && (T.isInfixOf "Mod"            msg
           || T.isInfixOf "rename"      (T.toLower msg))
       )

-- | Rendered empty-segment error mentions the canonical fix shape
-- "Foo.Bar" so the agent doesn't have to look up the grammar.
testRenderErrorEmptySegment :: IO Bool
testRenderErrorEmptySegment =
  let msg = renderModuleNameError (MNESegmentEmpty "Foo..Bar")
  in pure
       (  T.isInfixOf "Foo..Bar"     msg
       && T.isInfixOf "empty segment" msg
       && T.isInfixOf "Foo.Bar"       msg
       )

-- | Rendered invalid-char error names the offending character so
-- the LLM doesn't need to scan the input to find it.
testRenderErrorInvalidChar :: IO Bool
testRenderErrorInvalidChar =
  let msg = renderModuleNameError (MNESegmentInvalidChar "Foo-Bar" "Foo-Bar" '-')
  in pure
       (  T.isInfixOf "Foo-Bar" msg
       && T.isInfixOf "'-'"     msg
       )

-- | Property-shaped: the rendered error message is non-empty for
-- every error constructor — guards against future refactors that
-- might leave a constructor unhandled in 'renderModuleNameError'.
testRenderErrorAllNonEmpty :: IO Bool
testRenderErrorAllNonEmpty =
  let inputs =
        [ MNEEmpty
        , MNESegmentEmpty "Foo."
        , MNESegmentReserved "module" "module"
        , MNESegmentLeadingNotUpper "foo" "foo"
        , MNESegmentLeadingDigit "1Foo" "1Foo"
        , MNESegmentInvalidChar "Foo-" "Foo-" '-'
        ]
  in pure (not (any (T.null . renderModuleNameError) inputs))
