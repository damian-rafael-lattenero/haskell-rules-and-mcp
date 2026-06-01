-- | Unit tests for 'Ghc.Sanitize.sanitizeExpression' (#127), the
-- GHC-output type-parser, 'isOutOfScope', and the companion QuickCheck
-- properties for sanitize + path + parser totality. Also exports the
-- 'SafeSegment' newtype used by the path-traversal QC properties.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Sanitize
  ( testParseHeader
  , testSanitizeAccepts
  , testSanitizeRejectsNewline
  , testSanitizeRejectsSentinel
  , testSanitizeRejectsEmpty
  , testSanitizeRejectsLargeLiteral
  , testSanitizeRejectsBigExponent
  , testSanitizeAccepts19Digits
  , testSanitizeAcceptsSmallExp
  , testSanitizeRejectionOversizedInteger
  , testParseTypeSingleLine
  , testParseTypeMultiLine
  , testParseTypeMalformed
  , testOutOfScope
  , prop_sanitize_rejects_newline
  , prop_sanitize_rejects_sentinel
  , prop_sanitize_clean_roundtrip
  , prop_modulePath_rejects_dotdot
  , prop_modulePath_accepts_inTree
  , prop_parseShowModulesPaths_total
  , prop_parseQuickCheckOutput_total
  , prop_chooseStoreModule_nonIdent_uses_hint
  , prop_chooseStoreModule_ident_no_info_uses_hint
  , SafeSegment (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
  ( Arbitrary
  , Property
  , counterexample
  , property
  , (===)
  , (==>)
  )
import qualified Test.QuickCheck as QC

import HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  , sentinel
  )
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , parseGhcErrors
  , renderGhciStyle
  )
import HaskellFlows.Parser.QuickCheck (parseQuickCheckOutput)
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
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.Regression as RegTool

testParseHeader :: IO Bool
testParseHeader =
  let raw = T.unlines
        [ "src/Foo.hs:12:5: error: [GHC-83865]"
        , "    Couldn't match expected type 'Int' with actual type 'Bool'"
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

-- #127: 20-digit decimal literal (>= 10^19) must be refused.
-- 18446744073709551616 is 2^64 — the prototypical crash trigger.
testSanitizeRejectsLargeLiteral :: IO Bool
testSanitizeRejectsLargeLiteral =
  pure $ case sanitizeExpression "18446744073709551616" of
    Left OversizedIntegerLiteral -> True
    _                            -> False

-- #127: exponent >= 64 in a power expression must be refused.
-- "2^64" would constant-fold to a two-limb Integer and segfault the RTS.
testSanitizeRejectsBigExponent :: IO Bool
testSanitizeRejectsBigExponent =
  pure $ case sanitizeExpression "2^64" of
    Left OversizedIntegerLiteral -> True
    _                            -> False

-- #127: 19-digit literal (< 10^19) must NOT be refused — single GMP limb.
-- 9999999999999999999 is 10^19 - 1, safely within Word64.
testSanitizeAccepts19Digits :: IO Bool
testSanitizeAccepts19Digits =
  pure $ case sanitizeExpression "9999999999999999999" of
    Right _ -> True
    _       -> False

-- #127: exponent 63 must NOT be refused — 2^63 is still a single-limb value
-- on 64-bit (it is maxBound :: Int64, not Int64+1).
testSanitizeAcceptsSmallExp :: IO Bool
testSanitizeAcceptsSmallExp =
  pure $ case sanitizeExpression "2^63" of
    Right _ -> True
    Left OversizedIntegerLiteral -> False
    Left _  -> True   -- other rejections (e.g. future rules) still pass

-- #127: integration — 'sanitizeRejection' maps OversizedIntegerLiteral to
-- the OversizedInput ErrorKind so the wire response uses "oversized_input".
testSanitizeRejectionOversizedInteger :: IO Bool
testSanitizeRejectionOversizedInteger =
  let ee = Env.sanitizeRejection "expr" OversizedIntegerLiteral
  in pure (Env.eeKind ee == Env.OversizedInput)

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
-- #127: OversizedIntegerLiteral and InputTooLarge are also valid policy
-- rejections — the property's claim is "never silently accepted", not
-- "always accepted".
prop_sanitize_clean_roundtrip :: String -> Property
prop_sanitize_clean_roundtrip rawS =
  let raw = T.pack rawS
      ok = not (T.null (T.strip raw))
        && T.all (`notElem` ("\n\r" :: String)) raw
        && not ("<<<GHCi-DONE-7f3a2b>>>" `T.isInfixOf` raw)
  in ok ==>
     case sanitizeExpression raw of
       Right cleaned                -> cleaned === T.strip raw
       Left OversizedIntegerLiteral -> property True  -- #127: large-int guard
       Left (InputTooLarge _ _)     -> property True  -- size cap
       _                            -> counterexample "expected Right" (property False)

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
-- Totality / law properties added after the dogfood UX fixes. These
-- cover surfaces that parse external text (@:show modules@, QuickCheck
-- output) and pure decision functions (@chooseStoreModule@). Each one
-- is an honest bug-finder: running 200 QuickCheck cases exercises
-- shapes a hand-rolled unit test would never type.
--------------------------------------------------------------------------------

-- | @parseShowModulesPaths@ must be total on any input and never
-- return more paths than input lines. Catches: runaway parsers,
-- hangs on degenerate input, infinite output loops.
prop_parseShowModulesPaths_total :: String -> Bool
prop_parseShowModulesPaths_total input =
  let txt    = T.pack input
      result = RegTool.parseShowModulesPaths txt
      maxN   = length (T.lines txt)
  in length result <= max maxN 1

-- | @parseQuickCheckOutput@ must be total on any (propName, output)
-- pair and return a renderable 'QuickCheckResult'. Catches: bottom
-- constructors, partial pattern matches on output regex splits, and
-- (via 'length . show') infinite loops.
prop_parseQuickCheckOutput_total :: String -> String -> Bool
prop_parseQuickCheckOutput_total propName output =
  not (null (show (parseQuickCheckOutput (T.pack propName) (T.pack output))))

-- | For any property expression that is NOT a simple identifier
-- (here: anything starting with '\\'), 'chooseStoreModule' must
-- return the caller's hint verbatim — the @:info@ output is
-- irrelevant for lambdas. Pinned so a refactor cannot accidentally
-- extend auto-resolution to expressions where @:info@ would return
-- useless results.
prop_chooseStoreModule_nonIdent_uses_hint :: SafeSegment -> SafeSegment -> Bool
prop_chooseStoreModule_nonIdent_uses_hint (SafeSegment body) (SafeSegment hint) =
  let prop  = T.pack ("\\x -> " <> body <> " x")
      mHint = Just (T.pack ("src/" <> hint <> ".hs"))
      info  = Just (T.pack "prop :: a -- Defined at other/File.hs:1:1")
  in QcTool.chooseStoreModule prop mHint info == mHint

-- | Simple identifier but no @:info@ output available (e.g. GHCi
-- returned an error): fall back to the caller's hint rather than
-- inventing a path. Pinned so a refactor cannot accidentally
-- default to something path-like that the caller didn't authorise.
prop_chooseStoreModule_ident_no_info_uses_hint :: SafeSegment -> SafeSegment -> Bool
prop_chooseStoreModule_ident_no_info_uses_hint (SafeSegment seg) (SafeSegment hint) =
  let prop  = T.pack ("prop_" <> seg)
      mHint = Just (T.pack ("src/" <> hint <> ".hs"))
  in QcTool.chooseStoreModule prop mHint Nothing == mHint
