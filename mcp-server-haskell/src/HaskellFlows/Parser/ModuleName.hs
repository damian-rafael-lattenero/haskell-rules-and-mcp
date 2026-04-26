-- | Boundary validator for Haskell module names — rejects names that
-- would otherwise parse-corrupt the project's @.cabal@ when fed
-- through 'HaskellFlows.Tool.AddModules' / 'RemoveModules'.
--
-- Issue #47 (dogfood 2026-04-24): @ghc_add_modules([\"lowercase.module\"])@
-- happily registered the name into @exposed-modules@, breaking
-- every downstream tool that touches the @.cabal@ (every
-- @ghc_load@, @ghc_check_*@, @ghc_quickcheck@, @ghc_validate_cabal@
-- failed with cryptic stanza-flag errors). The fix is a single-pass
-- identifier check at the tool boundary, before any file scaffolding
-- or cabal mutation. The check is purely lexical — no GHC API call,
-- no IO — so it stays cheap on the hot path.
--
-- Grammar enforced (Haskell 2010, Section 5.1):
--
-- @
-- modid     ::= conid ('.' conid)*
-- conid     ::= 'A'..'Z' { 'A'..'Z' | 'a'..'z' | '0'..'9' | \\'\\\\\\'\\' | '_' }
-- @
--
-- and additionally we refuse any segment that is a Haskell reserved
-- keyword (Section 2.4). Strictly, the uppercase-first rule already
-- rules out keywords (every reserved word is lowercase), but the
-- keyword check fires FIRST so the agent sees the actionable
-- diagnostic \"'module' is a reserved keyword\" instead of the
-- merely-true \"'module' starts with a non-uppercase character\".
module HaskellFlows.Parser.ModuleName
  ( ModuleNameError (..)
  , validateModuleName
  , validateModuleNames
  , renderModuleNameError
  , reservedKeywords
  , isReservedKeyword
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Why a module name is rejected. Each constructor carries the
-- ORIGINAL input the caller supplied so error messages can quote it
-- back unambiguously — vague \"name invalid\" messages are useless
-- to LLM callers.
data ModuleNameError
  = MNEEmpty
    -- ^ Whole input is empty / whitespace-only.
  | MNESegmentEmpty !Text
    -- ^ Empty segment, e.g. @\"Foo..Bar\"@, @\".Foo\"@, @\"Foo.\"@.
    -- Carries the original input.
  | MNESegmentReserved !Text !Text
    -- ^ A segment is a Haskell reserved keyword. Carries
    -- (input, offending-segment).
  | MNESegmentLeadingNotUpper !Text !Text
    -- ^ A segment does not start with an uppercase ASCII letter.
    -- Carries (input, offending-segment).
  | MNESegmentLeadingDigit !Text !Text
    -- ^ A segment starts with a digit. Carries
    -- (input, offending-segment).
  | MNESegmentInvalidChar !Text !Text !Char
    -- ^ A segment contains a character outside @[A-Za-z0-9'_]@.
    -- Carries (input, offending-segment, offending-character).
  deriving stock (Eq, Show)

-- | Haskell 2010 reserved keywords (Section 2.4 of the Report).
-- Module names, libraries, and source files alike must NOT use
-- these as a path segment — they're either invalid as identifiers
-- or so confusing they're worth refusing on principle (e.g. the
-- @module@ keyword inside a module name).
--
-- Kept as a 'Set' for O(log n) membership and stable rendering in
-- error messages (no order quirks). The list is closed under
-- Haskell 2010; modern GHC extensions (PatternSynonyms's @pattern@,
-- TypeFamilies's @family@, etc.) are CONTEXTUAL keywords, not
-- reserved, so they remain legal as identifier segments.
reservedKeywords :: Set Text
reservedKeywords = Set.fromList
  [ "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "type"
  , "where"
  , "_"
  ]

-- | Predicate form of 'reservedKeywords' membership.
isReservedKeyword :: Text -> Bool
isReservedKeyword s = s `Set.member` reservedKeywords

-- | Validate a single module name. On success returns the trimmed
-- canonical form — callers should USE the returned 'Text' so
-- trailing whitespace doesn't survive the boundary into the
-- @.cabal@ or filesystem path.
--
-- The validator is total (no exceptions) and pure (no IO).
validateModuleName :: Text -> Either ModuleNameError Text
validateModuleName raw
  | T.null trimmed = Left MNEEmpty
  | otherwise =
      case findFirstError trimmed (T.splitOn "." trimmed) of
        Just e  -> Left e
        Nothing -> Right trimmed
  where
    trimmed = T.strip raw

-- | Bulk-validate a list. Returns @(rejected, accepted)@ — both
-- preserve their original input order. Empty rejected list means
-- the caller can proceed; non-empty means the caller MUST refuse
-- the whole batch (we never silently drop entries — a partial
-- success would leave the caller's worldview inconsistent with the
-- file system).
validateModuleNames :: [Text] -> ([(Text, ModuleNameError)], [Text])
validateModuleNames = foldr step ([], [])
  where
    step m (bad, good) = case validateModuleName m of
      Left e   -> ((m, e) : bad, good)
      Right ok -> (bad,           ok : good)

findFirstError :: Text -> [Text] -> Maybe ModuleNameError
findFirstError raw = go
  where
    go []           = Nothing
    go (seg : rest) = case validateSegment raw seg of
      Just e  -> Just e
      Nothing -> go rest

validateSegment :: Text -> Text -> Maybe ModuleNameError
validateSegment raw seg
  | T.null seg                  = Just (MNESegmentEmpty raw)
  | isReservedKeyword seg       = Just (MNESegmentReserved raw seg)
  | otherwise = case T.uncons seg of
      Nothing -> Just (MNESegmentEmpty raw)
      Just (c, rest)
        | isDigit c             -> Just (MNESegmentLeadingDigit raw seg)
        | not (isAsciiUpper c)  -> Just (MNESegmentLeadingNotUpper raw seg)
        | otherwise -> case T.find (not . isModChar) rest of
            Just bad -> Just (MNESegmentInvalidChar raw seg bad)
            Nothing  -> Nothing

-- | Characters legal AFTER the leading uppercase letter of a
-- module-name segment. ASCII only — Unicode identifiers are
-- legal in Haskell but our @hs-source-dirs@ + filesystem path
-- mapping assumes ASCII paths and would corrupt cross-platform.
isModChar :: Char -> Bool
isModChar c =
     isAsciiUpper c
  || isAsciiLower c
  || isDigit c
  || c == '_'
  || c == '\''

-- | Format a 'ModuleNameError' for the agent. Always includes:
--
--   * the offending input (so the LLM knows exactly which name
--     to fix),
--   * the specific reason (so the LLM doesn't have to guess), and
--   * a hint or suggested fix (so the LLM can self-correct
--     without another round-trip — round-trips cost tokens).
--
-- The shape is a single user-facing 'Text' string, suitable for
-- the @\"error\"@ field of a tool-result payload.
renderModuleNameError :: ModuleNameError -> Text
renderModuleNameError = \case
  MNEEmpty ->
    "module name is empty or whitespace-only — Haskell module \
    \names must be one or more non-empty segments separated by \
    \'.', e.g. \"Foo\" or \"Foo.Bar.Baz\"."

  MNESegmentEmpty raw ->
    "module name '" <> raw <> "' has an empty segment — names \
    \like \"Foo..Bar\", \".Foo\", or \"Foo.\" are not valid \
    \Haskell module identifiers. Use \"Foo.Bar\" instead."

  MNESegmentReserved raw seg ->
    "module name '" <> raw <> "' contains the reserved Haskell \
    \keyword '" <> seg <> "' — keywords like 'module', 'where', \
    \'class', 'data', 'type', etc. cannot appear as a module-name \
    \segment. Rename that segment (e.g. '" <> seg <> "Mod' or '" <>
    capitalise seg <> "Kind')."

  MNESegmentLeadingNotUpper raw seg ->
    "module name '" <> raw <> "' has segment '" <> seg <> "' \
    \starting with a non-uppercase character — every Haskell \
    \module-name segment must begin with an uppercase ASCII \
    \letter (A-Z). Did you mean '" <> capitalise seg <> "'?"

  MNESegmentLeadingDigit raw seg ->
    "module name '" <> raw <> "' has segment '" <> seg <> "' \
    \starting with a digit — Haskell identifiers cannot start \
    \with 0-9. Reorder the segment so a letter comes first \
    \(e.g. 'V" <> seg <> "')."

  MNESegmentInvalidChar raw seg c ->
    "module name '" <> raw <> "' has segment '" <> seg <> "' \
    \with invalid character " <> renderChar c <> " — Haskell \
    \module-name segments may use only A-Z, a-z, 0-9, underscore, \
    \and apostrophe."

renderChar :: Char -> Text
renderChar c = T.pack ['\'', c, '\'']

-- | Best-effort \"capitalise the first letter\" suggestion. ASCII
-- only — the character set we already enforce in 'isModChar'.
capitalise :: Text -> Text
capitalise t = case T.uncons t of
  Nothing        -> t
  Just (c, rest)
    | isAsciiLower c -> T.cons (toUpper c) rest
    | otherwise      -> t
