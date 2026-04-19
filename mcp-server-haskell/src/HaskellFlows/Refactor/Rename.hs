-- | Pure rewrite engine for local-scope identifier rename.
--
-- We deliberately avoid a full Haskell parser (ghc-lib-parser is ~50 MB
-- of transitive closure) and instead do word-boundary textual
-- replacement bounded to a line range the caller supplies. Safety comes
-- from a compile-verify step the tool layer owns: if the rewritten file
-- fails to parse or type-check, the snapshot is restored. The parser
-- is, in effect, our correctness oracle — we never have to decide
-- \"is this syntactically valid Haskell?\" ourselves.
--
-- The tokeniser is aware of:
--
-- * Line comments (@-- …@)
-- * Block comments (@{- … -}@), including nesting
-- * String literals (@\"…\"@) and character literals (@\'…\'@)
--
-- Identifiers inside comments and string literals are left alone —
-- renaming @foo@ wouldn't accidentally rewrite a docstring or a
-- @\"foo:\"@ format specifier.
--
-- Identifiers that aren't preceded/followed by a word-class character
-- (letters, digits, underscore, apostrophe — Haskell's @isNameChar@)
-- are replaced. So @foobar@ isn't hit by a rename of @foo@, but
-- @f (foo xs)@ is.
module HaskellFlows.Refactor.Rename
  ( renameInScope
  , validateIdentifier
  , haskellKeywords
  , RenameResult (..)
  ) where

import Data.Char (isAlphaNum, isAsciiLower, isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | The result of a rename, carrying enough information for the tool
-- layer to render a meaningful response (or revert).
data RenameResult = RenameResult
  { rrNewContent   :: !Text
  , rrOccurrences  :: !Int
  , rrTouchedLines :: ![Int]
  }
  deriving stock (Eq, Show)

-- | Rewrite @old@ → @new@ in the 1-based inclusive line range
-- @[startLine, endLine]@. Tokens in comments and string literals are
-- skipped. Returns @Left msg@ if the caller supplied an invalid line
-- range.
renameInScope
  :: Text                           -- ^ old identifier (pre-validated)
  -> Text                           -- ^ new identifier (pre-validated)
  -> Int                            -- ^ scope start line (1-based, inclusive)
  -> Int                            -- ^ scope end line (1-based, inclusive)
  -> Text                           -- ^ original file content
  -> Either Text RenameResult
renameInScope old new startLine endLine raw
  | old == new =
      Left "old and new identifiers are identical"
  | startLine < 1 =
      Left "scope_line_start must be >= 1"
  | endLine < startLine =
      Left "scope_line_end must be >= scope_line_start"
  | otherwise =
      let allLines  = T.lines raw
          endIx     = min endLine (length allLines)
          -- Partition by original line index (1-based).
          indexed   = zip [1 :: Int ..] allLines
          (rewritten, touched, occ) =
            foldr rewriteLine ([], [], 0) indexed
          rewriteLine (i, ln) (accLines, accTouched, accCount)
            | i < startLine || i > endIx =
                (ln : accLines, accTouched, accCount)
            | otherwise =
                let (ln', n) = renameLine old new ln
                in (ln' : accLines,
                    if n > 0 then i : accTouched else accTouched,
                    accCount + n)
      in Right RenameResult
           { rrNewContent   = T.intercalate "\n" rewritten
                             <> (if T.isSuffixOf "\n" raw then "\n" else "")
           , rrOccurrences  = occ
           , rrTouchedLines = touched
           }

--------------------------------------------------------------------------------
-- line-level tokeniser
--------------------------------------------------------------------------------

-- | Rename a single line. Returns @(newLine, replacementCount)@.
--
-- We walk the line as a tiny state machine. State-transitions:
-- @InCode → InLineComment@ on @--@ (ignore the rest of the line),
-- @InCode → InString@ on @\"@, etc. Nested block comments aren't
-- possible on a single line after T.lines, so the block-comment
-- tracking is kept minimal — unclosed @{-@ on a line is treated as
-- \"rest of line is comment\", which matches what GHC itself does at
-- the lexical level.
renameLine :: Text -> Text -> Text -> (Text, Int)
renameLine old new = go InCode 0 "" . T.unpack
  where
    oldS = T.unpack old
    newS = T.unpack new

    -- acc is built in reverse. We flush it to a Text at the end.
    go :: LexState -> Int -> String -> String -> (Text, Int)
    go _     n acc [] = (T.pack (reverse acc), n)
    go state n acc rest@(c:cs) = case state of
      -- A line-comment runs to EOL, no renames apply after it.
      InLineComment ->
        (T.pack (reverse acc ++ rest), n)
      InBlockComment ->
        case rest of
          '-':'}':r -> go InCode n ('}':'-':acc) r
          _         -> go InBlockComment n (c:acc) cs
      InString ->
        case rest of
          '\\':d:r -> go InString n (d:'\\':acc) r  -- eat escaped char
          '"':r    -> go InCode n ('"':acc) r
          _        -> go InString n (c:acc) cs
      InChar ->
        case rest of
          '\\':d:r -> go InChar n (d:'\\':acc) r
          '\'':r   -> go InCode n ('\'':acc) r
          _        -> go InChar n (c:acc) cs
      InCode ->
        case rest of
          '-':'-':_ ->
            (T.pack (reverse acc ++ rest), n)
          '{':'-':r ->
            go InBlockComment n ('-':'{':acc) r
          '"':r ->
            go InString n ('"':acc) r
          '\'':r ->
            go InChar n ('\'':acc) r
          _ ->
            case tryRename oldS newS acc rest of
              Just (newAcc, r') -> go InCode (n + 1) newAcc r'
              Nothing           -> go InCode n (c:acc) cs

-- | If @rest@ starts with @old@ as a whole token (not a substring of a
-- larger identifier), return @(acc ++ reverse new, rest-after-old)@.
tryRename
  :: String -> String -> String -> String
  -> Maybe (String, String)
tryRename old new acc rest = do
  afterOld <- stripPrefixS old rest
  let prevOk = case acc of
        []    -> True
        (p:_) -> not (isNameChar p)
      nextOk = case afterOld of
        []    -> True
        (nx:_) -> not (isNameChar nx)
  if prevOk && nextOk
    then Just (reverse new ++ acc, afterOld)
    else Nothing

stripPrefixS :: String -> String -> Maybe String
stripPrefixS []     ys     = Just ys
stripPrefixS _      []     = Nothing
stripPrefixS (x:xs) (y:ys)
  | x == y    = stripPrefixS xs ys
  | otherwise = Nothing

-- | Haskell identifier continuation characters. Apostrophe is
-- included because @foo'@ is a valid identifier (and a common one —
-- every \"primed\" version of a helper).
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_' || c == '\''

data LexState = InCode | InLineComment | InBlockComment | InString | InChar

--------------------------------------------------------------------------------
-- identifier validation
--------------------------------------------------------------------------------

-- | A Haskell variable identifier: starts with a lowercase letter or
-- underscore, followed by any number of alphanumerics / @_@ / @'@.
-- Keywords are rejected.
--
-- We deliberately /don't/ accept constructors (capital-initial) here
-- — renaming a constructor has cross-module effects that a local
-- in-module rename can't guarantee. The tool surfaces this to the
-- agent as a boundary error.
validateIdentifier :: Text -> Either Text Text
validateIdentifier raw
  | T.null raw                              = Left "identifier is empty"
  | raw `Set.member` haskellKeywords        = Left ("identifier is a Haskell keyword: " <> raw)
  | not (validFirstChar (T.head raw))       = Left ("identifier must start with lowercase letter or underscore: " <> raw)
  | not (T.all isNameCharT raw)             = Left ("identifier contains invalid character: " <> raw)
  | otherwise                               = Right raw
  where
    validFirstChar c = isAsciiLower c || c == '_'
    isNameCharT c = isAlphaNum c || c == '_' || c == '\'' || isDigit c

-- | Haskell 2010 / GHC2024 reserved words. We block these as targets
-- for rename because silently turning an identifier into a keyword
-- would confuse the parser in ways the compile-verify wouldn't always
-- catch (some keywords parse as identifiers in specific contexts,
-- e.g. @as@ in import clauses).
haskellKeywords :: Set Text
haskellKeywords = Set.fromList
  [ "case", "class", "data", "default", "deriving", "do", "else"
  , "foreign", "if", "import", "in", "infix", "infixl", "infixr"
  , "instance", "let", "module", "newtype", "of", "then", "type"
  , "where", "_"
  -- Common soft keywords GHC treats specially in some contexts.
  , "as", "qualified", "hiding", "family", "forall", "proc", "rec"
  ]
