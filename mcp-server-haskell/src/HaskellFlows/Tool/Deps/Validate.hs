-- | Boundary-validation helpers for @ghc_deps@: package names,
-- version constraints, and stanza selectors.
--
-- All functions are pure. No IO, no external dependencies beyond
-- @base@ and @text@.
module HaskellFlows.Tool.Deps.Validate
  ( validatePackageName
  , validateVersionConstraint
  , parseStanzaSelector
  , renderSelector
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- boundary validation
--------------------------------------------------------------------------------

-- | A Hackage package name is @[A-Za-z][A-Za-z0-9-]*@. Any other
-- character (including whitespace and meta-characters) is rejected.
validatePackageName :: Text -> Either Text Text
validatePackageName raw
  | T.null raw                = Left "package name is empty"
  | not (T.all okChar raw)    = Left ("invalid character in package name: " <> raw)
  | not (firstIsLetter raw)   = Left "package name must start with a letter"
  | otherwise                 = Right raw
  where
    okChar c = isAlphaNum c || c == '-'
    firstIsLetter t = case T.uncons t of
      Just (c, _) -> isAlphaNum c && not (isDigit c)
      Nothing     -> False

-- | Cabal version constraints are a tiny language: @>=@, @<=@, @<@,
-- @>@, @==@, @^>=@, conjunctions via @&&@, numeric @X.Y.Z@ versions,
-- and whitespace. We accept only those characters — no shell
-- metacharacters, no identifiers.
validateVersionConstraint :: Text -> Either Text Text
validateVersionConstraint raw
  | T.null stripped          = Left "version constraint is empty"
  | T.any (not . okChar) raw = Left ("invalid character in version constraint: " <> raw)
  | otherwise                = Right stripped
  where
    stripped = T.strip raw
    okChar c = isDigit c
            || isSpace c
            || c `elem` (".<>=^&" :: String)

--------------------------------------------------------------------------------
-- stanza selector
--------------------------------------------------------------------------------

-- | Parse a stanza selector from the agent into @(kind, maybe-name)@.
--
-- Accepted shapes:
--
-- * @library@
-- * @test-suite@            — first occurrence
-- * @test-suite:NAME@
-- * @executable@ / @executable:NAME@
-- * @benchmark@ / @benchmark:NAME@
-- * @foreign-library@ / @foreign-library:NAME@
--
-- Validation is strict: only alphanumerics, @-@, @_@, @:@ pass. Shell
-- metacharacters, path separators, whitespace are all rejected — the
-- string never reaches a shell (no spawns here) but defence in depth
-- is cheap.
parseStanzaSelector :: Text -> Either Text (Text, Maybe Text)
parseStanzaSelector raw
  | T.null stripped            = Left "stanza is empty"
  | T.any (not . okChar) stripped =
      Left ("invalid character in stanza selector: " <> raw)
  | otherwise = case T.splitOn ":" stripped of
      [kind]
        | kind `elem` allowedKinds -> Right (kind, Nothing)
        | otherwise                -> Left ("unknown stanza kind: " <> kind)
      [kind, name]
        | kind `elem` allowedKinds
        , not (T.null name)
        , T.all isIdChar name
        , isIdFirst (T.head name)
            -> Right (kind, Just name)
        | otherwise -> Left ("invalid stanza: " <> raw)
      _   -> Left ("invalid stanza format: " <> raw)
  where
    stripped     = T.strip raw
    allowedKinds =
      [ "library", "test-suite", "executable"
      , "benchmark", "foreign-library"
      ]
    okChar c   = isAlphaNum c || c == '-' || c == '_' || c == ':'
    isIdChar c = isAlphaNum c || c == '-' || c == '_'
    isIdFirst c = isAlphaNum c && not (isDigit c)

-- | Render a parsed stanza selector back to the @kind@ or @kind:name@
-- string form.
renderSelector :: (Text, Maybe Text) -> Text
renderSelector (k, Nothing)   = k
renderSelector (k, Just name) = k <> ":" <> name
