module Parser.Error
  ( Pos(..)
  , ParseError(..)
  , initialPos
  , updatePos
  , formatError
  ) where

import Test.QuickCheck (Arbitrary(..), elements, listOf)

-- | Source position: line and column
data Pos = Pos
  { posLine   :: !Int
  , posColumn :: !Int
  } deriving (Show, Eq, Ord)

instance Arbitrary Pos where
  arbitrary = Pos <$> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)
    where getPositive = abs . (+ 1)

-- | What went wrong during parsing
data ParseError = ParseError
  { errorPos      :: Pos
  , errorExpected :: [String]
  , errorFound    :: String
  } deriving (Show, Eq)

instance Arbitrary ParseError where
  arbitrary = ParseError
    <$> arbitrary
    <*> listOf (elements ["digit", "letter", "symbol", "'('", "')'", "eof"])
    <*> elements ["end of input", "'x'", "'1'", "'+'"]

-- | Starting position (line 1, column 1)
initialPos :: Pos
initialPos = Pos 1 1

-- | Advance position by one character (newline resets column)
updatePos :: Pos -> Char -> Pos
updatePos (Pos line _) '\n' = Pos (line + 1) 1
updatePos (Pos line col) '\t' = Pos line (col + 8 - ((col - 1) `mod` 8))
updatePos (Pos line col) _    = Pos line (col + 1)

-- | Human-readable error message
formatError :: ParseError -> String
formatError (ParseError pos expected found) =
  "(line " ++ show (posLine pos) ++ ", column " ++ show (posColumn pos) ++ "):\n"
  ++ "  unexpected " ++ found ++ "\n"
  ++ case expected of
       []  -> ""
       [e] -> "  expecting " ++ e
       es  -> "  expecting one of: " ++ joinWith ", " es
  where
    joinWith _ []     = ""
    joinWith _ [x]    = x
    joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs
