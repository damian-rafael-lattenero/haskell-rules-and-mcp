module Parser.Error
  ( Pos(..)
  , ParseError(..)
  , Expected(..)
  , initialPos
  , advancePos
  , mergeError
  ) where

import Test.QuickCheck (Arbitrary(..), oneof, elements, listOf, suchThat)

-- | Source position: line and column (1-based)
data Pos = Pos
  { posLine   :: !Int
  , posColumn :: !Int
  } deriving (Show, Eq, Ord)

-- | What the parser expected at the error location
data Expected
  = ExpectedChar Char
  | ExpectedString String
  | ExpectedSatisfy String   -- description of predicate
  | ExpectedEOF
  | ExpectedOneOf [Expected]
  deriving (Show, Eq)

-- | A parse error with position and expectations
data ParseError = ParseError
  { errorPos      :: !Pos
  , errorExpected :: [Expected]
  , errorMessage  :: Maybe String
  } deriving (Show, Eq)

-- | Starting position (line 1, column 1)
initialPos :: Pos
initialPos = Pos 1 1

-- | Advance position by one character
advancePos :: Pos -> Char -> Pos
advancePos (Pos line _col) '\n' = Pos (line + 1) 1
advancePos (Pos line col)  '\t' = Pos line (col + 8 - ((col - 1) `mod` 8))
advancePos (Pos line col)  _    = Pos line (col + 1)

-- | Merge two errors: keep the one at the furthest position,
--   or merge expectations if at the same position
mergeError :: ParseError -> ParseError -> ParseError
mergeError e1 e2
  | errorPos e1 > errorPos e2 = e1
  | errorPos e1 < errorPos e2 = e2
  | otherwise = ParseError
      { errorPos      = errorPos e1
      , errorExpected = errorExpected e1 ++ errorExpected e2
      , errorMessage  = errorMessage e1 <> errorMessage e2
      }

-- Arbitrary instances for QuickCheck

instance Arbitrary Pos where
  arbitrary = Pos <$> pos <*> pos
    where pos = arbitrary `suchThat` (> 0)

instance Arbitrary Expected where
  arbitrary = oneof
    [ ExpectedChar <$> arbitrary
    , ExpectedString <$> listOf arbitrary
    , ExpectedSatisfy <$> listOf (elements ['a'..'z'])
    , pure ExpectedEOF
    ]

instance Arbitrary ParseError where
  arbitrary = ParseError <$> arbitrary <*> arbitrary <*> arbitrary
