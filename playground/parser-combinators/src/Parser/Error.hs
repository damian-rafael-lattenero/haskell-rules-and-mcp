module Parser.Error
  ( SourcePos(..)
  , ParseError(..)
  , initialPos
  , updatePos
  , showError
  , mergeErrors
  ) where

import Test.QuickCheck (Arbitrary(..))

-- | Source position tracking
data SourcePos = SourcePos
  { posLine   :: !Int
  , posColumn :: !Int
  } deriving (Show, Eq, Ord)

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary

-- | Parse error with position and context
data ParseError = ParseError
  { errorPos      :: SourcePos
  , errorExpected :: [String]
  , errorFound    :: Maybe Char
  } deriving (Show, Eq)

instance Arbitrary ParseError where
  arbitrary = ParseError <$> arbitrary <*> arbitrary <*> arbitrary

initialPos :: SourcePos
initialPos = SourcePos 1 1

updatePos :: Char -> SourcePos -> SourcePos
updatePos '\n' (SourcePos l _) = SourcePos (l + 1) 1
updatePos _    (SourcePos l c) = SourcePos l (c + 1)

showError :: ParseError -> String
showError (ParseError pos expected found) =
  "Parse error at line " ++ show (posLine pos)
    ++ ", column " ++ show (posColumn pos) ++ ":\n"
    ++ foundMsg found
    ++ expectedMsg expected
  where
    foundMsg Nothing  = "  unexpected end of input\n"
    foundMsg (Just c) = "  unexpected " ++ show c ++ "\n"
    expectedMsg [] = ""
    expectedMsg [x] = "  expected " ++ x ++ "\n"
    expectedMsg xs = "  expected one of: " ++ unwords xs ++ "\n"

mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors e1 e2 = ParseError
  { errorPos      = errorPos e1
  , errorExpected = errorExpected e1 ++ errorExpected e2
  , errorFound    = errorFound e1
  }
