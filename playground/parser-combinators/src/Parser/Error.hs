module Parser.Error
  ( Pos(..)
  , ParseError(..)
  , initialPos
  , advancePos
  , formatError
  , unexpectedErr
  , expectedErr
  , mergeErrors
  ) where

import Data.List (intercalate, nub)
import Test.QuickCheck (Arbitrary(..))

data Pos = Pos
  { posLine :: !Int
  , posCol  :: !Int
  } deriving (Show, Eq, Ord)

instance Arbitrary Pos where
  arbitrary = Pos <$> arbitrary <*> arbitrary

data ParseError = ParseError
  { errPos      :: Pos
  , errExpected :: [String]
  , errFound    :: String
  } deriving (Show, Eq)

instance Arbitrary ParseError where
  arbitrary = ParseError <$> arbitrary <*> arbitrary <*> arbitrary

initialPos :: Pos
initialPos = Pos 1 1

advancePos :: Pos -> Char -> Pos
advancePos (Pos line _) '\n' = Pos (line + 1) 1
advancePos (Pos line col) '\t' = Pos line (col + 8 - ((col - 1) `mod` 8))
advancePos (Pos line col) _    = Pos line (col + 1)

formatError :: ParseError -> String
formatError (ParseError pos expected found) =
  "(line " ++ show (posLine pos) ++ ", column " ++ show (posCol pos) ++ "):\n"
  ++ "unexpected " ++ found ++ "\n"
  ++ if null expected
     then ""
     else "expecting " ++ formatExpected (nub expected)

formatExpected :: [String] -> String
formatExpected []     = ""
formatExpected [x]    = x
formatExpected [x, y] = x ++ " or " ++ y
formatExpected xs     = intercalate ", " (init xs) ++ ", or " ++ last xs

unexpectedErr :: Pos -> String -> ParseError
unexpectedErr pos found = ParseError pos [] found

expectedErr :: Pos -> String -> ParseError
expectedErr pos expected = ParseError pos [expected] ""

mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors e1 e2
  | errPos e1 > errPos e2 = e1
  | errPos e1 < errPos e2 = e2
  | otherwise = ParseError (errPos e1)
      (nub (errExpected e1 ++ errExpected e2))
      (if null (errFound e1) then errFound e2 else errFound e1)
