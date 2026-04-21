module Expr.Pretty
  ( pretty
  , parse
  , maxInputSize
  , maxParseDepth
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Expr.Syntax (Error (..), Expr (..))

maxInputSize :: Int
maxInputSize = 4096

maxParseDepth :: Int
maxParseDepth = 64

pretty :: Expr -> String
pretty = prettyPrec 0

prettyPrec :: Int -> Expr -> String
prettyPrec _ (Lit n)
  | n < 0     = "(" ++ show n ++ ")"
  | otherwise = show n
prettyPrec _ (Var v)   = v
prettyPrec p (Neg e)   = paren (p > 2) ("-" ++ prettyPrec 3 e)
prettyPrec p (Add l r) = paren (p > 0) (prettyPrec 0 l ++ " + " ++ prettyPrec 1 r)
prettyPrec p (Mul l r) = paren (p > 1) (prettyPrec 1 l ++ " * " ++ prettyPrec 2 r)

paren :: Bool -> String -> String
paren True s  = "(" ++ s ++ ")"
paren False s = s

parse :: String -> Either Error Expr
parse input
  | length input > maxInputSize = Left SizeExceeded
  | otherwise = case parseExpr maxParseDepth (dropWs input) of
      Left err        -> Left err
      Right (e, rest) -> case dropWs rest of
        "" -> Right e
        _  -> Left ParseError

dropWs :: String -> String
dropWs = dropWhile (\c -> c == ' ' || c == '\t')

parseExpr :: Int -> String -> Either Error (Expr, String)
parseExpr d s = do
  (l, s1) <- parseMul d s
  parseAddTail d l s1

parseAddTail :: Int -> Expr -> String -> Either Error (Expr, String)
parseAddTail d acc s = case dropWs s of
  ('+':rest) -> do
    (r, s1) <- parseMul d rest
    parseAddTail d (Add acc r) s1
  _          -> Right (acc, s)

parseMul :: Int -> String -> Either Error (Expr, String)
parseMul d s = do
  (l, s1) <- parseNeg d s
  parseMulTail d l s1

parseMulTail :: Int -> Expr -> String -> Either Error (Expr, String)
parseMulTail d acc s = case dropWs s of
  ('*':rest) -> do
    (r, s1) <- parseNeg d rest
    parseMulTail d (Mul acc r) s1
  _          -> Right (acc, s)

parseNeg :: Int -> String -> Either Error (Expr, String)
parseNeg d s = case dropWs s of
  ('-':rest)
    | d <= 0    -> Left DepthExceeded
    | otherwise -> do
        (e, s1) <- parseNeg (d - 1) rest
        Right (Neg e, s1)
  _ -> parseAtom d s

parseAtom :: Int -> String -> Either Error (Expr, String)
parseAtom d s = case dropWs s of
  ('(':rest)
    | d <= 0    -> Left DepthExceeded
    | otherwise -> do
        (e, s1) <- parseExpr (d - 1) rest
        case dropWs s1 of
          (')':rest2) -> Right (e, rest2)
          _           -> Left ParseError
  s'@(c:_)
    | isDigit c -> parseIntTok s'
    | isAlpha c -> parseVarTok s'
  _ -> Left ParseError

parseIntTok :: String -> Either Error (Expr, String)
parseIntTok s =
  let (digs, rest) = span isDigit s
  in  case reads digs :: [(Integer, String)] of
        [(n, "")]
          | n > toInteger (maxBound :: Int) -> Left Overflow
          | otherwise                       -> Right (Lit (fromInteger n), rest)
        _                                   -> Left ParseError

parseVarTok :: String -> Either Error (Expr, String)
parseVarTok s =
  let (name, rest) = span isAlphaNum s
  in  Right (Var name, rest)
