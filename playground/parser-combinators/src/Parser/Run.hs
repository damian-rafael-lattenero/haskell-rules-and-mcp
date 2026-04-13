module Parser.Run
  ( parse
  , parseAll
  , formatError
  -- * Example: arithmetic expression parser
  , Expr(..)
  , expr
  , eval
  ) where

import Control.Applicative ((<|>))
import Parser.Error (Pos(..), ParseError(..), Expected(..))
import Parser.Core (Parser(..), ParseResult(..), mkState, eof)
import Parser.Char (char, digit, spaces)
import Parser.Combinators (between, chainl1)

-- | Run a parser on a string, return Either
parse :: Parser a -> String -> Either ParseError a
parse p input = case runParser p (mkState input) of
  Success a _  -> Right a
  Failure e    -> Left e

-- | Run a parser, requiring it to consume all input
parseAll :: Parser a -> String -> Either ParseError a
parseAll p input = parse (p <* eof) input

-- | Format a ParseError as a human-readable string
formatError :: String -> ParseError -> String
formatError source (ParseError pos expected msg) =
  let header = "Parse error at line " ++ show (posLine pos)
            ++ ", column " ++ show (posColumn pos) ++ ":"
      srcLine = if posLine pos <= length (lines source) && not (null source)
                then "\n  " ++ lines source !! (posLine pos - 1)
                  ++ "\n  " ++ replicate (posColumn pos - 1) ' ' ++ "^"
                else ""
      expectedStr = case expected of
        [] -> ""
        _  -> "\n  Expected: " ++ formatExpected expected
      msgStr = case msg of
        Nothing -> ""
        Just m  -> "\n  " ++ m
  in header ++ srcLine ++ expectedStr ++ msgStr

formatExpected :: [Expected] -> String
formatExpected []  = "unknown"
formatExpected [e] = showExpected e
formatExpected es  = concatMap (\e -> showExpected e ++ ", ") (init es)
                  ++ "or " ++ showExpected (last es)

showExpected :: Expected -> String
showExpected (ExpectedChar c)      = show c
showExpected (ExpectedString s)    = show s
showExpected (ExpectedSatisfy d)   = d
showExpected ExpectedEOF           = "end of input"
showExpected (ExpectedOneOf es)    = "one of: " ++ formatExpected es

-- ============================================================
-- Example: Simple arithmetic expression parser
-- ============================================================

-- | AST for arithmetic expressions
data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- | Evaluate an expression
eval :: Expr -> Maybe Int
eval (Lit n)   = Just n
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = eval b >>= \bv ->
  if bv == 0 then Nothing else div <$> eval a <*> Just bv

-- | Parse an integer (one or more digits)
integer :: Parser Expr
integer = Lit . read <$> some digit <* spaces
  where some p = (:) <$> p <*> many p
        many p = some p <|> pure []

-- | Parse a parenthesized expression or integer
factor :: Parser Expr
factor = spaces *> (parens <|> integer)
  where parens = between (char '(' <* spaces) (char ')' <* spaces) expr

-- | Parse multiplication/division (higher precedence)
term :: Parser Expr
term = chainl1 factor mulOp
  where mulOp = (char '*' <* spaces *> pure Mul)
            <|> (char '/' <* spaces *> pure Div)

-- | Parse addition/subtraction (lower precedence)
expr :: Parser Expr
expr = chainl1 term addOp
  where addOp = (char '+' <* spaces *> pure Add)
            <|> (char '-' <* spaces *> pure Sub)
