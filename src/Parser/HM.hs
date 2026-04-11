module Parser.HM
  ( parseExpr
  , parseType
  , parseProgram
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isLower, isAlphaNum)

import Parser.Core
import Parser.Char
import Parser.Combinators

import HM.Syntax

-- | Reserved words in the HM language
reservedWords :: [String]
reservedWords = ["let", "rec", "in", "if", "then", "else", "true", "false", "fst", "snd"]

-- | Parse an identifier that is not a reserved word
ident :: Parser String
ident = do
  name <- identifier
  if name `elem` reservedWords
    then failWith ("unexpected keyword '" ++ name ++ "'")
    else pure name

-- | Levenshtein edit distance between two strings
editDistance :: String -> String -> Int
editDistance [] ys = length ys
editDistance xs [] = length xs
editDistance (x:xs) (y:ys)
  | x == y    = editDistance xs ys
  | otherwise = 1 + minimum [ editDistance xs (y:ys)   -- delete
                             , editDistance (x:xs) ys   -- insert
                             , editDistance xs ys        -- replace
                             ]

-- | Suggest a "did you mean?" if an identifier is close to a reserved word
suggestKeyword :: String -> String
suggestKeyword name =
  case [(w, d) | w <- reservedWords, let d = editDistance name w, d <= 2, d > 0] of
    [] -> ""
    xs -> let (best, _) = minimumBy (\a b -> compare (snd a) (snd b)) xs
          in " (did you mean '" ++ best ++ "'?)"

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ [x]    = x
minimumBy f (x:xs) = foldl (\a b -> if f a b == GT then b else a) x xs
minimumBy _ []     = error "minimumBy: empty list"

----------------------------------------------------------------------
-- Type parser
----------------------------------------------------------------------

-- | Parse a type expression (exported)
parseType :: Parser Type
parseType = typeExpr

-- | Type expression: typeAtom ( '->' typeExpr )?
typeExpr :: Parser Type
typeExpr = do
  t <- typeAtom
  (do _ <- symbol "->"; t2 <- typeExpr; pure (TArr t t2)) <|> pure t

-- | Atomic type: Int, Bool, type variable, or parenthesized
typeAtom :: Parser Type
typeAtom = typeCon <|> typeParen <|> typeVar

-- | Type constructor: Int, Bool, etc.
typeCon :: Parser Type
typeCon = do
  name <- upperIdentifier
  case name of
    "Int"  -> pure (TCon "Int")
    "Bool" -> pure (TCon "Bool")
    _      -> failWith ("unknown type constructor: " ++ name)

-- | Type variable (lowercase identifier)
typeVar :: Parser Type
typeVar = TVar <$> identifier

-- | Parenthesized type: grouping or product
typeParen :: Parser Type
typeParen = do
  _ <- symbol "("
  t <- typeExpr
  (do _ <- comma; t2 <- typeExpr; _ <- symbol ")"; pure (TProd t t2))
    <|> (symbol ")" *> pure t)

----------------------------------------------------------------------
-- Expression parser
----------------------------------------------------------------------

-- | Parse a complete expression from a string (exported)
parseProgram :: String -> Either ParseError Expr
parseProgram input = case parse (spaces *> expr <* eof) input of
  Right e  -> Right e
  Left err -> Left (addTypoHints input err)

-- | Post-process a parse error: scan input for identifiers close to keywords
addTypoHints :: String -> ParseError -> ParseError
addTypoHints input err =
  let words' = extractIdents input
      hints  = [(w, suggestKeyword w) | w <- words'
                                       , not (w `elem` reservedWords)
                                       , not (null (suggestKeyword w))]
  in case hints of
    ((w, hint):_) -> err { peExpected = peExpected err ++ " ['" ++ w ++ "'" ++ hint ++ "]" }
    _             -> err

-- | Extract all lowercase-starting identifiers from input
extractIdents :: String -> [String]
extractIdents [] = []
extractIdents (c:cs)
  | isLower c = let (w, rest) = span (\x -> isAlphaNum x || x == '_' || x == '\'') cs
                in (c:w) : extractIdents rest
  | otherwise = extractIdents cs

-- | Expression parser (exported)
parseExpr :: Parser Expr
parseExpr = expr

-- | Top-level expression
expr :: Parser Expr
expr = letExpr <|> lamExpr <|> ifExpr <|> opExpr

----------------------------------------------------------------------
-- Operator precedence chain (low to high)
----------------------------------------------------------------------

-- | Helper: build infix application from operator string
binOp :: String -> Expr -> Expr -> Expr
binOp op a b = EApp (EApp (EVar op) a) b

-- | Precedence 2: ||  (right-associative)
opExpr :: Parser Expr
opExpr = chainr1 andExpr (binOp "||" <$ operator "||")

-- | Precedence 3: &&  (right-associative)
andExpr :: Parser Expr
andExpr = chainr1 cmpExpr (binOp "&&" <$ operator "&&")

-- | Precedence 4: ==, /=, <, >, <=, >=  (left-associative, single comparison)
cmpExpr :: Parser Expr
cmpExpr = chainl1 addExpr cmpOp
  where
    cmpOp = (binOp "==" <$ operator "==")
        <|> (binOp "/=" <$ operator "/=")
        <|> (binOp "<=" <$ operator "<=")
        <|> (binOp ">=" <$ operator ">=")
        <|> (binOp "<"  <$ operator "<")
        <|> (binOp ">"  <$ operator ">")

-- | Precedence 6: +, -  (left-associative)
addExpr :: Parser Expr
addExpr = chainl1 mulExpr addOp
  where
    addOp = (binOp "+" <$ operator "+")
        <|> (binOp "-" <$ operator "-")

-- | Precedence 7: *  (left-associative)
mulExpr :: Parser Expr
mulExpr = chainl1 compExpr (binOp "*" <$ operator "*")

-- | Precedence 9: .  (right-associative, function composition)
compExpr :: Parser Expr
compExpr = chainr1 appExpr (binOp "." <$ operator ".")

-- | Let / letrec expression, supports multiple bindings: let x = 1; y = 2 in body
letExpr :: Parser Expr
letExpr = do
  reserved "let"
  isRec <- (True <$ reserved "rec") <|> pure False
  bindings <- sepBy1 binding semicolon
  reserved "in"
  body <- expr
  pure (desugarLet isRec bindings body)
  where
    binding = do
      name <- ident
      _ <- symbol "="
      e <- expr
      pure (name, e)
    desugarLet isRec bs body = foldr wrap body bs
      where wrap (n, e) = if isRec then ELetRec n e else ELet n e

-- | Lambda expression: \x y z -> body  (multi-arg, desugars to nested ELam)
lamExpr :: Parser Expr
lamExpr = do
  _ <- symbol "\\"
  xs <- some ident
  _ <- symbol "->"
  body <- expr
  pure (foldr ELam body xs)

-- | If-then-else expression
ifExpr :: Parser Expr
ifExpr = do
  reserved "if"
  cond <- expr
  reserved "then"
  thn <- expr
  reserved "else"
  els <- expr
  pure (EIf cond thn els)

-- | Application: one or more atoms, left-associative
appExpr :: Parser Expr
appExpr = do
  atoms <- some atom
  pure (foldl1 EApp atoms)

-- | Atomic expression
atom :: Parser Expr
atom = litBool <|> litInt <|> fstExpr <|> sndExpr <|> var <|> parenExpr

-- | Boolean literal
litBool :: Parser Expr
litBool = (ELit (LBool True) <$ reserved "true")
      <|> (ELit (LBool False) <$ reserved "false")

-- | Integer literal
litInt :: Parser Expr
litInt = ELit . LInt <$> natural

-- | Variable (not a keyword)
var :: Parser Expr
var = EVar <$> ident

-- | fst applied to an atom
fstExpr :: Parser Expr
fstExpr = do
  reserved "fst"
  e <- atom
  pure (EFst e)

-- | snd applied to an atom
sndExpr :: Parser Expr
sndExpr = do
  reserved "snd"
  e <- atom
  pure (ESnd e)

-- | Parenthesized expression: grouping, pair, or annotation
parenExpr :: Parser Expr
parenExpr = do
  _ <- symbol "("
  e <- expr
  -- Try pair, then annotation, then plain grouping
  (do _ <- comma; e2 <- expr; _ <- symbol ")"; pure (EPair e e2))
    <|> (do _ <- symbol ":"; t <- typeExpr; _ <- symbol ")"; pure (EAnn e t))
    <|> (symbol ")" *> pure e)

