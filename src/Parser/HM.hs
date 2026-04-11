module Parser.HM
  ( parseExpr
  , parseType
  , parseProgram
  ) where

import Control.Applicative (Alternative(..))

import Parser.Core
import Parser.Char

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
parseProgram = parse (spaces *> expr <* eof)

-- | Expression parser (exported)
parseExpr :: Parser Expr
parseExpr = expr

-- | Top-level expression
expr :: Parser Expr
expr = letExpr <|> lamExpr <|> ifExpr <|> appExpr

-- | Let / letrec expression
letExpr :: Parser Expr
letExpr = do
  reserved "let"
  isRec <- (True <$ reserved "rec") <|> pure False
  name <- ident
  _ <- symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  pure $ if isRec then ELetRec name e1 e2 else ELet name e1 e2

-- | Lambda expression
lamExpr :: Parser Expr
lamExpr = do
  _ <- symbol "\\"
  x <- ident
  _ <- symbol "->"
  body <- expr
  pure (ELam x body)

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
