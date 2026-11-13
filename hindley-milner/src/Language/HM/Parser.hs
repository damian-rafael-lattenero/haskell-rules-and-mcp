module Language.HM.Parser
  ( parseTerm
  , parseType
  , parseProgram
  , Parser
  ) where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import Language.HM.Types (Term(..), Type(..), Literal(..), TyVar, TyCon, Name, Pos(..), noPos)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Text Text

parseTerm :: Text -> Either (ParseErrorBundle Text Text) Term
parseTerm = parse (spaceConsumer *> pTerm <* eof) "<input>"

parseType :: Text -> Either (ParseErrorBundle Text Text) Type
parseType = parse (spaceConsumer *> pType <* eof) "<input>"

parseProgram :: Text -> Either (ParseErrorBundle Text Text) [Term]
parseProgram = parse (spaceConsumer *> many pTerm <* eof) "<input>"

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Term parser: parse a chain of atoms as left-associative application
pTerm :: Parser Term
pTerm = do
  pos <- getOffset
  atoms <- some pAtom
  pure $ foldl1 (\f a -> App f a (offsetToPos pos)) atoms

pAtom :: Parser Term
pAtom = choice
  [ pLit
  , try pAnn      -- Try annotation before variable to parse "x :: Int" as single atom
  , pVar
  , pLam
  , pLet
  , parens pTerm
  ]

pVar :: Parser Term
pVar = do
  pos <- getOffset
  name <- lexeme (notFollowedBy (symbol "=") *> notFollowedBy (symbol "in") *> notFollowedBy (symbol "let") *> pIdentifier)
  pure $ Var name (offsetToPos pos)

pLam :: Parser Term
pLam = do
  pos <- getOffset
  _ <- symbol "\\" <|> symbol "λ"
  name <- lexeme pIdentifier
  _ <- symbol "->"
  body <- pTerm
  pure $ Lam name body (offsetToPos pos)

pLet :: Parser Term
pLet = do
  pos <- getOffset
  _ <- symbol "let"
  name <- lexeme pIdentifier
  _ <- symbol "="
  rhs <- pTerm
  _ <- symbol "in"
  body <- pTerm
  pure $ Let name rhs body (offsetToPos pos)

pAnn :: Parser Term
pAnn = do
  pos <- getOffset
  -- For annotations, we need to parse a primary expression (not full pTerm to avoid ambiguity)
  e <- choice [pLit, pVar, parens pTerm]
  _ <- symbol "::"
  ty <- pType
  pure $ Ann e ty (offsetToPos pos)

pLit :: Parser Term
pLit = choice
  [ do
      pos <- getOffset
      i <- lexeme L.decimal
      pure $ Lit (LInt i) (offsetToPos pos)
  , do
      pos <- getOffset
      _ <- symbol "true"
      pure $ Lit (LBool True) (offsetToPos pos)
  , do
      pos <- getOffset
      _ <- symbol "false"
      pure $ Lit (LBool False) (offsetToPos pos)
  ]

pType :: Parser Type
pType = makeExprParser pTypeBase typeOperators

pTypeBase :: Parser Type
pTypeBase = choice
  [ TVar <$> lexeme pTypeVar
  , TCon <$> lexeme pTypeCon
  , parens pType
  ]

pTypeVar :: Parser TyVar
pTypeVar = do
  c <- lowerChar
  cs <- many alphaNumChar
  pure (c : cs)

pTypeCon :: Parser TyCon
pTypeCon = do
  c <- upperChar
  cs <- many alphaNumChar
  pure (c : cs)

pIdentifier :: Parser Name
pIdentifier = do
  c <- lowerChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_')
  pure (c : cs)

typeOperators :: [[Operator Parser Type]]
typeOperators =
  [ [ InfixR (do
        _ <- symbol "->"
        pure $ \t1 t2 -> TApp (TApp (TCon "->") t1) t2
      ) ]
  ]

offsetToPos :: Int -> Pos
offsetToPos _ = noPos