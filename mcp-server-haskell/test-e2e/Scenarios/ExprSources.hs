-- | Embedded source files the E2E scenario writes into the test
-- project. Kept as plain 'Text' constants so the test file has a
-- single translation unit — no @data-files@ ceremony in cabal,
-- no path-resolution fragility at runtime.
--
-- The modules match the dogfood spec exactly:
--
--   Expr.Syntax    — Expr / Error / Env ADTs.
--   Expr.Eval      — pure evaluator: @Env -> Expr -> Either Error Int@.
--   Expr.Simplify  — @simplify@ normaliser; sound w.r.t. @eval@.
--   Expr.Pretty    — pretty-printer + parser with roundtrip.
--
-- And the test-support:
--
--   Gen            — sized Arbitrary Expr + re-exports of every
--                    top-level so @:browse Gen@ sees all five as
--                    same-module siblings (what enables the
--                    sibling-aware suggest engine to fire).
module Scenarios.ExprSources
  ( syntaxSrc
  , evalSrc
  , simplifySrc
  , prettySrc
  , genSrc
  ) where

import Data.Text (Text)

syntaxSrc :: Text
syntaxSrc =
  "-- | Abstract syntax for arithmetic expressions with variables.\n\
  \module Expr.Syntax\n\
  \  ( Expr (..)\n\
  \  , Error (..)\n\
  \  , Env\n\
  \  ) where\n\
  \\n\
  \data Expr\n\
  \  = Lit Int\n\
  \  | Var String\n\
  \  | Neg Expr\n\
  \  | Add Expr Expr\n\
  \  | Mul Expr Expr\n\
  \  deriving stock (Eq, Show)\n\
  \\n\
  \data Error\n\
  \  = UnboundVar String\n\
  \  deriving stock (Eq, Show)\n\
  \\n\
  \type Env = [(String, Int)]\n"

evalSrc :: Text
evalSrc =
  "-- | Interpreter for 'Expr'. Pure, total on a well-formed 'Env'\n\
  \-- (every 'Var' must be present in the environment).\n\
  \module Expr.Eval (eval) where\n\
  \\n\
  \import Expr.Syntax\n\
  \\n\
  \-- | Evaluate an expression under an environment.\n\
  \eval :: Env -> Expr -> Either Error Int\n\
  \eval _   (Lit n)   = Right n\n\
  \eval env (Var x)   = maybe (Left (UnboundVar x)) Right (lookup x env)\n\
  \eval env (Neg e)   = negate <$> eval env e\n\
  \eval env (Add l r) = (+) <$> eval env l <*> eval env r\n\
  \eval env (Mul l r) = (*) <$> eval env l <*> eval env r\n"

-- | Sound simplifier — @0 * _@ absorption is DELIBERATELY OMITTED,
-- it would be unsound in the presence of @UnboundVar@ errors.
simplifySrc :: Text
simplifySrc =
  "-- | Algebraic simplification for 'Expr'. Soundness contract: for\n\
  \-- every environment @env@ and expression @e@,\n\
  \--   @eval env (simplify e) == eval env e@.\n\
  \module Expr.Simplify (simplify) where\n\
  \\n\
  \import Expr.Syntax\n\
  \\n\
  \simplify :: Expr -> Expr\n\
  \simplify = \\case\n\
  \  Lit n     -> Lit n\n\
  \  Var x     -> Var x\n\
  \  Neg inner -> simpNeg (simplify inner)\n\
  \  Add l r   -> simpAdd (simplify l) (simplify r)\n\
  \  Mul l r   -> simpMul (simplify l) (simplify r)\n\
  \\n\
  \simpNeg :: Expr -> Expr\n\
  \simpNeg (Neg x) = x\n\
  \simpNeg (Lit n) = Lit (negate n)\n\
  \simpNeg x       = Neg x\n\
  \\n\
  \simpAdd :: Expr -> Expr -> Expr\n\
  \simpAdd (Lit 0) r       = r\n\
  \simpAdd l       (Lit 0) = l\n\
  \simpAdd (Lit a) (Lit b) = Lit (a + b)\n\
  \simpAdd l       r       = Add l r\n\
  \\n\
  \-- NB: no @0 * _ = 0@ absorption — unsound in the presence of\n\
  \-- potentially-erroring sub-expressions (Var pointing at a missing env).\n\
  \simpMul :: Expr -> Expr -> Expr\n\
  \simpMul (Lit 1) r       = r\n\
  \simpMul l       (Lit 1) = l\n\
  \simpMul (Lit a) (Lit b) = Lit (a * b)\n\
  \simpMul l       r       = Mul l r\n"

prettySrc :: Text
prettySrc =
  "-- | Pretty-printer and minimal parser for 'Expr'. The roundtrip\n\
  \-- contract is: @parseExpr (pretty e) == Just e@.\n\
  \module Expr.Pretty\n\
  \  ( pretty\n\
  \  , parseExpr\n\
  \  ) where\n\
  \\n\
  \import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)\n\
  \import Expr.Syntax\n\
  \\n\
  \pretty :: Expr -> String\n\
  \pretty = go (0 :: Int)\n\
  \  where\n\
  \    go :: Int -> Expr -> String\n\
  \    go _ (Lit n)\n\
  \      | n < 0     = \"(\" ++ show n ++ \")\"\n\
  \      | otherwise = show n\n\
  \    go _ (Var x)   = x\n\
  \    go _ (Neg e)   = \"-(\" ++ go 0 e ++ \")\"\n\
  \    go p (Add l r) =\n\
  \      let s = go 1 l ++ \" + \" ++ go 2 r\n\
  \       in if p > 1 then \"(\" ++ s ++ \")\" else s\n\
  \    go p (Mul l r) =\n\
  \      let s = go 2 l ++ \" * \" ++ go 3 r\n\
  \       in if p > 2 then \"(\" ++ s ++ \")\" else s\n\
  \\n\
  \parseExpr :: String -> Maybe Expr\n\
  \parseExpr s = case pExpr (ws s) of\n\
  \  Just (e, rest) | null (ws rest) -> Just e\n\
  \  _                               -> Nothing\n\
  \\n\
  \type P a = String -> Maybe (a, String)\n\
  \\n\
  \ws :: String -> String\n\
  \ws = dropWhile isSpace\n\
  \\n\
  \pExpr :: P Expr\n\
  \pExpr = chainl1 pTerm plusOp\n\
  \  where\n\
  \    plusOp s = case ws s of\n\
  \      '+' : rest -> Just (Add, rest)\n\
  \      _          -> Nothing\n\
  \\n\
  \pTerm :: P Expr\n\
  \pTerm = chainl1 pAtom mulOp\n\
  \  where\n\
  \    mulOp s = case ws s of\n\
  \      '*' : rest -> Just (Mul, rest)\n\
  \      _          -> Nothing\n\
  \\n\
  \chainl1 :: P Expr -> P (Expr -> Expr -> Expr) -> P Expr\n\
  \chainl1 p op = \\s0 -> do\n\
  \  (x0, s1) <- p s0\n\
  \  loop x0 s1\n\
  \  where\n\
  \    loop x s = case op s of\n\
  \      Just (f, s') -> case p s' of\n\
  \        Just (y, s'') -> loop (f x y) s''\n\
  \        Nothing       -> Nothing\n\
  \      Nothing -> Just (x, s)\n\
  \\n\
  \pAtom :: P Expr\n\
  \pAtom s0 = case ws s0 of\n\
  \  \"\"               -> Nothing\n\
  \  '-' : '(' : rest -> do\n\
  \    (e, s1) <- pExpr rest\n\
  \    case ws s1 of\n\
  \      ')' : s2 -> Just (Neg e, s2)\n\
  \      _        -> Nothing\n\
  \  '(' : rest ->\n\
  \    case pSignedLit rest of\n\
  \      Just (lit, s1) -> case ws s1 of\n\
  \        ')' : s2 -> Just (lit, s2)\n\
  \        _        -> Nothing\n\
  \      Nothing -> do\n\
  \        (e, s1) <- pExpr rest\n\
  \        case ws s1 of\n\
  \          ')' : s2 -> Just (e, s2)\n\
  \          _        -> Nothing\n\
  \  c : _\n\
  \    | isDigit c ->\n\
  \        let (ds, rest) = span isDigit (ws s0)\n\
  \         in Just (Lit (read ds), rest)\n\
  \    | isAlpha c ->\n\
  \        let (name, rest) = span isAlphaNum (ws s0)\n\
  \         in Just (Var name, rest)\n\
  \    | otherwise -> Nothing\n\
  \\n\
  \pSignedLit :: P Expr\n\
  \pSignedLit s0 = case ws s0 of\n\
  \  '-' : rest@(c : _) | isDigit c ->\n\
  \    let (ds, rest2) = span isDigit rest\n\
  \     in Just (Lit (negate (read ds)), rest2)\n\
  \  _ -> Nothing\n"

-- | Test-support module that re-exports every pure top-level AND
-- supplies a sized 'Arbitrary Expr' instance. Being a module the
-- E2E loads, its @:browse@ output lists simplify + eval + pretty +
-- parseExpr as same-module siblings — which is how the
-- sibling-aware suggest engine discovers them.
genSrc :: Text
genSrc =
  "module Gen\n\
  \  ( module Expr.Syntax\n\
  \  , module Expr.Eval\n\
  \  , module Expr.Simplify\n\
  \  , module Expr.Pretty\n\
  \  , genIdent\n\
  \  ) where\n\
  \\n\
  \import Expr.Eval\n\
  \import Expr.Pretty\n\
  \import Expr.Simplify\n\
  \import Expr.Syntax\n\
  \import Test.QuickCheck\n\
  \\n\
  \genIdent :: Gen String\n\
  \genIdent = do\n\
  \  h <- elements ['a' .. 'z']\n\
  \  n <- choose (0 :: Int, 4)\n\
  \  t <- vectorOf n (elements (['a' .. 'z'] ++ ['0' .. '9']))\n\
  \  pure (h : t)\n\
  \\n\
  \instance Arbitrary Expr where\n\
  \  arbitrary = sized go\n\
  \    where\n\
  \      go 0 = oneof\n\
  \        [ Lit <$> arbitrary\n\
  \        , Var <$> genIdent\n\
  \        ]\n\
  \      go n = frequency\n\
  \        [ (2, Lit <$> arbitrary)\n\
  \        , (2, Var <$> genIdent)\n\
  \        , (1, Neg <$> go (n `div` 2))\n\
  \        , (2, Add <$> go (n `div` 2) <*> go (n `div` 2))\n\
  \        , (2, Mul <$> go (n `div` 2) <*> go (n `div` 2))\n\
  \        ]\n\
  \  shrink (Lit n)   = Lit <$> shrink n\n\
  \  shrink (Var _)   = []\n\
  \  shrink (Neg e)   = e : (Neg <$> shrink e)\n\
  \  shrink (Add l r) = [l, r] ++ [Add l' r | l' <- shrink l] ++ [Add l r' | r' <- shrink r]\n\
  \  shrink (Mul l r) = [l, r] ++ [Mul l' r | l' <- shrink l] ++ [Mul l r' | r' <- shrink r]\n"
