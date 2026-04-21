{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec
import Test.QuickCheck

import Expr.Eval
  ( checkedAdd
  , checkedMul
  , checkedNeg
  , eval
  , evalClosed
  )
import Expr.Pretty (maxInputSize, maxParseDepth, parse, pretty)
import Expr.Simplify (simplify)
import Expr.Syntax
  ( Env
  , Error (..)
  , Expr (..)
  , emptyEnv
  , extendEnv
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Expr.Eval (unit)" $ do
    it "Lit n evaluates to n" $
      evalClosed (Lit 42) `shouldBe` Right 42
    it "Add folds constants" $
      evalClosed (Add (Lit 2) (Lit 3)) `shouldBe` Right 5
    it "Mul folds constants" $
      evalClosed (Mul (Lit 3) (Lit 4)) `shouldBe` Right 12
    it "Neg negates" $
      evalClosed (Neg (Lit 5)) `shouldBe` Right (-5)
    it "unbound var yields UnboundVar" $
      evalClosed (Var "x") `shouldBe` Left (UnboundVar "x")
    it "bound var resolves via env" $
      let env = extendEnv "x" 7 emptyEnv :: Env
      in  eval env (Add (Var "x") (Lit 3)) `shouldBe` Right 10
    it "Add overflow reported" $
      evalClosed (Add (Lit maxBound) (Lit 1)) `shouldBe` Left Overflow
    it "Mul overflow reported" $
      evalClosed (Mul (Lit maxBound) (Lit 2)) `shouldBe` Left Overflow
    it "Neg minBound reported" $
      evalClosed (Neg (Lit minBound)) `shouldBe` Left Overflow
    it "checkedAdd returns overflow near boundary" $
      checkedAdd maxBound 1 `shouldBe` Left Overflow
    it "checkedMul returns overflow near boundary" $
      checkedMul maxBound 2 `shouldBe` Left Overflow
    it "checkedNeg minBound returns overflow" $
      checkedNeg minBound `shouldBe` Left Overflow

  describe "Expr.Simplify (unit)" $ do
    it "0 + x = x" $
      simplify (Add (Lit 0) (Var "x")) `shouldBe` Var "x"
    it "x + 0 = x" $
      simplify (Add (Var "x") (Lit 0)) `shouldBe` Var "x"
    it "1 * x = x" $
      simplify (Mul (Lit 1) (Var "x")) `shouldBe` Var "x"
    it "x * 1 = x" $
      simplify (Mul (Var "x") (Lit 1)) `shouldBe` Var "x"
    it "0 * x = 0" $
      simplify (Mul (Lit 0) (Var "x")) `shouldBe` Lit 0
    it "x * 0 = 0" $
      simplify (Mul (Var "x") (Lit 0)) `shouldBe` Lit 0
    it "Neg (Neg x) = x" $
      simplify (Neg (Neg (Var "x"))) `shouldBe` Var "x"
    it "constant folding: Add" $
      simplify (Add (Lit 2) (Lit 3)) `shouldBe` Lit 5
    it "constant folding: Mul" $
      simplify (Mul (Lit 3) (Lit 4)) `shouldBe` Lit 12
    it "constant folding: Neg of literal" $
      simplify (Neg (Lit 5)) `shouldBe` Lit (-5)
    it "constant folding skipped on overflow (Add)" $
      simplify (Add (Lit maxBound) (Lit 1)) `shouldBe` Add (Lit maxBound) (Lit 1)
    it "constant folding skipped on overflow (Mul)" $
      simplify (Mul (Lit maxBound) (Lit 2)) `shouldBe` Mul (Lit maxBound) (Lit 2)
    it "nested simplification reaches fixpoint" $
      simplify (Add (Mul (Lit 0) (Var "y")) (Var "x")) `shouldBe` Var "x"

  describe "Expr.Pretty (unit)" $ do
    it "prints flat Add" $
      pretty (Add (Lit 2) (Lit 3)) `shouldBe` "2 + 3"
    it "mul precedence higher than add" $
      pretty (Add (Lit 2) (Mul (Lit 3) (Lit 4))) `shouldBe` "2 + 3 * 4"
    it "parens enforce evaluation order" $
      pretty (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` "(2 + 3) * 4"
    it "unary minus renders tight" $
      pretty (Mul (Neg (Lit 2)) (Lit 3)) `shouldBe` "-2 * 3"
    it "nested neg parens" $
      pretty (Neg (Neg (Lit 2))) `shouldBe` "-(-2)"
    it "parses flat add" $
      parse "2 + 3" `shouldBe` Right (Add (Lit 2) (Lit 3))
    it "parses mul precedence" $
      parse "2 + 3 * 4" `shouldBe` Right (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
    it "parses parens" $
      parse "(2 + 3) * 4" `shouldBe` Right (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    it "parses unary minus" $
      parse "-5" `shouldBe` Right (Neg (Lit 5))
    it "parses variable" $
      parse "x + y" `shouldBe` Right (Add (Var "x") (Var "y"))
    it "tolerates whitespace" $
      parse "  2   +   3  " `shouldBe` Right (Add (Lit 2) (Lit 3))
    it "rejects empty input" $
      parse "" `shouldBe` Left ParseError
    it "rejects garbage" $
      parse "@#$" `shouldBe` Left ParseError
    it "rejects unbalanced parens" $
      parse "(2 + 3" `shouldBe` Left ParseError
    it "rejects oversized input (SizeExceeded guard)" $
      parse (replicate (maxInputSize + 1) 'x') `shouldBe` Left SizeExceeded
    it "rejects deeply nested parens (DepthExceeded guard)" $
      let nested = replicate (maxParseDepth + 1) '(' ++ "1"
                      ++ replicate (maxParseDepth + 1) ')'
      in  parse nested `shouldBe` Left DepthExceeded
    it "rejects deeply nested unary minus (DepthExceeded guard)" $
      let nested = replicate (maxParseDepth + 1) '-' ++ "1"
      in  parse nested `shouldBe` Left DepthExceeded

  describe "Expr properties" $ do
    it "roundtrip: parse (pretty e) == Right e" $
      property prop_roundtrip
    it "simplify preserves successful evaluation on closed expr" $
      property prop_simplifyPreservesSuccess
    it "simplify is idempotent" $
      property prop_simplifyIdempotent
    it "eval is deterministic" $
      property prop_evalDeterministic
    it "checkedAdd is commutative when it succeeds" $
      property prop_addCommutative

prop_roundtrip :: Expr -> Property
prop_roundtrip e = parse (pretty e) === Right e

prop_simplifyPreservesSuccess :: ClosedExpr -> Property
prop_simplifyPreservesSuccess (ClosedExpr e) =
  case evalClosed e of
    Right v -> evalClosed (simplify e) === Right v
    Left _  -> property True

prop_simplifyIdempotent :: Expr -> Property
prop_simplifyIdempotent e = simplify (simplify e) === simplify e

prop_evalDeterministic :: Expr -> Property
prop_evalDeterministic e = evalClosed e === evalClosed e

prop_addCommutative :: Int -> Int -> Property
prop_addCommutative a b =
  case (checkedAdd a b, checkedAdd b a) of
    (Right x, Right y) -> x === y
    (Left _, Left _)   -> property True
    _                  -> property False

newtype ClosedExpr = ClosedExpr { unClosed :: Expr }
  deriving stock (Show)

instance Arbitrary ClosedExpr where
  arbitrary = ClosedExpr <$> sized genClosed
  shrink (ClosedExpr e) =
    [ ClosedExpr e' | e' <- shrink e, not (hasVar e') ]

hasVar :: Expr -> Bool
hasVar = \case
  Lit _   -> False
  Var _   -> True
  Neg x   -> hasVar x
  Add l r -> hasVar l || hasVar r
  Mul l r -> hasVar l || hasVar r

genClosed :: Int -> Gen Expr
genClosed n
  | n <= 0 = Lit <$> chooseInt (0, 10)
  | otherwise = frequency
      [ (1, Lit <$> chooseInt (0, 10))
      , (1, Neg <$> genClosed (n `div` 2))
      , (2, Add <$> genClosed (n `div` 2) <*> genClosed (n `div` 2))
      , (2, Mul <$> genClosed (n `div` 2) <*> genClosed (n `div` 2))
      ]

instance Arbitrary Expr where
  arbitrary = sized genExpr
  shrink = \case
    Lit _   -> []
    Var _   -> []
    Neg e   -> e : [Neg e' | e' <- shrink e]
    Add l r -> [l, r] ++ [Add l' r | l' <- shrink l] ++ [Add l r' | r' <- shrink r]
    Mul l r -> [l, r] ++ [Mul l' r | l' <- shrink l] ++ [Mul l r' | r' <- shrink r]

genExpr :: Int -> Gen Expr
genExpr n
  | n <= 0 = oneof
      [ Lit <$> chooseInt (0, 100)
      , Var <$> genVarName
      ]
  | otherwise = frequency
      [ (1, Lit <$> chooseInt (0, 100))
      , (1, Var <$> genVarName)
      , (1, Neg <$> genExpr (n `div` 2))
      , (2, Add <$> genExpr (n `div` 2) <*> genExpr (n `div` 2))
      , (2, Mul <$> genExpr (n `div` 2) <*> genExpr (n `div` 2))
      ]

genVarName :: Gen String
genVarName = do
  c <- elements ['a' .. 'h']
  pure [c]
