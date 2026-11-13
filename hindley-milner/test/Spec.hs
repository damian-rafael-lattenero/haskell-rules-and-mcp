module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Language.HM

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Hindley-Milner Tests"
  [ parserTests
  , inferTests
  , prettyTests
  , propertyTests
  ]

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "parse variable" $
      parseTerm "x" @?= Right (Var "x" noPos)
  , testCase "parse lambda" $
      parseTerm "\\x -> x" @?= Right (Lam "x" (Var "x" noPos) noPos)
  , testCase "parse application" $
      parseTerm "f x" @?= Right (App (Var "f" noPos) (Var "x" noPos) noPos)
  , testCase "parse let" $
      parseTerm "let x = 1 in x" @?= Right (Let "x" (Lit (LInt 1) noPos) (Var "x" noPos) noPos)
  , testCase "parse annotation" $
      parseTerm "x :: Int" @?= Right (Ann (Var "x" noPos) (TCon "Int") noPos)
  , testCase "parse type variable" $
      parseType "a" @?= Right (TVar "a")
  , testCase "parse type constructor" $
      parseType "Int" @?= Right (TCon "Int")
  , testCase "parse function type" $
      parseType "a -> b" @?= Right (TApp (TApp (TCon "->") (TVar "a")) (TVar "b"))
  ]

inferTests :: TestTree
inferTests = testGroup "Type Inference"
  [ testCase "identity function" $ do
      Right (scheme, ty) <- pure $ inferType "\\x -> x"
      renderScheme scheme @?= "forall a. a -> a"
  , testCase "const function" $ do
      Right (scheme, ty) <- pure $ inferType "\\x -> \\y -> x"
      renderScheme scheme @?= "forall a b. a -> b -> a"
  , testCase "application" $ do
      Right (scheme, ty) <- pure $ inferType "(\\x -> x) 1"
      renderType ty @?= "Int"
  , testCase "let polymorphism" $ do
      Right (scheme, ty) <- pure $ inferType "let id = \\x -> x in id 1"
      -- This should work with let-polymorphism
      renderType ty @?= "Int"
  , testCase "integer literal" $ do
      Right (scheme, ty) <- pure $ inferType "42"
      renderType ty @?= "Int"
  , testCase "boolean literal" $ do
      Right (scheme, ty) <- pure $ inferType "true"
      renderType ty @?= "Bool"
  , testCase "annotation" $ do
      Right (scheme, ty) <- pure $ inferType "(\\x -> x) :: Int -> Int"
      renderType ty @?= "Int -> Int"
  ]

prettyTests :: TestTree
prettyTests = testGroup "Pretty Printing"
  [ testCase "pretty type variable" $
      renderType (TVar "a") @?= "a"
  , testCase "pretty type constructor" $
      renderType (TCon "Int") @?= "Int"
  , testCase "pretty function type" $
      renderType (TApp (TApp (TCon "->") (TVar "a")) (TVar "b")) @?= "a -> b"
  , testCase "pretty scheme" $
      renderScheme (Forall ["a", "b"] (TApp (TApp (TCon "->") (TVar "a")) (TVar "b"))) @?= "forall a b. a -> b"
  ]

propertyTests :: TestTree
propertyTests = testGroup "QuickCheck Properties"
  [ testProperty "infer identity is polymorphic" $
      \(_ :: ()) -> case inferType "\\x -> x" of
        Right (Forall vars _, _) -> length vars == 1
        Left _ -> False
  , testProperty "infer const has two type vars" $
      \(_ :: ()) -> case inferType "\\x -> \\y -> x" of
        Right (Forall vars _, _) -> length vars == 2
        Left _ -> False
  ]