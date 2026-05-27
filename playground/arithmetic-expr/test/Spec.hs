-- | Test suite for the arithmetic expression evaluator.
--
-- Runs unit tests + QuickCheck properties.
-- Exit code: 0 = all pass, 1 = any failure.
module Main where

import qualified Data.Map.Strict as Map
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck

import Expr.Syntax
import Expr.Eval
import Expr.Simplify
import Expr.Pretty

--------------------------------------------------------------------------------
-- Arbitrary instance for Expr
--
-- IMPORTANT: Lit always generates non-negative values so that
-- pretty (Lit n) = show n (no leading minus) and the roundtrip holds.
-- Negative numbers are represented as Neg (Lit n) with n > 0.
--------------------------------------------------------------------------------

instance Arbitrary Expr where
    arbitrary = sized go
      where
        go 0 = oneof
            [ Lit . abs <$> arbitrary
            , Var <$> elements varNames
            ]
        go n = frequency
            [ (3, Lit . abs <$> arbitrary)
            , (1, Add <$> go (n `div` 2) <*> go (n `div` 2))
            , (1, Mul <$> go (n `div` 2) <*> go (n `div` 2))
            , (1, Neg <$> go (n `div` 2))
            , (2, Var <$> elements varNames)
            ]
        varNames = ["x", "y", "z", "a", "b", "n"]

    shrink (Lit _)     = []
    shrink (Var _)     = []
    shrink (Neg e)     = [e] ++ [Neg e' | e' <- shrink e]
    shrink (Add e1 e2) = [e1, e2]
                      ++ [Add e1' e2  | e1' <- shrink e1]
                      ++ [Add e1  e2' | e2' <- shrink e2]
    shrink (Mul e1 e2) = [e1, e2]
                      ++ [Mul e1' e2  | e1' <- shrink e1]
                      ++ [Mul e1  e2' | e2' <- shrink e2]

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests :: [(String, Bool)]
unitTests =
    -- Eval: basic cases
    [ ("eval Lit",         eval Map.empty (Lit 42)                == Right 42)
    , ("eval Add",         eval Map.empty (Add (Lit 3) (Lit 4))  == Right 7)
    , ("eval Mul",         eval Map.empty (Mul (Lit 3) (Lit 4))  == Right 12)
    , ("eval Neg",         eval Map.empty (Neg (Lit 5))          == Right (-5))
    , ("eval Var hit",     eval (Map.singleton "x" 7) (Var "x")  == Right 7)
    , ("eval Var miss",    eval Map.empty (Var "y")              == Left (UnboundVar "y"))
    , ("eval nested",      eval (Map.singleton "x" 2)
                               (Add (Mul (Var "x") (Lit 3)) (Neg (Lit 1)))
                                                                  == Right 5)

    -- Simplify: identity / annihilation / folding
    , ("simp 0+x",         simplify (Add (Lit 0) (Var "x"))      == Var "x")
    , ("simp x+0",         simplify (Add (Var "x") (Lit 0))      == Var "x")
    , ("simp 1*x",         simplify (Mul (Lit 1) (Var "x"))      == Var "x")
    , ("simp x*1",         simplify (Mul (Var "x") (Lit 1))      == Var "x")
    , ("simp 0*x",         simplify (Mul (Lit 0) (Var "x"))      == Lit 0)
    , ("simp x*0",         simplify (Mul (Var "x") (Lit 0))      == Lit 0)
    , ("simp neg-neg",     simplify (Neg (Neg (Var "x")))        == Var "x")
    , ("simp fold +",      simplify (Add (Lit 3) (Lit 4))        == Lit 7)
    , ("simp fold *",      simplify (Mul (Lit 3) (Lit 4))        == Lit 12)
    , ("simp fold neg",    simplify (Neg (Lit 5))                == Lit (-5))
    , ("simp deep",        simplify (Add (Lit 0)
                               (Mul (Lit 1) (Add (Lit 2) (Lit 3))))
                                                                  == Lit 5)

    -- Pretty / parse roundtrip: manual cases
    , ("pretty Lit",       pretty (Lit 42)                       == "42")
    , ("pretty Var",       pretty (Var "x")                      == "x")
    , ("pretty Neg",       pretty (Neg (Lit 3))                  == "(-3)")
    , ("pretty Add",       pretty (Add (Lit 1) (Lit 2))          == "(1 + 2)")
    , ("pretty Mul",       pretty (Add (Lit 3) (Lit 4))          == "(3 + 4)")
    , ("parse Lit",        parseExpr "42"                        == Right (Lit 42))
    , ("parse Var",        parseExpr "x"                         == Right (Var "x"))
    , ("parse Neg",        parseExpr "(-3)"                      == Right (Neg (Lit 3)))
    , ("parse Add",        parseExpr "(1 + 2)"                   == Right (Add (Lit 1) (Lit 2)))
    , ("parse Mul",        parseExpr "(3 * 4)"                   == Right (Mul (Lit 3) (Lit 4)))
    , ("parse nested",     parseExpr "((1 + 2) * (-x))"
                                                                  == Right (Mul (Add (Lit 1) (Lit 2)) (Neg (Var "x"))))
    ]

--------------------------------------------------------------------------------
-- QuickCheck properties
--------------------------------------------------------------------------------

-- | Simplification preserves evaluation for closed expressions.
prop_simplifyPreservesEval :: Env -> Expr -> Property
prop_simplifyPreservesEval env e =
    eval env (simplify e) === eval env e

-- | Simplification is idempotent.
prop_simplifyIdempotent :: Expr -> Property
prop_simplifyIdempotent e =
    simplify (simplify e) === simplify e

-- | 0 + e simplifies to the same result as e.
prop_addZeroLeft :: Expr -> Property
prop_addZeroLeft e =
    simplify (Add (Lit 0) e) === simplify e

-- | e + 0 simplifies to the same result as e.
prop_addZeroRight :: Expr -> Property
prop_addZeroRight e =
    simplify (Add e (Lit 0)) === simplify e

-- | 1 * e simplifies to the same result as e.
prop_mulOneLeft :: Expr -> Property
prop_mulOneLeft e =
    simplify (Mul (Lit 1) e) === simplify e

-- | Double negation cancels.
prop_doubleNeg :: Expr -> Property
prop_doubleNeg e =
    simplify (Neg (Neg e)) === simplify e

-- | Roundtrip: parse (pretty e) == Right e for non-negative Lits.
prop_roundtrip :: Expr -> Property
prop_roundtrip e =
    parseExpr (pretty e) === Right e

--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

runUnit :: (String, Bool) -> IO Bool
runUnit (name, ok) = do
    putStrLn $ (if ok then "PASS" else "FAIL") <> "  " <> name
    pure ok

runQC :: String -> Property -> IO Bool
runQC name prop = do
    result <- quickCheckWithResult (stdArgs { maxSuccess = 500 }) prop
    let ok = case result of { Success {} -> True; _ -> False }
    putStrLn $ (if ok then "PASS" else "FAIL") <> "  " <> name
    pure ok

main :: IO ()
main = do
    putStrLn "=== Unit tests ==="
    unitOks <- mapM runUnit unitTests

    putStrLn "\n=== QuickCheck properties ==="
    qcOks <- sequence
        [ runQC "prop_simplifyPreservesEval" (property prop_simplifyPreservesEval)
        , runQC "prop_simplifyIdempotent"    (property prop_simplifyIdempotent)
        , runQC "prop_addZeroLeft"           (property prop_addZeroLeft)
        , runQC "prop_addZeroRight"          (property prop_addZeroRight)
        , runQC "prop_mulOneLeft"            (property prop_mulOneLeft)
        , runQC "prop_doubleNeg"             (property prop_doubleNeg)
        , runQC "prop_roundtrip"             (property prop_roundtrip)
        ]

    let allOk = and unitOks && and qcOks
    if allOk then do
        putStrLn "\nAll tests passed."
        exitSuccess
    else do
        putStrLn "\nSome tests FAILED."
        exitFailure
