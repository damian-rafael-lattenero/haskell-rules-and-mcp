module Main where

import LambdaHM
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck

-- ─── Helpers ──────────────────────────────────────────────────────────────────

pass :: Bool -> String -> IO Bool
pass ok lbl = do
  putStrLn ((if ok then "PASS  " else "FAIL  ") ++ lbl)
  return ok

qcPassed :: Testable p => String -> p -> IO Bool
qcPassed lbl p = do
  result <- quickCheckWithResult (stdArgs{maxSuccess = 200, chatty = False}) p
  let ok = case result of Success{} -> True; _ -> False
  putStrLn ((if ok then "PASS  " else "FAIL  ") ++ lbl)
  return ok

-- ─── Unit tests ───────────────────────────────────────────────────────────────

-- Int literal gets type Int
unitIntLit :: IO Bool
unitIntLit = pass (typecheck (Lit (LInt 42)) == Right tInt) "Int literal : Int"

-- Bool literal gets type Bool
unitBoolLit :: IO Bool
unitBoolLit = pass (typecheck (Lit (LBool True)) == Right tBool) "Bool literal : Bool"

-- Identity function   \x. x   has type   a -> a
unitIdentity :: IO Bool
unitIdentity =
  let expr = Lam "x" (Var "x")
  in case typecheck expr of
       Right (TArr (TVar a) (TVar b)) -> pass (a == b) "identity : a -> a"
       Right t -> pass False ("identity : a -> a (got " ++ prettyType t ++ ")")
       Left e -> pass False ("identity : type error " ++ show e)

-- Unbound variable is rejected
unitUnbound :: IO Bool
unitUnbound = pass (typecheck (Var "z") == Left (UnboundVariable "z")) "unbound var -> error"

-- Occurs-check: \x. x x  should produce InfiniteType
unitOccursCheck :: IO Bool
unitOccursCheck =
  let expr = Lam "x" (App (Var "x") (Var "x"))
  in case typecheck expr of
       Left (InfiniteType _ _) -> pass True "occurs check -> InfiniteType"
       _ -> pass False "occurs check -> InfiniteType (unexpected)"

-- Let-polymorphism: let id = \x. x in id id  should typecheck
unitLetPoly :: IO Bool
unitLetPoly =
  let expr = Let "id" (Lam "x" (Var "x")) (App (Var "id") (Var "id"))
  in case typecheck expr of
       Right (TArr _ _) -> pass True "let-poly: (id id) : a -> a"
       Right t -> pass False ("let-poly: got " ++ prettyType t)
       Left e -> pass False ("let-poly: type error " ++ show e)

-- Conditional expression: if true then 1 else 2 : Int
unitIfExpr :: IO Bool
unitIfExpr =
  let expr = If (Lit (LBool True)) (Lit (LInt 1)) (Lit (LInt 2))
  in pass (typecheck expr == Right tInt) "if true then 1 else 2 : Int"

-- ─── Evaluation unit tests ────────────────────────────────────────────────────

unitEvalInt :: IO Bool
unitEvalInt = case run (Lit (LInt 7)) of
  Right (VInt 7) -> pass True "eval: 7 -> VInt 7"
  _ -> pass False "eval: 7 -> VInt 7"

unitEvalApp :: IO Bool
unitEvalApp =
  let expr = App (App (Var "add") (Lit (LInt 3))) (Lit (LInt 4))
  in case run expr of
       Right (VInt 7) -> pass True "eval: add 3 4 = 7"
       v -> pass False ("eval: add 3 4, got " ++ show v)

unitEvalLet :: IO Bool
unitEvalLet =
  let expr = Let "x" (Lit (LInt 10)) (App (App (Var "mul") (Var "x")) (Var "x"))
  in case run expr of
       Right (VInt 100) -> pass True "eval: let x=10 in x*x = 100"
       v -> pass False ("eval: let x=10 in x*x, got " ++ show v)

-- ─── QuickCheck properties ────────────────────────────────────────────────────

-- Prop 1: Int literals always typecheck to Int (no matter the value)
prop_intLitType :: Int -> Bool
prop_intLitType n = typecheck (Lit (LInt n)) == Right tInt

-- Prop 2: Bool literals always typecheck to Bool
prop_boolLitType :: Bool -> Bool
prop_boolLitType b = typecheck (Lit (LBool b)) == Right tBool

-- Prop 3: Int literal evaluation always returns the same integer
prop_intLitEval :: Int -> Bool
prop_intLitEval n = case run (Lit (LInt n)) of Right (VInt m) -> m == n; _ -> False

-- Prop 4: Bool literal evaluation always returns the same bool
prop_boolLitEval :: Bool -> Bool
prop_boolLitEval b = case run (Lit (LBool b)) of Right (VBool r) -> r == b; _ -> False

-- Prop 5: well-typed expressions never produce a runtime type error
--         (we test on the small closed fragment of literals + if)
prop_wellTypedNoRuntimeError :: Bool -> Int -> Int -> Bool
prop_wellTypedNoRuntimeError cond t f =
  let expr = If (Lit (LBool cond)) (Lit (LInt t)) (Lit (LInt f))
  in case (typecheck expr, run expr) of
       (Right _, Left _) -> False
       _ -> True

-- Prop 6: type inference is stable under re-wrapping in identity let
--         let id = \x.x in e  has the same type as e (for literals)
prop_letIdentityPreservesType :: Int -> Bool
prop_letIdentityPreservesType n =
  let lit = Lit (LInt n)
      wrapped = Let "id" (Lam "x" (Var "x")) lit
  in typecheck lit == typecheck wrapped

-- ─── Runner ───────────────────────────────────────────────────────────────────

main :: IO ()
main = do
  putStrLn "=== LambdaHM — type inference + eval tests ===\n"

  putStrLn "-- Unit: typing --"
  results <- sequence
    [ unitIntLit
    , unitBoolLit
    , unitIdentity
    , unitUnbound
    , unitOccursCheck
    , unitLetPoly
    , unitIfExpr
    , putStrLn "" >> return True
    , pass True "-- Unit: evaluation --"
    , unitEvalInt
    , unitEvalApp
    , unitEvalLet
    , putStrLn "" >> return True
    , pass True "-- QuickCheck properties --"
    ]
  qcResults <- sequence
    [ qcPassed "prop_intLitType"                prop_intLitType
    , qcPassed "prop_boolLitType"               prop_boolLitType
    , qcPassed "prop_intLitEval"                prop_intLitEval
    , qcPassed "prop_boolLitEval"               prop_boolLitEval
    , qcPassed "prop_wellTypedNoRuntimeError"    prop_wellTypedNoRuntimeError
    , qcPassed "prop_letIdentityPreservesType"   prop_letIdentityPreservesType
    ]

  putStrLn ""
  let allOk = and results && and qcResults
  if allOk
    then putStrLn "All tests passed." >> exitSuccess
    else putStrLn "Some tests FAILED." >> exitFailure
