module Main where

import HM.Syntax
import HM.Infer (runInfer)
import HM.Pretty (ppExpr, ppScheme)

main :: IO ()
main = do
  putStrLn "=== Hindley-Milner Type Inference ==="
  putStrLn ""
  mapM_ runExample examples

runExample :: (String, Expr) -> IO ()
runExample (label, expr) = do
  putStrLn $ "-- " ++ label
  putStrLn $ "   " ++ ppExpr expr
  case runInfer expr of
    Left err -> putStrLn $ "   ERROR: " ++ show err
    Right sc -> putStrLn $ "   :: " ++ ppScheme sc
  putStrLn ""

examples :: [(String, Expr)]
examples =
  [ ("Integer literal",
     ELit (LInt 42))

  , ("Boolean literal",
     ELit (LBool True))

  , ("Identity function",
     ELam "x" (EVar "x"))

  , ("Const function",
     ELam "x" (ELam "y" (EVar "x")))

  , ("Function application",
     ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x"))))

  , ("Let polymorphism: id used at Int and Bool",
     ELet "id" (ELam "x" (EVar "x"))
       (ELet "a" (EApp (EVar "id") (ELit (LInt 5)))
         (EApp (EVar "id") (ELit (LBool True)))))

  , ("If-then-else",
     EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2)))

  , ("Compose: \\f g x -> f (g x)",
     ELam "f" (ELam "g" (ELam "x"
       (EApp (EVar "f") (EApp (EVar "g") (EVar "x"))))))

  , ("ERROR: unbound variable",
     EVar "foo")

  , ("ERROR: if condition not Bool",
     EIf (ELit (LInt 1)) (ELit (LInt 2)) (ELit (LInt 3)))

  , ("ERROR: branch type mismatch",
     EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LBool False)))
  ]
