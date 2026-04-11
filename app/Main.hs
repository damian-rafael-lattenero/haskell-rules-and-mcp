module Main where

import HM.Syntax
import HM.Infer (runInfer)
import HM.Pretty (ppExpr, ppScheme)
import Parser.HM (parseProgram)
import Parser.Core (ppParseError)

main :: IO ()
main = do
  putStrLn "=== Hindley-Milner Type Inference ==="
  putStrLn ""
  putStrLn "--- Manual AST examples ---"
  putStrLn ""
  mapM_ runExample examples
  putStrLn "--- Parsed from text ---"
  putStrLn ""
  mapM_ runParsedExample parsedExamples

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

  -- Recursive let (letrec)
  , ("Letrec: recursive identity",
     ELetRec "f" (ELam "x" (EIf (ELit (LBool True)) (EVar "x") (EApp (EVar "f") (EVar "x"))))
       (EVar "f"))

  , ("Letrec with polymorphic usage",
     ELetRec "f" (ELam "x" (EVar "x"))
       (ELet "a" (EApp (EVar "f") (ELit (LInt 5)))
         (EApp (EVar "f") (ELit (LBool True)))))

  -- Pairs
  , ("Pair construction",
     EPair (ELit (LInt 1)) (ELit (LBool True)))

  , ("First projection",
     EFst (EPair (ELit (LInt 1)) (ELit (LBool True))))

  , ("Polymorphic pair: \\x -> (x, x)",
     ELam "x" (EPair (EVar "x") (EVar "x")))

  , ("Swap: \\p -> (snd p, fst p)",
     ELam "p" (EPair (ESnd (EVar "p")) (EFst (EVar "p"))))

  -- Type annotations
  , ("Annotation: (42 : Int)",
     EAnn (ELit (LInt 42)) (TCon "Int"))

  , ("Annotation: (\\x -> x : Int -> Int)",
     EAnn (ELam "x" (EVar "x")) (TArr (TCon "Int") (TCon "Int")))

  , ("ERROR: annotation mismatch (42 : Bool)",
     EAnn (ELit (LInt 42)) (TCon "Bool"))
  ]

runParsedExample :: (String, String) -> IO ()
runParsedExample (label, source) = do
  putStrLn $ "-- " ++ label
  putStrLn $ "   " ++ show source
  case parseProgram source of
    Left err -> putStrLn $ "   PARSE ERROR: " ++ ppParseError err
    Right expr' -> do
      putStrLn $ "   AST: " ++ ppExpr expr'
      case runInfer expr' of
        Left err -> putStrLn $ "   TYPE ERROR: " ++ show err
        Right sc -> putStrLn $ "   :: " ++ ppScheme sc
  putStrLn ""

parsedExamples :: [(String, String)]
parsedExamples =
  [ ("Integer literal", "42")
  , ("Boolean literal", "true")
  , ("Identity", "\\x -> x")
  , ("Const", "\\x -> \\y -> x")
  , ("Application", "\\f -> \\x -> f x")
  , ("Let polymorphism", "let id = \\x -> x in let a = id 5 in id true")
  , ("If-then-else", "if true then 1 else 2")
  , ("Compose", "\\f -> \\g -> \\x -> f (g x)")
  , ("Pair", "(1, true)")
  , ("Fst", "fst (1, true)")
  , ("Snd", "snd (1, true)")
  , ("Swap", "\\p -> (snd p, fst p)")
  , ("Letrec", "let rec f = \\x -> if true then x else f x in f")
  , ("Annotation", "(42 : Int)")
  , ("Annotation on lambda", "(\\x -> x : Int -> Int)")
  , ("Product type annotation", "(\\x -> x : (Int, Bool) -> (Int, Bool))")
  , ("ERROR: annotation mismatch", "(42 : Bool)")
  , ("ERROR: parse error", "let in")
  ]
