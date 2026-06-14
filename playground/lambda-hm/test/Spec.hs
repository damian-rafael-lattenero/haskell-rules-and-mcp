{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Test.QuickCheck
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure, exitSuccess)

import Lambda.Syntax
import Lambda.Eval
import Lambda.Infer

-- ---------------------------------------------------------------------------
-- Arbitrary instances (templates from ghc_arbitrary)

instance Arbitrary Lit where
    arbitrary = oneof [ LInt <$> arbitrary, LBool <$> arbitrary ]

instance Arbitrary Term where
    arbitrary = sized go
      where
        go 0 = oneof [ Var <$> varName, Lit <$> arbitrary ]
        go n = frequency
            [ (2, Var <$> varName)
            , (1, Lam <$> varName <*> go (n `div` 2))
            , (1, App <$> go (n `div` 2) <*> go (n `div` 2))
            , (1, Let <$> varName <*> go (n `div` 2) <*> go (n `div` 2))
            , (2, Lit <$> arbitrary)
            ]
        varName = elements ["x", "y", "z", "f", "g"]

instance Arbitrary Type where
    arbitrary = sized go
      where
        go 0 = oneof [ TVar <$> tvName, pure TInt, pure TBool ]
        go n = frequency
            [ (2, TVar <$> tvName)
            , (2, pure TInt)
            , (2, pure TBool)
            , (1, TArr <$> go (n `div` 2) <*> go (n `div` 2))
            ]
        tvName = elements ["a", "b", "c", "d"]

-- ---------------------------------------------------------------------------
-- Law 1: empty substitution is identity
prop_emptySubstIdentity :: Type -> Bool
prop_emptySubstIdentity t = applySubst Map.empty t == t

-- Law 2: substitution composition is associative in effect
--   apply (s1 . s2) t == apply s1 (apply s2 t)
prop_composeSubstCorrect :: Type -> Bool
prop_composeSubstCorrect t =
    let s1 = Map.fromList [("a", TInt)]
        s2 = Map.fromList [("b", TVar "a")]
        composed = composeSubst s1 s2
    in applySubst composed t == applySubst s1 (applySubst s2 t)

-- Law 3: unify t t always succeeds (reflexivity)
prop_unifyRefl :: Type -> Bool
prop_unifyRefl t =
    case runInfer (unify t t) of
        Left _  -> False
        Right _ -> True

-- Law 4: if unify succeeds, applying the substitution makes both sides equal
prop_unifySound :: Type -> Type -> Bool
prop_unifySound t1 t2 =
    case runInfer (unify t1 t2) of
        Left  _  -> True  -- unification may fail; that's fine
        Right s  -> applySubst s t1 == applySubst s t2

-- Law 5: literals always type-check to their base type
prop_litTypeSafe :: Bool
prop_litTypeSafe =
    inferScheme emptyEnv (Lit (LInt 42))  == Right (Forall [] TInt)  &&
    inferScheme emptyEnv (Lit (LBool True)) == Right (Forall [] TBool)

-- Law 6: identity lambda \x.x has type forall a. a -> a
prop_idType :: Bool
prop_idType =
    case inferScheme emptyEnv (Lam "x" (Var "x")) of
        Right (Forall [_] (TArr (TVar a) (TVar b))) -> a == b
        _ -> False

-- Law 7: well-typed closed terms evaluate without unbound-variable errors
prop_typeSafety :: Property
prop_typeSafety = forAll closedTerm $ \t ->
    case inferScheme emptyEnv t of
        Left  _ -> True   -- ill-typed: skip
        Right _ ->
            case eval Map.empty t of
                Left err -> "unbound" `notElem` words err
                Right _  -> True
  where
    closedTerm = sized $ \n -> frequency
        [ (3, Lit <$> arbitrary)
        , (1, do x <- elements ["x","y"]
                 b <- resize (max 0 (n-1)) arbitrary
                 return (Lam x b))
        , (1, do x <- elements ["x","y"]
                 v <- resize (max 0 (n-1)) (Lit <$> arbitrary)
                 b <- resize (max 0 (n-1)) arbitrary
                 return (Let x v b))
        ]

-- ---------------------------------------------------------------------------
-- Runner

check :: String -> IO Result -> IO Bool
check name act = do
    r <- act
    case r of
        Success {} -> putStrLn ("PASS  " ++ name) >> return True
        _          -> putStrLn ("FAIL  " ++ name) >> return False

main :: IO ()
main = do
    results <- sequence
        [ check "emptySubst identity"    $ quickCheckResult prop_emptySubstIdentity
        , check "composeSubst correct"   $ quickCheckResult prop_composeSubstCorrect
        , check "unify reflexivity"      $ quickCheckResult prop_unifyRefl
        , check "unify soundness"        $ quickCheckResult prop_unifySound
        , check "lit type safety"        $ quickCheckResult prop_litTypeSafe
        , check "id has polymorphic type"$ quickCheckResult prop_idType
        , check "type safety (eval)"     $ quickCheckResult prop_typeSafety
        ]
    if and results then exitSuccess else exitFailure
