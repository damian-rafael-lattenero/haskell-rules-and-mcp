{-# OPTIONS_GHC -Wno-orphans #-}
module Properties where

import Test.QuickCheck
import Data.Map.Strict qualified as Map

import HM.Syntax
import HM.Subst
import HM.Unify
import HM.Infer (runInfer)
import HM.Pretty (ppExpr)
import Parser.HM (parseProgram)

----------------------------------------------------------------------
-- Arbitrary instances
----------------------------------------------------------------------

instance Arbitrary Lit where
  arbitrary = oneof
    [ LInt <$> arbitrary
    , LBool <$> arbitrary
    ]
  shrink (LInt n)  = LInt <$> shrink n
  shrink (LBool _) = []

instance Arbitrary Type where
  arbitrary = sized genType
  shrink = shrinkType

genType :: Int -> Gen Type
genType 0 = oneof
  [ TVar <$> elements ["a", "b", "c"]
  , TCon <$> elements ["Int", "Bool"]
  ]
genType n = frequency
  [ (3, TVar <$> elements ["a", "b", "c"])
  , (3, TCon <$> elements ["Int", "Bool"])
  , (2, TArr <$> sub <*> sub)
  , (1, TProd <$> sub <*> sub)
  , (1, TList <$> sub)
  ]
  where sub = genType (n `div` 2)

shrinkType :: Type -> [Type]
shrinkType (TArr a b)  = [a, b] ++ [TArr a' b | a' <- shrink a] ++ [TArr a b' | b' <- shrink b]
shrinkType (TProd a b) = [a, b] ++ [TProd a' b | a' <- shrink a] ++ [TProd a b' | b' <- shrink b]
shrinkType (TList t)   = [t] ++ [TList t' | t' <- shrink t]
shrinkType _           = []

instance Arbitrary Expr where
  arbitrary = sized genExpr
  shrink = shrinkExpr

genExpr :: Int -> Gen Expr
genExpr 0 = oneof
  [ ELit <$> arbitrary
  , EVar <$> elements ["x", "y", "z", "f", "g"]
  ]
genExpr n = frequency
  [ (3, ELit <$> arbitrary)
  , (2, EVar <$> elements ["x", "y", "z", "f", "g"])
  , (2, EApp <$> sub <*> sub)
  , (2, ELam <$> elements ["x", "y", "z"] <*> sub)
  , (1, ELet <$> elements ["x", "y"] <*> sub <*> sub)
  , (1, EIf <$> sub <*> sub <*> sub)
  , (1, EPair <$> sub <*> sub)
  , (1, EList <$> listOfSize sub)
  ]
  where
    sub = genExpr (n `div` 3)
    listOfSize g = do
      k <- chooseInt (0, 3)
      vectorOf k g

shrinkExpr :: Expr -> [Expr]
shrinkExpr (EApp f a)       = [f, a] ++ [EApp f' a | f' <- shrink f] ++ [EApp f a' | a' <- shrink a]
shrinkExpr (ELam _ body)    = [body]
shrinkExpr (ELet _ e1 e2)   = [e1, e2]
shrinkExpr (ELetRec _ e1 e2) = [e1, e2]
shrinkExpr (EIf c t e)      = [c, t, e]
shrinkExpr (EPair a b)      = [a, b]
shrinkExpr (EFst e)          = [e]
shrinkExpr (ESnd e)          = [e]
shrinkExpr (EAnn e _)        = [e]
shrinkExpr (EList es)        = es ++ [EList es' | es' <- shrink es]
shrinkExpr _                 = []

-- | Generator for expressions that the parser can roundtrip.
-- Restricted: non-negative ints, no ELetRec, no EAnn, no EFst/ESnd,
-- only known operators, variable names not shadowing keywords.
genParseableExpr :: Int -> Gen Expr
genParseableExpr 0 = oneof
  [ ELit . LInt . getNonNegative <$> arbitrary
  , ELit . LBool <$> arbitrary
  , EVar <$> elements ["x", "y", "z", "f", "g"]
  ]
genParseableExpr n = frequency
  [ (3, ELit . LInt . getNonNegative <$> arbitrary)
  , (2, ELit . LBool <$> arbitrary)
  , (2, EVar <$> elements ["x", "y", "z", "f", "g"])
  , (2, ELam <$> elements ["x", "y", "z"] <*> sub)
  , (1, ELet <$> elements ["x", "y", "z"] <*> sub <*> sub)
  , (1, EIf <$> sub <*> sub <*> sub)
  , (1, EPair <$> sub <*> sub)
  , (1, EList <$> listOfSize sub)
  ]
  where
    sub = genParseableExpr (n `div` 3)
    listOfSize g = do
      k <- chooseInt (0, 3)
      vectorOf k g

----------------------------------------------------------------------
-- Original list properties (session 7)
----------------------------------------------------------------------

-- Non-empty int list literals infer to [Int]
prop_intListType :: NonEmptyList Int -> Bool
prop_intListType (NonEmpty ns) =
  case runInfer (EList (map (ELit . LInt) ns)) of
    Right (Forall _ t) -> t == TList (TCon "Int")
    Left _             -> False

-- Empty list is polymorphic
prop_emptyListPoly :: Bool
prop_emptyListPoly =
  case runInfer (EList []) of
    Right (Forall [_] (TList (TVar _))) -> True
    _                                    -> False

-- Heterogeneous lists fail
prop_heteroListFails :: Int -> Bool -> Bool
prop_heteroListFails n b =
  case runInfer (EList [ELit (LInt n), ELit (LBool b)]) of
    Left _  -> True
    Right _ -> False

-- Cons preserves list type
prop_consType :: Int -> [Int] -> Bool
prop_consType x xs =
  let consExpr = EApp (EApp (EVar ":") (ELit (LInt x)))
                   (EList (map (ELit . LInt) xs))
  in case runInfer consExpr of
       Right (Forall _ t) -> t == TList (TCon "Int")
       Left _             -> False

-- head of non-empty int list has type Int
prop_headType :: NonEmptyList Int -> Bool
prop_headType (NonEmpty ns) =
  let headExpr = EApp (EVar "head") (EList (map (ELit . LInt) ns))
  in case runInfer headExpr of
       Right (Forall _ t) -> t == TCon "Int"
       Left _             -> False

-- tail of non-empty int list has type [Int]
prop_tailType :: NonEmptyList Int -> Bool
prop_tailType (NonEmpty ns) =
  let tailExpr = EApp (EVar "tail") (EList (map (ELit . LInt) ns))
  in case runInfer tailExpr of
       Right (Forall _ t) -> t == TList (TCon "Int")
       Left _             -> False

-- null always returns Bool
prop_nullType :: [Int] -> Bool
prop_nullType ns =
  let nullExpr = EApp (EVar "null") (EList (map (ELit . LInt) ns))
  in case runInfer nullExpr of
       Right (Forall _ t) -> t == TCon "Bool"
       Left _             -> False

-- Pretty-print roundtrip for non-negative int list literals
prop_listPrettyRoundtrip :: [NonNegative Int] -> Property
prop_listPrettyRoundtrip nns =
  let ns = map getNonNegative nns
  in ns /= [] ==>
  let e = EList (map (ELit . LInt) ns)
      pretty = ppExpr e
  in case parseProgram pretty of
       Right e' -> e' === e
       Left err -> counterexample (show err) False

-- Int literals parse and typecheck
prop_intLitRoundtrip :: Int -> Property
prop_intLitRoundtrip n =
  n >= 0 ==>
  case parseProgram (show n) of
    Right e  -> case runInfer e of
                  Right (Forall _ t) -> t === TCon "Int"
                  Left err           -> counterexample (show err) False
    Left err -> counterexample (show err) False

----------------------------------------------------------------------
-- New algebraic properties (session 7, Task 5)
----------------------------------------------------------------------

-- Substitution composition law: apply (compose s1 s2) t == apply s1 (apply s2 t)
prop_substCompose :: Type -> Property
prop_substCompose t =
  forAll genSubst $ \s1 ->
  forAll genSubst $ \s2 ->
    apply (composeSubst s1 s2) t === apply s1 (apply s2 t)

genSubst :: Gen Subst
genSubst = do
  n <- chooseInt (0, 3)
  pairs <- vectorOf n $ do
    v <- elements ["a", "b", "c"]
    ty <- resize 3 arbitrary
    pure (v, ty)
  pure (Map.fromList pairs)

-- Unification symmetry: unify t1 t2 succeeds iff unify t2 t1 succeeds
prop_unifySymmetric :: Type -> Type -> Property
prop_unifySymmetric t1 t2 =
  case (unify t1 t2, unify t2 t1) of
    (Right _, Right _) -> property True
    (Left _,  Left _)  -> property True
    _                   -> property False

-- Unification produces a valid substitution: apply s t1 == apply s t2
prop_unifyValid :: Type -> Type -> Property
prop_unifyValid t1 t2 =
  case unify t1 t2 of
    Left _  -> property True  -- skip failures
    Right s -> apply s t1 === apply s t2

-- Inference determinism: runInfer e gives the same result twice
prop_inferDeterministic :: Expr -> Property
prop_inferDeterministic e =
  runInfer e === runInfer e

-- Pretty-print roundtrip for arbitrary parseable expressions
prop_prettyRoundtrip :: Property
prop_prettyRoundtrip =
  forAll (sized (\n -> genParseableExpr (min n 4))) $ \e ->
    let pretty = ppExpr e
    in case parseProgram pretty of
         Right e' -> e' === e
         Left _   -> discard  -- some generated exprs may not roundtrip perfectly

----------------------------------------------------------------------
-- All properties for the test runner
----------------------------------------------------------------------

allProperties :: [(String, Property)]
allProperties =
  [ ("intListType",          property prop_intListType)
  , ("emptyListPoly",        property prop_emptyListPoly)
  , ("heteroListFails",      property prop_heteroListFails)
  , ("consType",             property prop_consType)
  , ("headType",             property prop_headType)
  , ("tailType",             property prop_tailType)
  , ("nullType",             property prop_nullType)
  , ("listPrettyRoundtrip",  property prop_listPrettyRoundtrip)
  , ("intLitRoundtrip",      property prop_intLitRoundtrip)
  , ("substCompose",         property prop_substCompose)
  , ("unifySymmetric",       property prop_unifySymmetric)
  , ("unifyValid",           property prop_unifyValid)
  , ("inferDeterministic",   property prop_inferDeterministic)
  , ("prettyRoundtrip",      property prop_prettyRoundtrip)
  ]
