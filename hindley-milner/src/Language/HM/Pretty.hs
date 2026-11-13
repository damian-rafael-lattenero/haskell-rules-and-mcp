module Language.HM.Pretty
  ( prettyType
  , prettyScheme
  , prettyTerm
  , renderType
  , renderScheme
  , renderTerm
  ) where

import Data.Text (Text)
import Language.HM.Types
import Prettyprinter
import Prettyprinter.Render.Text

prettyType :: Type -> Doc ann
prettyType = \case
  TVar v -> pretty v
  TCon c -> pretty c
  TApp (TApp (TCon "->") t1) t2 ->
    parensIf (isArrow t1) (prettyType t1) <+> "->" <+> prettyType t2
  TApp t1 t2 ->
    parensIf (not (isAtomic t1)) (prettyType t1) <+> prettyType t2

prettyScheme :: Scheme -> Doc ann
prettyScheme (Forall [] ty) = prettyType ty
prettyScheme (Forall vars ty) =
  "forall" <+> hsep (map pretty vars) <> "." <+> prettyType ty

prettyTerm :: Term -> Doc ann
prettyTerm = \case
  Var x _ -> pretty x
  Lam x body _ -> "\\" <> pretty x <+> "->" <+> prettyTerm body
  App f a _ -> parensIf (not (isAtomicTerm f)) (prettyTerm f) <+> prettyTerm a
  Let x rhs body _ -> "let" <+> pretty x <+> "=" <+> prettyTerm rhs <+> "in" <+> prettyTerm body
  Lit lit _ -> prettyLit lit
  Ann e ty _ -> parensIf True (prettyTerm e) <+> "::" <+> prettyType ty

prettyLit :: Literal -> Doc ann
prettyLit (LInt i)  = pretty i
prettyLit (LBool b) = pretty b

isArrow :: Type -> Bool
isArrow (TApp (TApp (TCon "->") _) _) = True
isArrow _ = False

isAtomic :: Type -> Bool
isAtomic (TVar _) = True
isAtomic (TCon _) = True
isAtomic _ = False

isAtomicTerm :: Term -> Bool
isAtomicTerm (Var _ _) = True
isAtomicTerm (Lit _ _) = True
isAtomicTerm _ = False

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  = parens
parensIf False = id

renderType :: Type -> Text
renderType = renderStrict . layoutSmart defaultLayoutOptions . prettyType

renderScheme :: Scheme -> Text
renderScheme = renderStrict . layoutSmart defaultLayoutOptions . prettyScheme

renderTerm :: Term -> Text
renderTerm = renderStrict . layoutSmart defaultLayoutOptions . prettyTerm