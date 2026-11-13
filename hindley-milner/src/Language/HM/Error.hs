module Language.HM.Error
  ( InferError(..)
  , renderInferError
  , SourcePos(..)
  , SourceError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.HM.Types
import Language.HM.Unify (UnifyError(..))
import Prettyprinter
import Prettyprinter.Render.Text

data InferError
  = UnboundVar Name Pos
  | UnificationFail Type Type Pos
  | OccursCheck TyVar Type Pos
  | AnnotationMismatch Type Type Pos
  | ParserError Text Pos
  deriving (Eq, Show)

data SourcePos = SourcePos
  { srcFile   :: FilePath
  , srcLine   :: Int
  , srcColumn :: Int
  } deriving (Eq, Show)

data SourceError = SourceError
  { errPos   :: SourcePos
  , errMsg   :: Text
  , errCode  :: Text
  } deriving (Eq, Show)

renderInferError :: InferError -> Text
renderInferError err = T.strip $ renderStrict $ layoutSmart defaultLayoutOptions $ pretty err

instance Pretty InferError where
  pretty (UnboundVar name pos) =
    "Unbound variable" <+> squotes (pretty name) <+> "at" <+> prettyPos pos
  pretty (UnificationFail t1 t2 pos) =
    "Cannot unify" <+> pretty t1 <+> "with" <+> pretty t2 <+> "at" <+> prettyPos pos
  pretty (OccursCheck var ty pos) =
    "Occurs check failed:" <+> pretty var <+> "occurs in" <+> pretty ty <+> "at" <+> prettyPos pos
  pretty (AnnotationMismatch inferred expected pos) =
    "Type annotation mismatch:" <+>
    "inferred" <+> pretty inferred <+>
    "but expected" <+> pretty expected <+>
    "at" <+> prettyPos pos
  pretty (ParserError msg pos) =
    "Parse error:" <+> pretty msg <+> "at" <+> prettyPos pos

prettyPos :: Pos -> Doc ann
prettyPos (Pos l c _) = "line" <+> pretty l <> "," <+> "column" <+> pretty c

instance Pretty Type where
  pretty (TVar v)     = pretty v
  pretty (TCon c)     = pretty c
  pretty (TApp t1 t2) = parens (pretty t1 <+> "->" <+> pretty t2)

instance Pretty Scheme where
  pretty (Forall [] ty) = pretty ty
  pretty (Forall vars ty) =
    "forall" <+> hsep (map pretty vars) <> "." <+> pretty ty

instance Pretty Literal where
  pretty (LInt i)  = pretty i
  pretty (LBool b) = pretty b