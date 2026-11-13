module Language.HM
  ( module Language.HM.Types
  , module Language.HM.Env
  , module Language.HM.Unify
  , module Language.HM.Infer
  , module Language.HM.Error
  , module Language.HM.Parser
  , module Language.HM.Pretty
  , inferType
  , inferType'
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.HM.Types
import Language.HM.Env
import Language.HM.Unify
import Language.HM.Infer
import Language.HM.Error
import Language.HM.Parser
import Language.HM.Pretty
import Text.Megaparsec (errorBundlePretty)

inferType :: Text -> Either Text (Scheme, Type)
inferType input = do
  term <- case parseTerm input of
    Left err -> Left (T.pack $ show err)
    Right t  -> Right t
  case infer emptyEnv term of
    Left err -> Left (renderInferError err)
    Right (subst, ty) -> Right (generalize emptyEnv (applySubst subst ty), applySubst subst ty)

inferType' :: Env -> Text -> Either Text (Scheme, Type)
inferType' env input = do
  term <- case parseTerm input of
    Left err -> Left (T.pack $ show err)
    Right t  -> Right t
  case infer env term of
    Left err -> Left (renderInferError err)
    Right (subst, ty) -> Right (generalize env (applySubst subst ty), applySubst subst ty)