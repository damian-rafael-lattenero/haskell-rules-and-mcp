module Lambda.Pretty
    ( prettyTerm
    , prettyType
    , prettyScheme
    , prettyValue
    , prettyResult
    ) where

import Lambda.Syntax
import Lambda.Eval (Value(..))
import Lambda.Infer (TypeEnv, inferScheme)

prettyTerm :: Term -> String
prettyTerm = show

prettyType :: Type -> String
prettyType = show

prettyScheme :: Scheme -> String
prettyScheme = show

prettyValue :: Value -> String
prettyValue = show

-- | Run inference + evaluation and format a human-readable result line.
prettyResult :: TypeEnv -> Term -> String
prettyResult env term =
    case inferScheme env term of
        Left err -> "Type error: " ++ err
        Right sc -> prettyTerm term ++ " : " ++ prettyScheme sc
