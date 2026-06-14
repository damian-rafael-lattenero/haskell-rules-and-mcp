module Lambda.Syntax where


type Name = String

data Term
    = Var Name
    | Lam Name Term
    | App Term Term
    | Let Name Term Term
    | Lit Lit
    deriving (Eq)

data Lit
    = LInt  Int
    | LBool Bool
    deriving (Eq)

instance Show Lit where
    show (LInt  n) = show n
    show (LBool b) = if b then "true" else "false"

instance Show Term where
    show (Var x)       = x
    show (Lam x t)     = "\\" ++ x ++ ". " ++ show t
    show (App t1 t2)   = showApp t1 ++ " " ++ showAtom t2
    show (Let x t1 t2) = "let " ++ x ++ " = " ++ show t1 ++ " in " ++ show t2
    show (Lit l)       = show l

showAtom :: Term -> String
showAtom t@(Var _) = show t
showAtom t@(Lit _) = show t
showAtom t         = "(" ++ show t ++ ")"

showApp :: Term -> String
showApp t@(App _ _) = show t
showApp t           = showAtom t

-- | Monomorphic types
data Type
    = TVar Name
    | TInt
    | TBool
    | TArr Type Type
    deriving (Eq)

instance Show Type where
    show (TVar a)   = a
    show TInt       = "Int"
    show TBool      = "Bool"
    show (TArr a b) = showTAtom a ++ " -> " ++ show b

showTAtom :: Type -> String
showTAtom t@(TVar _) = show t
showTAtom TInt        = "Int"
showTAtom TBool       = "Bool"
showTAtom t           = "(" ++ show t ++ ")"

-- | Polytypes: forall a1..an. t
data Scheme = Forall [Name] Type deriving (Eq)

instance Show Scheme where
    show (Forall [] t) = show t
    show (Forall vs t) = "forall " ++ unwords vs ++ ". " ++ show t

freeTVars :: Type -> [Name]
freeTVars (TVar a)   = [a]
freeTVars TInt        = []
freeTVars TBool       = []
freeTVars (TArr a b) = freeTVars a ++ freeTVars b

freeTVarsScheme :: Scheme -> [Name]
freeTVarsScheme (Forall vs t) = filter (`notElem` vs) (freeTVars t)
