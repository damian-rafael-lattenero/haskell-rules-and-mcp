-- | Core syntax types for arithmetic expressions.
module Expr.Syntax
    ( Expr (..)
    , Error (..)
    , Env
    ) where

import Data.Map.Strict (Map)

-- | An arithmetic expression tree.
--
-- Convention: 'Lit' carries non-negative integers only.
-- Negative values are represented as @Neg (Lit n)@ with @n > 0@.
-- This keeps the pretty-printer / parser roundtrip unambiguous.
data Expr
    = Lit Int          -- ^ integer literal (>= 0 by convention)
    | Add Expr Expr    -- ^ addition
    | Mul Expr Expr    -- ^ multiplication
    | Neg Expr         -- ^ arithmetic negation
    | Var String       -- ^ variable reference
    deriving stock (Show, Eq)

-- | Errors that can occur during evaluation.
data Error
    = UnboundVar String  -- ^ variable not found in the environment
    | DivisionByZero     -- ^ reserved for future division support
    deriving stock (Show, Eq)

-- | Variable environment: maps names to integer values.
type Env = Map String Int
