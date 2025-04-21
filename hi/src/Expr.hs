module Expr (
  Expr(..),
  Value(..),
  -- Term(..),
  -- Factor(..),
  Ident,
  Defn(..),
  Env,
  Mem
) where

-- Identifier
type Ident = String 

data Expr = Number Int
          | Boolean Bool
          -- | TermExpr Term
          | Add Expr Expr
          | Sub Expr Expr 
          | Mult Expr Expr
          | Div Expr Expr
          | Var Ident
          | Equals Expr Expr
          | Gt Expr Expr
          | Lt Expr Expr
          | If Expr Expr Expr
          | Let Defn Expr 
          | Lam [Ident] Expr 
          | Apply Expr [Expr]
          -- | New
          -- | Deref Expr 
          -- | Seq Expr Expr
          -- | Assign Expr Expr
          deriving (Show, Eq, Ord)

-- 项
-- data Term = FactorTerm Factor | Mult Term Term | Div Term Term
--   deriving (Show, Eq, Ord)

-- -- 因子 (数字 或 (Expr))
-- data Factor = Bracket Expr | Num Int
--   deriving (Show, Eq, Ord)
        
type Env = [(Ident, Value)]

-- return value
data Value = NumVal Int 
          | BoolVal Bool 
          | Closure [Ident] Expr Env 
          | Null 
          | MemAddr Int
  deriving (Show, Eq, Ord)

type Mem = Env

data Defn = Val Ident Expr
          | Rec Ident Expr
          deriving (Show, Eq, Ord)