module Expr (
  Expr(..),
  Value(..),
  Term(..),
  Factor(..),
  Env,
  Mem
) where

-- Identifier
type Ident = String 

data Expr = Number Int
          | Boolean Bool
          | TermExpr Term
          | Add Expr Expr
          | Sub Expr Expr 
          | Var Ident
          -- | If Expr Expr Expr
          -- | Equals Expr Expr
          -- | Let Defn Expr 
          -- | Lam [Ident] Expr 
          -- | Apply Expr [Expr]
          -- | New
          -- | Deref Expr 
          -- | Seq Expr Expr
          -- | Assign Expr Expr
          deriving (Show)

-- 项
data Term = FactorTerm Factor | Mult Term Term | Div Term Term
  deriving (Show)

-- 因子 (数字 或 (Expr))
data Factor = Bracket Expr | Num Int
  deriving (Show)
        
type Env = [(Ident, Value)]

-- return value
data Value = NumVal Int 
          | BoolVal Bool 
          | Closure [Ident] Expr Env 
          | Null 
          | MemAddr Int
  deriving (Show)

type Mem = [Value]

