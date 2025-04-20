module Expr (
  Expr(..),
  Expr'(..),
  Value(..),
  Env,
  Mem
) where

-- Identifier
type Ident = String 

data Expr = Number Int
          | Boolean Bool
          | Expr' Expr' 
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
          deriving (Show, Eq)

data Expr' = Expr Term | Add Expr' Expr' | Sub Expr' Expr' 
  deriving (Show)

data Term = Term Factor | Mult Term Term | Div Term Term
  deriving (Show)

data Factor = Bracket Expr | Num Int
  deriving (Show)
        
type Env = [(Ident, Value)]

data Value = NumVal Int 
          | BoolVal Bool 
          | Closure [Ident] Expr Env 
          | Null 
          | MemAddr Int
  deriving (Show, Eq)

type Mem = [Value]

