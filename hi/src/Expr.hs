module Expr (
  Expr(..),
  Value(..),
  Ident,
  Defn(..),
  Env,
  Mem
) where

-- Identifier
type Ident = String 

data Expr = Number Int
          | Boolean Bool
          | Add Expr Expr
          | Sub Expr Expr 
          | Mult Expr Expr
          | Div Expr Expr
          | Var Ident
          | Equals Expr Expr
          | Gt Expr Expr
          | Lt Expr Expr
          | Set Ident Expr
          | If Expr Expr Expr
          | Let Defn Expr 
          | Lam [Ident] Expr 
          | Apply Expr [Expr]
          | Seq [Expr]
          -- | Where Expr [Expr]
          -- | New
          -- | Deref Expr 
          -- | Assign Expr Expr
          deriving (Show, Eq, Ord)


        
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