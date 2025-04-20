module Eval (
  eval
) where 

import Expr 
import Control.Monad.State


eval :: Expr -> Env -> State [Mem] Value
eval (Number n)    _ = return $ NumVal n 
eval (Boolean b)   _ = return $ BoolVal b 

eval (TermExpr t)  _ = return $ NumVal $ evalTerm t
eval (Add e1 e2) env = do 
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 + n2)
    _ -> error "Type error in Add: expected NumVal"
eval (Sub e1 e2) env = do 
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 - n2)
    _ -> error "Type error in Add: expected NumVal"

eval (Var v) _     = undefined

evalTerm :: Term -> Int
evalTerm (FactorTerm f) = evalFactor f
evalTerm (Mult t1 t2)   = evalTerm t1 * evalTerm t2
evalTerm (Div t1 t2)    = evalTerm t1 `div` evalTerm t2

evalFactor :: Factor -> Int
evalFactor (Num n)     = n
evalFactor (Bracket e) = 
  case evalState (eval e []) [] of
    NumVal n -> n
    _        -> error "Expected NumVal in Bracket"



-- evalExpr' (Expr t) env = evalTerm t env

-- eval (Expr'(Add e1 e2)) env = do 
--   ~(NumVal n1) <- eval e1 env 
--   ~(NumVal n2) <- eval e2 env 
--   return $ NumVal $ n1 + n2 

-- eval (Expr' (Sub e1 e2)) env = do 
--   ~(NumVal n1) <- eval e1 env 
--   ~(NumVal n2) <- eval e2 env 
--   return $ NumVal $ n1 + n2 
