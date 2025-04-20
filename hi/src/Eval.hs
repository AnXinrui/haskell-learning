module Eval (
  eval
) where 

import Expr 
import Control.Monad.State


eval :: Expr -> Env -> State [Mem] Value
eval (Number n)  _ = return $ NumVal n 
eval (Boolean b) _ = return $ BoolVal b 
eval (Expr' e) env = evalExpr' e env
eval (Var v) _ = undefined


-- 计算 Expr' 类型的表达式
evalExpr' :: Expr' -> Env -> State [Mem] Value
evalExpr' (Add e1 e2) env = do
  v1 <- evalExpr' e1 env
  v2 <- evalExpr' e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 + n2)
    _ -> error "Type error in Add: expected NumVal"

evalExpr' (Sub e1 e2) env = do
  v1 <- evalExpr' e1 env
  v2 <- evalExpr' e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 - n2)
    _ -> error "Type error in Sub: expected NumVal"

-- evalExpr' (Expr t) env = evalTerm t env

-- eval (Expr'(Add e1 e2)) env = do 
--   ~(NumVal n1) <- eval e1 env 
--   ~(NumVal n2) <- eval e2 env 
--   return $ NumVal $ n1 + n2 

-- eval (Expr' (Sub e1 e2)) env = do 
--   ~(NumVal n1) <- eval e1 env 
--   ~(NumVal n2) <- eval e2 env 
--   return $ NumVal $ n1 + n2 
