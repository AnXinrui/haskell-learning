{-# LANGUAGE LambdaCase #-}

module Eval (
  eval
) where 

import Expr 
import Control.Monad.State
import Data.Functor.Identity

eval :: Expr -> Env -> State Mem Value
eval (Number n)    _ = return $ NumVal n 
eval (Boolean b)   _ = return $ BoolVal b 

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

eval (Mult e1 e2) env = do 
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 * n2)
    _ -> error "Type error in Add: expected NumVal"

eval (Div e1 e2) env = do 
  v1 <- eval e1 env
  v2 <- eval e2 env
  case (v1, v2) of
    (NumVal n1, NumVal n2) -> return $ NumVal (n1 `div` n2)
    _ -> error "Type error in Add: expected NumVal"


eval (Var v)     env =do 
  case find env v of 
    Just val -> return val 
    Nothing -> do 
      mem <- get 
      case find mem v of 
        Just val -> return val 
        Nothing  -> error $ "undefined variable: " ++ v

eval (Equals e1 e2) env = BoolVal <$> ((==) <$> eval e1 env <*> eval e2 env)
eval (Gt     e1 e2) env = BoolVal <$> ((>) <$> eval e1 env <*> eval e2 env)
eval (Lt     e1 e2) env = BoolVal <$> ((<) <$> eval e1 env <*> eval e2 env)

eval (Set i e) env = do 
  val <- eval e env
  mem <- get
  put $ (i, val) : mem  
  return $ BoolVal True

eval (If g e1 e2) env = eval g env >>= \case
  (BoolVal True)  -> eval e1 env
  (BoolVal False) -> eval e2 env
  _               -> error "Type error in If: expected Boolean"

eval (Let d e) env = elab env d >>= eval e 

eval (Lam ids e)  env = return $ Closure ids e env 
eval (Apply f xs) env = do 
  f' <- eval f env 
  xs' <- mapM (`eval` env) xs
  apply f' xs' 

eval (Seq [e])      env =  eval e env 
eval (Seq (e : es)) env = do 
  _   <- eval e env 
  eval (Seq es) env 
eval (Seq [])         _ = return $ BoolVal False


find :: Env -> Ident -> Maybe Value
find []  _ = Nothing
find ((i', x):xs) i
  | i' == i  = Just x
  | otherwise = find xs i

 
elab :: Env -> Defn -> StateT Mem Identity [(Ident, Value)]
elab env (Val i e)           = eval e env >>= \r -> return $ (i, r) : env
elab env (Rec i (Lam ids e)) = return env' 
  where env' = (i, Closure ids e env') : env 
elab _ _                     = error "only lambdas can be recursive!"

apply :: Value -> [Value] -> State Mem Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _                      = error "using a value as if it's a function"