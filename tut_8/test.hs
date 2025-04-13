import Debug.Trace
import Data.List (intercalate)

type Ident = String

data Expr = Number Int
          | Boolean Bool
          | Plus Expr Expr
          | Minus Expr Expr
          | Var Ident
          | If Expr Expr Expr
          | Equals Expr Expr
          | Let Defn Expr 
          | Lam [Ident] Expr 
          | Apply Expr [Expr]
          deriving (Show, Eq)

data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env
  deriving (Show, Eq)

data Defn = Val Ident Expr
          | Rec Ident Expr
          deriving (Show, Eq)

type Env = [(Ident, Value)]

eval :: Expr -> Env -> Value
eval (Number i) env = NumVal i 
eval (Boolean b) env = BoolVal b
eval (Plus e1 e2) env = 
    let (NumVal r1) = eval e1 env
        (NumVal r2) = eval e2 env 
    in NumVal (r1 + r2)
eval (Minus e1 e2) env = 
    let (NumVal r1) = eval e1 env
        (NumVal r2) = eval e2 env 
    in NumVal (r1 - r2)
eval (Var i) env = find env i 
eval (If g e1 e2) env = case eval g env of 
                          (BoolVal True)  -> eval e1 env 
                          (BoolVal False) -> eval e2 env
eval (Equals e1 e2) env = BoolVal $ eval e1 env == eval e2 env
eval (Let d e) env = eval e (elab env d)
eval (Lam ids e) env = Closure ids e env 
eval (Apply f xs) env = 
  let f' = eval f env
      xs' = map (flip eval env) xs
  in apply f' xs'

apply :: Value -> [Value] -> Value
apply clo@(Closure ids e env) vals =
    let newEnv = zip ids vals ++ env
        envStr = intercalate ", " [k ++ "=" ++ show v | (k, v) <- newEnv]
    in trace ("Applying closure with env: [" ++ envStr ++ "]") (eval e newEnv)
apply _ _ = error "using a value as if it's a function"

find :: Env -> Ident -> Value
find env i = snd $ head $ filter (\(i', _) -> i' == i) env 

elab :: Env -> Defn -> Env
elab env (Val i e) = (i, eval e env) : env 
elab env (Rec i (Lam args e)) = env' where env' = (i, Closure args e env') : env
elab _ _ = error "only lambdas can be recursive "

-- 递归定义 sum 函数并应用到 4
main :: IO ()
main = do
  let expr = Let 
                (Rec "sum" 
                  (Lam ["n"] 
                    (If 
                      (Equals (Var "n") (Number 0)) 
                      (Number 0) 
                      (Plus 
                        (Var "n") 
                        (Apply (Var "sum") [Minus (Var "n") (Number 1)])
                      )
                    )
                  )
                ) 
                (Apply (Var "sum") [Number 4])
  print $ eval expr []
