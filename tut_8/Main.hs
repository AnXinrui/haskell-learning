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
          | Apply Expr [Expr] -- fib 10 :: Apply (Lam ["n"] ...) [Number 10]
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
eval (Plus e1 e2) env = let (NumVal r1) = eval e1 env in
                    let (NumVal r2) = eval e2 env in
                    NumVal (r1 + r2)
eval (Minus e1 e2) env = let (NumVal r1) = eval e1 env in
                    let (NumVal r2) = eval e2 env in
                    NumVal (r1 - r2)
eval (Var i) env = find env i 
eval (If g e1 e2) env = case eval g env of 
                          (BoolVal True)  -> eval e1 env 
                          (BoolVal False) -> eval e2 env
eval (Equals e1 e2) env = BoolVal $ eval e1 env == eval e2 env
eval (Let d e) env = eval e (elab env d)
eval (Lam ids e) env = Closure ids e env 
eval (Apply f xs) env = apply f' xs'
  where f' = eval f env
        xs' = map (flip eval env) xs

apply :: Value -> [Value] -> Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = error "using a value as if it's a function"
{-
λ> e = Let "add" (Lam ["x","y"] (Plus (Var "x") (Var "y"))) (Apply (Var "add") [Number 1, Number 2])
λ> eval e
-}

find :: Env -> Ident -> Value
-- e = Let "x" (Number 10) (Minus (Number 22) (Var "x"))
find env i = snd $ head $ filter (\(i', _) -> i' == i) env 

elab env (Val i e) = (i, eval e env) : env 
elab env (Rec i (Lam args e)) = env' where env' = (i, Closure args e env') : env
elab _ _ = error "only lambdas can be recursive "

-- e = Let (Val "x" $ (Number 10)) (Minus (Number 22) (Var "x"))
{-
e = Let (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (Plus (Var "n") (Apply (Var "sum") [Minus (Var "n") (Number 1)]))))) (Apply (Var "sum") (Number 3))

-}

{-

-}