type Ident = String

newtype Id a = Id a

instance Functor Id where 
  fmap f (Id x) = Id $ f x 

instance Applicative Id where
  (Id f) <*> (Id x) = Id $ f x 
  pure = Id 

instance Monad Id where
  (Id x) >>= f = f x

type M a = Id a 

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

eval :: Expr -> Env -> M Value
eval (Number i) env = return $ NumVal i 
eval (Boolean b) env = return $ BoolVal b
eval (Plus e1 e2) env = do
  (NumVal r1) <- eval e1 env 
  (NumVal r2) <- eval e2 env 
  return $ NumVal (r1 + r2)
eval (Minus e1 e2) env = do
  (NumVal r1) <- eval e1 env 
  (NumVal r2) <- eval e2 env 
  return $ NumVal (r1 - r2)
eval (Var i) env = return $ find env i 
eval (If g e1 e2) env = eval g env >>= \r ->
  case r of (BoolVal True)  -> eval e1 env 
            (BoolVal False) -> eval e2 env

eval (Equals e1 e2) env = BoolVal <$> ((==) <$> eval e1 env <*> eval e2 env)
eval (Let d e) env = elab e nv d >>= eval e
eval (Lam ids e) env = return $ Closure ids e env 
eval (Apply f xs) env = do 
  f' <- eval f env 
  xs' <- mapM (flip eval env) xs
  apply f' xs'
  

apply :: Value -> [Value] -> M Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = error "using a value as if it's a function"
{-
λ> e = Let "add" (Lam ["x","y"] (Plus (Var "x") (Var "y"))) (Apply (Var "add") [Number 1, Number 2])
λ> eval e
-}

find :: Env -> Ident -> Value
-- e = Let "x" (Number 10) (Minus (Number 22) (Var "x"))
find env i = snd $ head $ filter (\(i', _) -> i' == i) env 

elab env (Val i e) = return $ eval e env >>= \r -> (i, r) : env 
elab env (Rec i l@(Lam args e)) = env' 
  where env' = env' >>= eval l >>= \e' -> return $ (i, e') : env
elab _ _ = error "only lambdas can be recursive "

-- e = Let (Val "x" $ (Number 1 0)) (Minus (Number 22) (Var "x"))
{-
e = Let (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (Plus (Var "n") (Apply (Var "sum") [Minus (Var "n") (Number 1)]))))) (Apply (Var "sum") (Number 3))

-}