type Ident = String

newtype Id a = Id a deriving (Show)

instance Functor Id where 
  fmap f (Id x) = Id $ f x 

instance Applicative Id where
  (Id f) <*> (Id x) = Id $ f x 
  pure = Id 

instance Monad Id where
  (Id x) >>= f = f x

instance MonadFail (Either String) where
  fail = Left

data State m a = St {runState :: (m -> (a, m))}

instance Functor (State m) where 
  fmap f (St x) = St $ \m -> let (a, m') = x m 
                             in (f a, m')

instance Applicative (State m) where
  pure x = St $ \m -> (x, m)
  St f <*> St x = St $ \m -> let (f', m' ) = f m in 
                             let (x', m'') = x m' in 
                             (f' x', m'')

instance Monad (State m) where
  -- (>>=) :: State m a -> (a -> State m b) -> State m b
  (St xm) >>= f = St $ \m -> let (x, m') = xm m in 
                             let (St g) = f x in 
                          g m' 

get :: State m m 
get = St $ \m -> (m, m)

set :: a -> State a ()
set x = St $ \m -> ((), x)

type Mem = [Value]

-- type M a = Either String a
type M a = State Mem a

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
          | New
          | Deref Expr 
          | Seq Expr Expr
          | Assign Expr Expr
          deriving (Show, Eq)

data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env | Null | MemAddr Int
  deriving (Show, Eq)

data Defn = Val Ident Expr
          | Rec Ident Expr
          deriving (Show, Eq)

type Env = [(Ident, Value)]

eval :: Expr -> Env -> M Value              -- State [Int] Value
eval (Number i) env = return $ NumVal i 
eval (Boolean b) env = return $ BoolVal b
eval (Plus e1 e2) env = do
  ~(NumVal r1) <- eval e1 env 
  ~(NumVal r2) <- eval e2 env 
  return $ NumVal (r1 + r2)
eval (Minus e1 e2) env = do
  ~(NumVal r1) <- eval e1 env 
  ~(NumVal r2) <- eval e2 env 
  return $ NumVal (r1 - r2)
eval (Var i) env = return $ find env i 
eval (If g e1 e2) env = eval g env >>= \r ->
  case r of (BoolVal True)  -> eval e1 env 
            (BoolVal False) -> eval e2 env

eval (Equals e1 e2) env = BoolVal <$> ((==) <$> eval e1 env <*> eval e2 env)
-- eval (Let d e) env = elab env d >>= \env' -> eval e env' 
eval (Let d e) env = elab env d >>= eval e
eval (Lam ids e) env = return $ Closure ids e env 
eval (Apply f xs) env = do 
  f' <- eval f env 
  xs' <- mapM (flip eval env) xs
  apply f' xs'

eval (New) env = do
  mem <- get
  let ret = length mem
  set $ mem ++ [Null]
  return $ MemAddr ret


eval (Deref e) env = do 
  ~(MemAddr i) <- eval e env
  mem <- get
  return $ mem !! i 
                  
eval (Seq e1 e2)    env = eval e1 env >> eval e2 env 

eval (Assign e1 e2) env = do  
  ~(MemAddr i) <- eval e1 env
  e2' <- eval e2 env
  mem <- get
  let mem' = take i mem ++ [e2'] ++ drop i mem
  set mem'
  return Null
                             

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

elab env (Val i e) = eval e env >>= \r -> return $ (i, r) : env
elab env (Rec i l@(Lam args e)) = return env' 
  where env' = (i, Closure args e env') : env
  -- where env' = env' >>= eval l >>= \e' -> return $ (i, e') : env
elab _ _ = error "only lambdas can be recursive "

-- e = Let (Val "x" $ (Number 1 0)) (Minus (Number 22) (Var "x"))
{-
e = Let (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (Plus (Var "n") (Apply (Var "sum") [Minus (Var "n") (Number 1)]))))) (Apply (Var "sum") [Number 3])

-}
fibExpr :: Expr
fibExpr =
  Let
    (Rec "fib"
      (Lam ["n"]
        (If (Equals (Var "n") (Number 0))
            (Number 0)
            (If (Equals (Var "n") (Number 1))
                (Number 1)
                (Plus
                  (Apply (Var "fib") [Minus (Var "n") (Number 1)])
                  (Apply (Var "fib") [Minus (Var "n") (Number 2)])
                )
            )
        )
      )
    )
    (Apply (Var "fib") [Number 5])

f = Let
    (Rec "fib"
      (Lam ["n"]
        (If (Equals (Var "n") (Number 0))
            (Number 0)
            (If (Equals (Var "n") (Number 1))
                (Number 1)
                (Plus
                  (Apply (Var "fib") [Minus (Var "n") (Number 1)])
                  (Apply (Var "fib") [Minus (Var "n") (Number 2)])
                )
            )
        )
      )
    )
e = (Let (Val "n" (Number 10)) (Var "n"))

{-
e = (Let (Val "n" (Number 10)) (Var "n"))
let res = eval e []
runState M []
-}

env = []
main = do 
  print "hello world"
  -- runTest 
  -- print $ eval e env 
  -- print $ eval (Apply (Var "fib") [Number 5]) env
  -- print $ eval e env
  -- print (env :: Env)
  -- print $ eval (Apply (Var "fib") [Number 5]) []

  -- f = Let (Rec "fib" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (If (Equals (Var "n") (Number 1)) (Number 1) (Plus (Apply (Var "fib") [Minus (Var "n") (Number 1)]) (Apply (Var "fib") [Minus (Var "n") (Number 2)]))))))

  -- eval (f (Apply (Var "fib") [Number 5])) []


let e = Let ((Val "x") New) (Seq (Assign (Var "x") (Number 42)) (Assign (Var "x") (Plus (Deref (Var "x")) (Number 1))))
-- runTest :: IO()
-- runTest = do 
