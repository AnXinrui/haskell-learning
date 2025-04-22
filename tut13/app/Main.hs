-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

-- module Main (main) where

-- import Data.Attoparsec.Text hiding (take)
-- import Control.Applicative
-- import qualified Data.Text as T 


-- type Ident = String

-- newtype Id a = Id a deriving (Show)

-- instance Functor Id where 
--   fmap f (Id x) = Id $ f x 

-- instance Applicative Id where
--   (Id f) <*> (Id x) = Id $ f x 
--   pure = Id 

-- instance Monad Id where
--   (>>=) :: Id a -> (a -> Id b) -> Id b
--   (Id x) >>= f = f x

-- data State m a = St {runState :: (m -> (a, m))}

-- instance Functor (State m) where 
--   fmap f (St x) = St $ \m -> let (a, m') = x m 
--                              in (f a, m')

-- instance Applicative (State m) where
--   pure x = St $ \m -> (x, m)
--   St f <*> St x = St $ \m -> let (f', m' ) = f m in 
--                              let (x', m'') = x m' in 
--                              (f' x', m'')

-- instance Monad (State m) where
--   (St xm) >>= f = St $ \m -> let (x, m') = xm m in 
--                              let (St g) = f x in 
--                           g m' 

-- get :: State m m 
-- get = St $ \m -> (m, m)

-- set :: a -> State a ()
-- set x = St $ \m -> ((), x)

-- type Mem = [Value]

-- type M a = State Mem a

-- data Expr = Number Int
--           | Boolean Bool
--           | Plus Expr Expr
--           | Minus Expr Expr
--           | Var Ident
--           | If Expr Expr Expr
--           | Equals Expr Expr
--           | Let  Defn Expr 
--           | Lam [Ident] Expr 
--           | Apply Expr [Expr] -- fib 10 :: Apply (Lam ["n"] ...) [Number 10]
--           | New
--           | Deref Expr 
--           | Seq Expr Expr
--           | Assign Expr Expr
--           deriving (Show, Eq)

-- data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env | Null | MemAddr Int
--   deriving (Show, Eq)

-- data Defn = Val Ident Expr
--           | Rec Ident Expr
--           deriving (Show, Eq)

-- parseFun t = parseOnly parseExpr (T.pack t)

-- parseExpr = parsePM <|> parseExpr'

-- parseExpr' :: Parser Expr 
-- parseExpr' = parseConst
--           <|> parseIf
--           <|> parseLet
--           <|> parseLam
--           <|> parseApp 
--           <|> parseVal


-- ss = skipSpace

-- atom = T.unpack <$> takeWhile1 (\c -> c /=' ' && c /= '"' && c /= '-' && c /= ':' )

-- parseConst :: Parser Expr
-- parseConst = Number <$> decimal 
--           <|> "True"  *> pure (Boolean True)
--           <|> "False" *> pure (Boolean False)

-- parseIf :: Parser Expr
-- parseIf = do 
--   "if" *> ss 
--   cond <- parseExpr <* ss 
--   "then" *> ss 
--   e1 <- parseExpr <* ss
--   "else" *> ss
--   e2 <- parseExpr <* ss
--   return (If cond e1 e2)

-- parseVal = Var <$> atom

-- parseLet = do 
--   "let" *> ss
--   d <- parseDefn <* ss 
--   "in" *> ss 
--   e <- parseExpr 
--   return $ Let d e 

-- parseDefn = Val <$> ("val" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr
--         <|> Rec <$> ("rec" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr


-- parseLam = do 
--   char '!'
--   atoms <- many1 (atom <* ss)
--   "->" *> ss 
--   exr <- parseExpr
--   return $ Lam atoms exr

-- parsePM = parsePM' <|> parseTerm

-- parsePM' = do 
--   t <- parseTerm <* ss
--   con <- (char '+' *> pure Plus
--       <|> char '-' *> pure Minus)
--   ss 
--   e <- parseExpr
--   return $ con t e 

-- parseTerm = parseTerm' <|> parseFactor

-- parseTerm' = do 
--   f <- parseFactor <* ss 
--   "==" *> ss
--   t <- parseTerm
--   return $ Equals f t

-- parseFactor = char '(' *> parseExpr <* char ')'
--           <|> parseExpr'

-- parseApp = do 
--   f <- "%{" *> parseExpr
--   char ':' *> ss 
--   ins <- many (parseExpr <* ss) 
--   return $ Apply f ins

-- type Env = [(Ident, Value)]

-- eval :: Expr -> Env -> M Value              -- State [Int] Value
-- eval (Number i) env = return $ NumVal i 
-- eval (Boolean b) env = return $ BoolVal b
-- eval (Plus e1 e2) env = do
--   ~(NumVal r1) <- eval e1 env 
--   ~(NumVal r2) <- eval e2 env 
--   return $ NumVal (r1 + r2)
-- eval (Minus e1 e2) env = do
--   ~(NumVal r1) <- eval e1 env 
--   ~(NumVal r2) <- eval e2 env 
--   return $ NumVal (r1 - r2)
-- eval (Var i) env = return $ find env i 
-- eval (If g e1 e2) env = eval g env >>= \r ->
--   case r of (BoolVal True)  -> eval e1 env 
--             (BoolVal False) -> eval e2 env

-- eval (Equals e1 e2) env = BoolVal <$> ((==) <$> eval e1 env <*> eval e2 env)
-- -- eval (Let d e) env = elab env d >>= \env' -> eval e env' 
-- eval (Let d e) env = elab env d >>= eval e
-- eval (Lam ids e) env = return $ Closure ids e env 
-- eval (Apply f xs) env = do 
--   f' <- eval f env 
--   xs' <- mapM (flip eval env) xs
--   apply f' xs'

-- eval (New) env = do
--   mem <- get
--   let ret = length mem
--   set $ mem ++ [Null]
--   return $ MemAddr ret


-- eval (Deref e) env = do 
--   ~(MemAddr i) <- eval e env
--   mem <- get
--   return $ mem !! i 
                  
-- eval (Seq e1 e2)    env = eval e1 env >> eval e2 env 

-- eval (Assign e1 e2) env = do  
--   ~(MemAddr i) <- eval e1 env
--   e2' <- eval e2 env
--   mem <- get
--   let mem' = take i mem ++ [e2'] ++ drop i mem
--   set mem'
--   return Null
                             

-- apply :: Value -> [Value] -> M Value
-- apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
-- apply _ _ = error "using a value as if it's a function"


-- find :: Env -> Ident -> Value
-- find env i = snd $ head $ filter (\(i', _) -> i' == i) env 

-- elab env (Val i e) = eval e env >>= \r -> return $ (i, r) : env
-- elab env (Rec i l@(Lam args e)) = return env' 
--   where env' = (i, Closure args e env') : env
-- elab _ _ = error "only lambdas can be recursive "



-- env = []

-- main :: IO ()
-- main = do 
--     putStrLn "Hello Haskell Test!"
--     -- let e = "let val x = True in if x then 1 else 0"
--     -- let e = "!a b -> a + b"
--     let e = "let rec fib = !n -> if n == 0 then 0 else if n == 1 then 1 else %{fib:n-1} + %{fib:n-2} in %{fib:1}"
--     print $ parseFun e 


{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

module Main (main) where

import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Data.Text as T 

type Ident = String

newtype Id a = Id a deriving (Show)

instance Functor Id where 
  fmap f (Id x) = Id $ f x 

instance Applicative Id where
  (Id f) <*> (Id x) = Id $ f x 
  pure = Id 

instance Monad Id where
  (>>=) :: Id a -> (a -> Id b) -> Id b
  (Id x) >>= f = f x

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
  (St xm) >>= f = St $ \m -> let (x, m') = xm m in 
                             let (St g) = f x in 
                          g m' 

get :: State m m 
get = St $ \m -> (m, m)

set :: a -> State a ()
set x = St $ \m -> ((), x)

type Mem = [Value]

type M a = State Mem a

data Expr = Number Int
          | Boolean Bool
          | Plus Expr Expr
          | Minus Expr Expr
          | Var Ident
          | If Expr Expr Expr
          | Equals Expr Expr
          | Let  Defn Expr 
          | Lam [Ident] Expr 
          | Apply Expr [Expr]
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

parseFun t = parseOnly parseExpr (T.pack t)

parseExpr = parsePM <|> parseExpr'

parseExpr' :: Parser Expr 
parseExpr' = parseConst
          <|> parseIf
          <|> parseLet
          <|> parseLam
          <|> parseApp 
          <|> parseVal

ss = skipSpace

atom = T.unpack <$> takeWhile1 (\c -> c /=' ' && c /= '"' && c /= '-' && c /= ':' )

reserved :: [String]
reserved = ["let", "in", "rec", "val", "if", "then", "else", "True", "False"]

parseVal :: Parser Expr
parseVal = do
  a <- atom
  if a `elem` reserved
    then fail $ "reserved word: " ++ a
    else return (Var a)

parseConst :: Parser Expr
parseConst = Number <$> decimal 
          <|> "True"  *> pure (Boolean True)
          <|> "False" *> pure (Boolean False)

parseIf :: Parser Expr
parseIf = do 
  "if" *> ss 
  cond <- parseExpr <* ss 
  "then" *> ss 
  e1 <- parseExpr <* ss
  "else" *> ss
  e2 <- parseExpr <* ss
  return (If cond e1 e2)

parseLet = do 
  "let" *> ss
  d <- parseDefn <* ss 
  "in" *> ss 
  e <- parseExpr 
  return $ Let d e 

parseDefn = Val <$> ("val" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr
        <|> Rec <$> ("rec" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr

parseLam = do 
  char '!'
  atoms <- many1 (atom <* ss)
  "->" *> ss 
  exr <- parseExpr
  return $ Lam atoms exr

parsePM = parsePM' <|> parseTerm

parsePM' = do 
  t <- parseTerm <* ss
  con <- (char '+' *> pure Plus
      <|> char '-' *> pure Minus)
  ss 
  e <- parseExpr
  return $ con t e 

parseTerm = parseTerm' <|> parseFactor

parseTerm' = do 
  f <- parseFactor <* ss 
  "==" *> ss
  t <- parseTerm
  return $ Equals f t

parseFactor = char '(' *> parseExpr <* char ')'
          <|> parseExpr'

parseApp = do 
  f <- "%{" *> parseExpr
  char ':' *> ss 
  ins <- many (parseExpr <* ss) 
  return $ Apply f ins

type Env = [(Ident, Value)]

eval :: Expr -> Env -> M Value
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
  let mem' = take i mem ++ [e2'] ++ drop (i + 1) mem
  set mem'
  return Null
                             
apply :: Value -> [Value] -> M Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = error "using a value as if it's a function"

find :: Env -> Ident -> Value
find env i = snd $ head $ filter (\(i', _) -> i' == i) env 

elab env (Val i e) = eval e env >>= \r -> return $ (i, r) : env
elab env (Rec i l@(Lam args e)) = return env' 
  where env' = (i, Closure args e env') : env
elab _ _ = error "only lambdas can be recursive "

env = []

main :: IO ()
main = do 
    putStrLn "Hello Haskell Test!"
    let e = "let rec fib = !n -> if n == 0 then 0 else if n == 1 then 1 else %{fib:n-1} + %{fib:n-2} in %{fib:5}"
    print $ parseFun e 
