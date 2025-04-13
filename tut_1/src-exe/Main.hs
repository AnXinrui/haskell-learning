module Main where
import Foreign.C (Errno)

{- hello -}

data Hlist a = Empty | Cons a (Hlist a)

class Show' a where
  show' :: a -> String

instance Show' a => Show' (Hlist a) where
  show' Empty = "Empty"
  show' (Cons x xs) = "Cons " ++ show' x ++ " " ++ show' xs 
instance Show' Int where
  show' = show 

-- show' (Cons 3 (Cons 2 Empty) :: Hlist Int)

htoList :: Hlist a -> [a]
htoList Empty = []
htoList (Cons x xs) = x : htoList xs

data Error = Error deriving Show

safeDevide :: Int -> Int -> Either Error Int 
safeDevide _ 0 = Left Error
safeDevide a b = Right (a `div` b)



main :: IO ()
main = putStrLn "Hello, Haskell!"
