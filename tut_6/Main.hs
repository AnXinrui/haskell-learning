data Failture a = Fail | Ok a deriving (Show)

instance Functor Failture where
  fmap f Fail = Fail
  fmap f (Ok a) = Ok $ f a 

instance Applicative Failture where
  pure :: a -> Failture a
  pure = Ok
  (<*>) :: Failture (a -> b) -> Failture a -> Failture b
  (Ok f) <*> (Ok x) = Ok $ f x
  Fail <*> _ = Fail
  _ <*> Fail = Fail

instance Monad Failture where
  (Ok x) >>= f = f x 
  Fail >>= _ = Fail

data Person = Person Int String Int deriving Show

safeDivide :: Failture Int -> Failture Int -> Failture Int
safeDivide xm ym = xm >>= (\x ->
                   ym >>= (\y -> if y == 0 then Fail
                                 else return (x `div` y)))