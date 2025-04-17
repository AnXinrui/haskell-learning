-- runState :: State m a -> m -> (a, m)
newtype State m a = St { runState :: m -> (a, m) }

instance Functor (State m) where
  fmap :: (a -> b) -> (State m a) -> (State m b)
  fmap f (St x) = St $ \m -> let (x', m') = x m  in 
                             (f x', m')

instance Applicative (State m) where 
  pure :: a -> State m a 
  pure x = St $ \m -> (x, m)
  (<*>) :: State m (a -> b) -> State m a -> State m b 
  (St f) <*> (St x) = St $ \m -> let (f', m' ) = f m  in 
                                 let (x', m'') = x m' in
                                 (f' x', m'')

instance Monad (State m) where
  (>>=) :: State m a -> (a -> State m b) -> State m b
  St x >>= f = St $ \m -> let (x'', m') = x m in 
                          let (St g)    = f x''  in  
                          (g m')

-- 定义内存类型（使用整数作为内存值）
type Mem = [Int]

get :: State Mem Mem 
get = St $ \m -> (m, m)

set :: Mem -> State Mem ()
set x = St $ \m -> ((), x)

-------------------------- TEST ---------------------------

doubleTick :: Int -> State Int Int 
doubleTick n = St $ \m -> (n * 2, m + 1)

monadState :: State Int Int 
monadState = incrementState >>= doubleTick

pureState :: State Int Int
pureState = pure 100

doubleincState :: State Int Int 
doubleincState = fmap (* 2) incrementState

incrementState :: State Int Int
incrementState = St $ \s -> (s + 2, s + 1)

main :: IO()
main = do 
  let (r1, r2) = runState incrementState 4
  print r1 
  print r2
  let (r1, r2) = runState doubleincState 4
  print r1 
  print r2
  let (r1, r2) = runState pureState 4
  print r1 
  print r2
  let (r1, r2) = runState monadState 4
  print r1 
  print r2
