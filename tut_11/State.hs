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

-- 新建一个内存槽 --- State Monad 返回的是最终的状态
-- mem :: [Int] 
newMem :: State Mem Int 
newMem = do 
  mem <- get 
  let addr = length mem 
  set (mem ++ [0])
  return addr 

-- 获取该地址所存储的变量的值
deref :: Int -> State Mem Int 
deref i = do 
  mem <- get 
  return $ mem !! i 

-- 赋值
assign :: Int -> Int -> State Mem Int 
assign i v = do 
  mem <- get 
  let mem' = take i mem ++ [v] ++ drop (i + 1) mem 
  set mem'
  return v 

main :: IO()
main = runTest
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

runTest :: IO()
runTest = do 
  let (r1a, r2a) = runState incrementState 4
  print r1a 
  print r2a

  let (r1b, r2b) = runState doubleincState 4
  print r1b 
  print r2b

  let (r1c, r2c) = runState pureState 4
  print r1c 
  print r2c

  let (r1d, r2d) = runState monadState 4
  print r1d 
  print r2d

  putStrLn "==============State Monad================="

  let (i1, e1) = runState newMem [1..4]
  print i1 
  print e1

  let (i2, e2) = runState (assign i1 99) e1
  print i2 
  print e2 

  let (i3, e3) = runState (deref i1) e2
  print i3 
  print e3  
