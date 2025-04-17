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

get :: State m m 
get = St $ \m -> (m, m)

set :: a -> State a ()
set x = St $ \m -> ((), x)       
-- 定义内存类型（使用整数作为内存值）
type Mem = [Int]

-- 定义我们的 State 操作
getMem :: State Mem Mem
getMem = get

setMem :: Mem -> State Mem ()
setMem = set

-- 创建一个简单的内存更新操作
newMem :: State Mem Int
newMem = do
  mem <- getMem
  let newMemAddr = length mem  -- 新的内存地址是当前内存的长度
  setMem (mem ++ [0])           -- 更新内存，在末尾添加一个值（0）
  return newMemAddr             -- 返回新的内存地址

-- 定义一个简单的 Deref 操作
deref :: Int -> State Mem Int
deref addr = do
  mem <- getMem
  return (mem !! addr)  -- 从内存中获取指定地址的值

-- 测试代码：创建新的内存，进行 Deref 操作
main :: IO ()
main = do
  let (newAddr, _) = runState newMem []  -- 从空内存开始创建新的内存
  putStrLn $ "New memory address: " ++ show newAddr
  let (value, _) = runState (deref newAddr) [100, 200, 300]  -- 获取新地址的值
  putStrLn $ "Value at new address: " ++ show value