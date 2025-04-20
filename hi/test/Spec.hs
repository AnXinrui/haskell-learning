import Eval
import Control.Monad.State
import Expr

main :: IO ()
main = do
  putStrLn "=================TEST================"
  let (a, b) = runState (eval (Number 3) []) [] 
  print a 
  print b

  