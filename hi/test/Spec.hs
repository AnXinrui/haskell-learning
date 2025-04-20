import Eval
import Control.Monad.State
import Expr 

main :: IO ()
main = do
  putStrLn "=================TEST================"
  calcTest

-- stateTest = do
--   let (a, b) = runState (eval (Number 3) []) [] 
--   print a 
--   print b

-- ((3 + 5) * (10 - 2)) / (4 + 2)
ex :: Expr
ex = 
  TermExpr (Div
    (Mult
      (FactorTerm (Bracket (Add (Number 3) (Number 5))))
      (FactorTerm (Bracket (Sub (Number 10) (Number 2)))))
    (FactorTerm (Bracket (Add (Number 4) (Number 2)))))


calcTest :: IO()
calcTest = do   
  let (r, m) = runState (eval ex []) []
  print r 
  print m 