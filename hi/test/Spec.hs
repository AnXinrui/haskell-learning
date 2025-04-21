import Eval
import Expr 
import Parser
import Control.Monad.State

main :: IO ()
main = do
  putStrLn "=================CALCTEST================"
  -- calcTest
  putStrLn "==================VARTEST================"
  varTest
  putStrLn "==================LETTEST================"
  letTest1
  putStrLn "==================LETTEST================"
  letTest2 
  print $ parseFun "123"

-- stateTest = do
--   let (a, b) = runState (eval (Number 3) []) [] 
--   print a 
--   print b

-- ((3 + 5) * (10 - 2)) / (4 + 2)
-- ex0 :: Expr
-- ex0 = 
--   TermExpr (Div
--     (Mult
--       (FactorTerm (Bracket (Add (Number 3) (Number 5))))
--       (FactorTerm (Bracket (Sub (Number 10) (Number 2)))))
--     (FactorTerm (Bracket (Add (Number 4) (Number 2)))))


-- calcTest :: IO()
-- calcTest = do   
--   let (r, m) = runState (eval ex0 []) []
--   print r 
--   print m 

ex1 :: Expr
ex1 = Var "x"
varTest :: IO ()
varTest = do 
  let (r0, m0) = runState (eval ex1 []) [("x", NumVal 8)]
  print r0 
  print m0
  let (r1, m1) = runState (eval ex1 [("x", NumVal 3)]) []
  print r1 
  print m1

ex2 :: Expr
ex2 = Let (Val "x" (Number 7)) (Add (Var "x") (Number 1))

letTest1 :: IO ()
letTest1 = do 
  let (r, m) = runState (eval ex2 []) []
  print r 
  print m 

factorial :: Expr
factorial = Lam ["i"] (If (Lt (Var "i") (Number 2)) (Number 1) (Mult (Var "i") 
  (Apply (Var "fac") [Sub (Var "i") (Number 1)])))

ex3 :: Expr
ex3 = Let (Rec "fac" factorial) (Apply (Var "fac") [Number 5])
-- ex3 = Let (Rec "fac" (Lam ["i"] (If (Lt (Var "i") (Number 2)) (Number 1) (Mult (Var "i") 
--   (Apply (Var "fac") [Number 1]))))) (Apply (Var "fac") [Number 4])

letTest2 :: IO()
letTest2 = do 
  let (r, m) = runState (eval ex3 []) []
  print r 
  print m 