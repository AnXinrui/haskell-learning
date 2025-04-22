import Eval
-- -- import Expr 
-- import qualified Data.Text as T
import Parser
import Control.Monad.State
-- import Data.Attoparsec.Text hiding (take)

main :: IO ()
main = do
  
  -- putStrLn "=================CALCTEST================"
  -- -- calcTest
  -- putStrLn "==================VARTEST================"
  -- varTest
  -- putStrLn "==================LETTEST================"
  -- letTest1
  -- putStrLn "==================LETTEST================"
  -- letTest2 
  -- putStrLn "==================PARTEST================"
  -- letParserTest
  calcParserTest




letParserTest :: IO()
letParserTest = do 
  -- let s1 = "let val x = 665 in x + 1"
  -- -- let s = "x + 1"
  -- print $ parseFun s1
  -- case parseFun s1 of
  --   Right e -> print $ runState (eval e []) []  -- 打印 (结果, 状态)
  --   _ -> putStrLn "解析失败"  
  -- print $ parseFun "\\s -> s + 1"
  -- print $ parseFun "fib(3)"
  
  let s2 = "let rec fib = !x -> if x < 2 { x } else { %{fib:x-1} + %{fib:x-2} } in %{fib:5}"
  -- let s2 = "let rec fib = 2 in 3"
  -- let s2 = "let rec fib = !x -> if x < 2 { %{fib:3} } else { x } in 3"
  putStrLn s2
  print (parseFun s2)
  case parseFun s2 of 
    Right e -> print $ runState (eval e []) []  -- 打印 (结果, 状态)
    _ -> putStrLn "解析失败"  

  -- let s3 = "let rec fac = !x -> if x < 2 {1} else { %{fac:x-1} * x} in %{fac:1}"


  -- let s2 = "%{fib:3 }"
  -- let s2 = "if n < 2 { x } else { %{fib:x-1} + %{fib:x-2} } in %{fib:3}"


-- ifParserTest :: IO ()
-- ifParserTest = do 
--   let s = "if False { 1 + 1 } else {4 + 3}"
--   print $ parseFun s 
--   case parseFun s of
--     Right e -> print $ runState (eval e []) []  -- 打印 (结果, 状态)
--     _ -> putStrLn "解析失败"





  
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

-- ex1 :: Expr
-- ex1 = Var "x"
-- varTest :: IO ()
-- varTest = do 
--   let (r0, m0) = runState (eval ex1 []) [("x", NumVal 8)]
--   print r0 
--   print m0
--   let (r1, m1) = runState (eval ex1 [("x", NumVal 3)]) []
--   print r1 
--   print m1

-- ex2 :: Expr
-- ex2 = Let (Val "x" (Number 7)) (Add (Var "x") (Number 1))

-- letTest1 :: IO ()
-- letTest1 = do 
--   let (r, m) = runState (eval ex2 []) []
--   print r 
--   print m 

-- factorial :: Expr
-- factorial = Lam ["i"] (If (Lt (Var "i") (Number 2)) (Number 1) (Mult (Var "i") 
--   (Apply (Var "fac") [Sub (Var "i") (Number 1)])))

-- ex3 :: Expr
-- ex3 = Let (Rec "fac" factorial) (Apply (Var "fac") [Number 5])
-- -- ex3 = Let (Rec "fac" (Lam ["i"] (If (Lt (Var "i") (Number 2)) (Number 1) (Mult (Var "i") 
-- --   (Apply (Var "fac") [Number 1]))))) (Apply (Var "fac") [Number 4])

-- letTest2 :: IO()
-- letTest2 = do 
--   let (r, m) = runState (eval ex3 []) []
--   print r 
--   print m 


calcParserTest :: IO ()
calcParserTest = do 
  print $ parseFun "123"
  print $ parseFun "1 + 2"
  print $ parseFun "1 > 2"
  print $ parseFun "1 *  2"
  print $ parseFun "1 < 3 * 4"
  let s = "((1+ 3)  * (1 +3 * 3)) < ((2+ 3)  * (1 +3 * 3))"
  print $ parseFun s
  case parseFun s of
    Right e -> print $ runState (eval e []) []  -- 打印 (结果, 状态)
    _ -> putStrLn "解析失败"
  let s1 = "(1 + 3 * 4)+ (3 + 7)"
  print $ parseFun s1
  case parseFun s1 of
    Right e -> print $ runState (eval e []) []  -- 打印 (结果, 状态)
    _ -> putStrLn "解析失败"  
