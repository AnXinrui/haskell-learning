module Main (main) where

import Eval
import Expr
import Parser
import System.IO (hFlush, stdout)
import Control.Monad.State (runState)

main :: IO ()
main = do
  putStrLn catArt
  repl []

repl :: Mem -> IO()
repl mem = do 
  putStr "[~]$ " 
  hFlush stdout
  inp <- getLine
  if inp == ":q"
    then do
      putStrLn "bye~" 
      return () 
    else do
      case parseFun inp of 
        Right e -> do
          let (r, m) = runState (eval e []) mem
          print r
          repl m
        _ -> do 
          putStrLn "解析失败"   
          repl mem

catArt :: String
catArt = do
  unlines
    [ " /\\     /\\"
    , "{  `---'  }"
    , "{  O   O  }"
    , "~~>  V  <~~"
    , " \\  \\|/  /"
    , "  `-----'____"
    , " /     \\    \\_"
    , "{       }\\  )_\\_   _"
    , "|  \\_/  |/ /  \\_\\_/ )"
    , " \\__/  /(_/     \\__/"
    , "   (__/"
    ]
  