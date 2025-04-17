import Expr

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M

data RuntimeError
  = MissingVar String
  | EmptyEnv
  | NeedBool Expr
  | NeedFunc Expr
  | NeedNum Expr
  | MisMatchArgPar
  | UnknownOp String
  | NotInLoop
  | OtherError

type Frame = M.Map String Expr

type Env a =
  WriterT
    [String]
    ( StateT
        [Frame]
        (Except RuntimeError)
    )
    a

run :: Expr -> Env Expr 
run(Var n) = lookupVar n 



lookupVar :: String -> Env Expr
lookupVar n = do
  env <- get
  find env
 where
  find :: [Frame] -> Env Expr
  find [] = throwError (MissingVar n)
  find (f : fs) =
    let r = M.lookup n f
     in case r of
          Nothing -> find fs
          Just e -> run e