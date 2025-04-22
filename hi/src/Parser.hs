{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

module Parser (
  parseFun 
) where

import Expr
import Data.Functor
import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text hiding (take)

parseFun :: String -> Either String Expr
parseFun t = parseOnly parseExpr (T.pack t) 

parseExpr :: Parser Expr
parseExpr = parseComp <|> parseConst



parseComp :: Parser Expr
parseComp = parseTerm `chainl1` addOp
  where
    addOp = ss *> (char '+' $> Add)
        <|> ss *> (char '-' $> Sub)

parseTerm :: Parser Expr
parseTerm = parseFactor `chainl1` mulOp
  where
    mulOp = ss *> (char '*' $> Mult)
        <|> ss *> (char '/' $> Div)

parseFactor :: Parser Expr
parseFactor = ss *> char '(' *> parseComp <* char ')'
          <|> ss *> parseConst

parseConst :: Parser Expr
parseConst = Number <$> decimal 
          <|> ("True"  $> Boolean True)
          <|> ("False" $> Boolean False)

ss :: Parser ()
ss = skipSpace

chainl1 :: (Alternative m, Monad m) => m b -> m (b -> b -> b) -> m b
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x
