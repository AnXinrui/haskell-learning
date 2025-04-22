{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

module Parser (
  parseFun,
) where

import Expr
import Data.Functor
import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text hiding (take)

parseFun :: String -> Either String Expr
parseFun t = parseOnly parseExpr (T.pack t) 

parseExpr :: Parser Expr
parseExpr = parseComp 
        <|> parseConst
        <|> parseIf
        <|> parseSeq
        <|> parseSet
        <|> parseLet
        <|> parseLam
        <|> parseApp
        <|> parseVal
       
    
parseSet :: Parser Expr
parseSet = do 
  "set" *> ss
  arg <- atom <* ss
  Set arg <$> parseExpr

parseIf :: Parser Expr 
parseIf = do 
  "if" *> ss 
  cond <- parseExpr <* ss 
  e1 <- parseSeq <* ss
  "else" *> ss
  e2 <- parseSeq <* ss
  return (If cond e1 e2)

parseSeq :: Parser Expr 
parseSeq = do 
  char '{' *> ss 
  exprs <- many' (parseExpr <* ss) 
  char '}' *> ss
  return $ Seq exprs

parseVal :: Parser Expr
parseVal = Var <$> atom

parseLet :: Parser Expr
parseLet = do 
  "let" *> ss 
  d <- parseDefn 
  ss
  "in"  *> ss 
  e <- parseExpr <* ss 
  return $ Let d e 

parseDefn :: Parser Defn
parseDefn = Val <$> ("val" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr
        <|> Rec <$> ("rec" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr

parseLam :: Parser Expr
parseLam = do 
  char '!' *> ss 
  ids <- many1 (atom <* ss)
  "->" *> ss
  Lam ids <$> parseExpr

parseApp :: Parser Expr
parseApp = do 
  f <- "%{" *> parseExpr
  char ':' *> ss 
  ins <- many (parseExpr <* ss) 
  char '}' *> ss
  return $ Apply f ins
-- parseApp = do 
--   func <- parseFactor <* ss
--   char '(' *> ss
--   args <- parseExpr `sepBy` (char ',' *> ss) <* char ')' <* ss
--   return $ Apply func args

parseComp :: Parser Expr
parseComp = parseTerm `chainl1` addOp
  where
    addOp = ss *> (char '+' $> Add)
        <|> ss *> (char '-' $> Sub)

parseTerm :: Parser Expr
parseTerm = parseCompare 
        <|> parseFactor `chainl1` mulOp
              where
                mulOp = ss *> (char '*' $> Mult)
                    <|> ss *> (char '/' $> Div)

parseCompare :: Parser Expr 
parseCompare = do 
  f <- parseFactor <* ss
  op <- choice
    [ "==" $> Equals
    , "<"  $> Lt
    , ">"  $> Gt
    ]
  ss
  op f <$> parseTerm

parseFactor :: Parser Expr
parseFactor = ss *> char '(' *> parseComp <* char ')'
          <|> ss *> parseApp
          <|> ss *> parseConst
          <|> ss *> parseVal

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

keyWords :: [T.Text]
keyWords = ["set", "let", "in", "val", "rec", "if", "then", "else", "True", "False"]

atom :: Parser String
atom = do
  result <- T.unpack <$> takeWhile1 (\c -> c /= ' ' 
                                        && c /= '"' 
                                        && c /= '-' 
                                        && c /= '+'
                                        && c /= ':' 
                                        && c /= '='
                                        && c /= '('
                                        && c /= ')'
                                        && c /= '{'
                                        && c /= '}'
                                        && c /= '!'
                                        && c /= '%')
  -- 如果解析结果在关键词列表中，则失败
  if T.pack result `elem` keyWords
     then fail $ "Keyword " ++ result ++ " is not allowed."
     else return result