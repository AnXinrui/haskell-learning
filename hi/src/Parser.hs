{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

module Parser (
  parseFun 
) where

import Expr
import Data.Functor
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.Internal.Types
import Data.Attoparsec.ByteString.Char8 

parseFun :: String -> Either String Expr
parseFun t = parseOnly parseExpr (B.pack t) 

parseExpr = parseComp <|> parseConst

parseComp = parseTerm `chainl1` addOp
  where
    addOp = ss *> (char '+' *> pure Add)
        <|> ss *> (char '-' *> pure Sub)

parseTerm = parseFactor `chainl1` mulOp
  where
    mulOp = ss *> (char '*' *> pure Mult)
        <|> ss *> (char '/' *> pure Div)

parseFactor = ss *> char '(' *> parseComp <* char ')'
          <|> ss *> parseConst

parseConst :: Data.Attoparsec.Internal.Types.Parser B.ByteString Expr
parseConst = Number <$> decimal 
          <|> ("True"  $> Boolean True)
          <|> ("False" $> Boolean False)

ss :: Data.Attoparsec.ByteString.Char8.Parser ()
ss = skipSpace

chainl1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x
