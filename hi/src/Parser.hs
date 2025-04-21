{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

module Parser where

import Expr
import Data.Functor
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.Internal.Types
import Data.Attoparsec.ByteString.Char8 (skipSpace, Parser, decimal, parseOnly)

parseFun :: String -> Either String Expr
parseFun t = parseOnly parseExpr (B.pack t) 

parseExpr :: Data.Attoparsec.Internal.Types.Parser B.ByteString Expr
parseExpr = parseConst
parseConst :: Data.Attoparsec.Internal.Types.Parser B.ByteString Expr
parseConst = Number <$> decimal 
          <|> ("True"  $> Boolean True)
          <|> ("False" $> Boolean False)

ss :: Data.Attoparsec.ByteString.Char8.Parser ()
ss = skipSpace