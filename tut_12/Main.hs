module Main where

import Control.Applicative
import Data.Char (digitToInt)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser x) = Parser $ \s -> do 
    (x', s') <- x s
    return (f x', s')

instance Applicative Parser where
  pure :: a -> Parser a 
  pure x = Parser $ \s -> Just (x, s)
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
  Parser f <*> Parser x = Parser $ \s -> do 
    (f', s' ) <- f s 
    (a', s'') <- x s'
    return (f' a', s'')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b 
  Parser x >>= f = Parser $ \s -> do 
    (x', s') <- x s 
    runParser (f x') s'

instance MonadFail Parser where
  fail _ = Parser $ \s -> Nothing

instance Alternative Parser where 
  empty = fail ""
  (Parser x) <|> (Parser y) = Parser $ \s ->
    case x s of 
      Just x -> Just x
      Nothing -> y s 

char :: Char -> Parser Char
char c = Parser charP
  where charP []                 = Nothing
        charP (x:xs) | x == c    = Just (c, xs)
                     | otherwise = Nothing
              

string :: String -> Parser String 
string = mapM char 

-- mapM f [] = return []
-- mapM f (x : xs) = do 
--   x'  <- f x 
--   xs' <- mapM f xs 
--   return (x' : xs')

space :: Parser Char 
space = char ' ' <|> char '\n' <|> char '\r'
  <|> char '\t'

spaces = many space

-- parseHW = (,) <$> (string "Hello" <* spaces)  <*> string "World"   -- <*：先执行左边 a，再执行右边 b，保留左边的结果

parseIntChar :: Parser Char
parseIntChar = char '0' <|> char '1' <|> char '2'
           <|> char '3' <|> char '4' <|> char '5'
           <|> char '6' <|> char '7' <|> char '8'
           <|> char '9' 

parseInt :: Parser Int 
parseInt = str2Int 0 <$> some parseIntChar

str2Int :: Int -> String -> Int 
str2Int acc       [] = acc 
str2Int acc (x : xs) = str2Int (acc * 10 + (digitToInt x)) xs






main :: IO() 
main = putStrLn "Hello Haskell!"