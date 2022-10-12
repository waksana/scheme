module Tokenizer (
    tokenizer,
    SExp (..)
)
where

import Text.Parsec
import Text.Read (readMaybe)
import Data.Char

data SExp = Symbol String
          | Number Integer
          | Bool Bool
          | List [SExp]
          deriving (Show, Eq)

symbolChar :: Parsec String () Char
symbolChar = noneOf " \t\r\n()"

symbol :: Parsec String () SExp
symbol = Symbol <$> many1 symbolChar

parenthesis:: Parsec String () a -> Parsec String () a
parenthesis p = char '(' *> spaces *> p <* spaces <* char ')'

list = List <$> parenthesis (stoken `sepEndBy` spaces)

stoken = symbol <|> list

tokenToExp :: SExp -> SExp
tokenToExp (Symbol str)
  | str == "#t" = Bool True   
  | str == "#f" = Bool False
  | otherwise = readExp str
  where 
    readExp str = case readMaybe str of
      Nothing -> Symbol str
      Just num -> Number num
tokenToExp (List ts) = List $ map tokenToExp ts

tokenizer = fmap tokenToExp . parse stoken ""
