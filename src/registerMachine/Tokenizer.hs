module Tokenizer (
    tokenizer,
    SExp (..)
)
where

import Text.Parsec
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
  | all isDigit str = Number (read str)
  | otherwise = Symbol str
tokenToExp (List ts) = List $ map tokenToExp ts

tokenizer = fmap tokenToExp . parse stoken ""
