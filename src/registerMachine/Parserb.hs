module Parserb (
    instructions,
    Instruction (..),
    Expression (..)
)
where

import Text.Parsec

data Expression = Symbol String
                | Number Integer
                | Bool Bool
                | Ls [Expression]
                | Fetch String
                | IsList Expression
                | IsSymbol Expression
                | Eq Expression Expression
                | Sub Expression Expression
                | Add Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                | Cons Expression Expression
                | Car Expression
                | Cdr Expression
                deriving (Show, Eq)

data Instruction = Label String
                 | Assign String Expression
                 | Branch Expression Expression
                 | Save String
                 | Restore String
                 | Goto Expression
                 deriving (Show, Eq)

symbol :: Parsec String () Char
symbol = noneOf " \t\r\n()"

parenthesis:: Parsec String () a -> Parsec String () a
parenthesis p = char '(' *> spaces *> p <* spaces <* char ')'

symbolExpression :: Parsec String () Expression
symbolExpression = Symbol <$> many1 symbol

numberExpression :: Parsec String () Expression
numberExpression = Number . read <$> many1 digit

boolExpression :: Parsec String () Expression
boolExpression = Bool . read <$> (string "True" <|> string "False")

nullExpression :: Parsec String () Expression
nullExpression = string "null" >> return (Ls [])

fetchExpression :: Parsec String () Expression
fetchExpression = Fetch <$> (string "fetch" >> many1 space *> many1 symbol)

eqOperator :: Parsec String () (Expression -> Expression -> Expression)
eqOperator = char '=' >> return Eq

addOperator :: Parsec String () (Expression -> Expression -> Expression)
addOperator = char '+' >> return Add

subOperator :: Parsec String () (Expression -> Expression -> Expression)
subOperator = char '-' >> return Sub

mulOperator :: Parsec String () (Expression -> Expression -> Expression)
mulOperator = char '*' >> return Mul

divOperator :: Parsec String () (Expression -> Expression -> Expression)
divOperator = char '/' >> return Div

modOperator :: Parsec String () (Expression -> Expression -> Expression)
modOperator = char '%' >> return Mod

consOperator :: Parsec String () (Expression -> Expression -> Expression)
consOperator = string "cons" >> return Cons

carOperator :: Parsec String () (Expression -> Expression)
carOperator = string "car" >> return Car

cdrOperator :: Parsec String () (Expression -> Expression)
cdrOperator = string "cdr" >> return Cdr

isListOperator :: Parsec String () (Expression -> Expression)
isListOperator = string "list?" >> return IsList

isSymbolOperator:: Parsec String () (Expression -> Expression)
isSymbolOperator = string "symbol?" >> return IsSymbol


operator = eqOperator <|> addOperator <|> subOperator <|> mulOperator <|> divOperator <|> modOperator <|> try consOperator
singleParameterOperator = try carOperator <|> try cdrOperator <|> try isListOperator <|> try isSymbolOperator

operatorExpression :: Parsec String () Expression
operatorExpression = do
    op <- operator
    many1 space
    left <- expression
    many1 space
    op left <$> expression

singleParameterOperatorExpression = do
    op <- singleParameterOperator
    many1 space
    op <$> expression

expression :: Parsec String () Expression
expression = numberExpression
         <|> boolExpression
         <|> nullExpression
         <|> symbolExpression
         <|> parenthesis (fetchExpression <|> operatorExpression <|> singleParameterOperatorExpression)

labelInstruction :: Parsec String () Instruction
labelInstruction = Label <$> many1 symbol

assignInstruction = do
    string "assign"
    many1 space
    reg <- many1 symbol
    many1 space
    Assign reg <$> expression

branchInstruction = do
    string "branch"
    many1 space
    exp <- expression
    many1 space
    Branch exp <$> (parenthesis fetchExpression <|> symbolExpression)

stackInstruction = do
    op <- (string "save" >> return Save) <|> (string "restore" >> return Restore)
    many1 space
    op <$> many1 symbol

gotoInstruction = do
    string "goto"
    many1 space
    Goto <$> (parenthesis fetchExpression <|> symbolExpression)

instruction = parenthesis (assignInstruction <|> branchInstruction <|> stackInstruction <|> gotoInstruction) <|> labelInstruction

instructions = parenthesis (instruction `sepEndBy` many1 space)
