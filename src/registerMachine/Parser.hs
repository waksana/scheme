module Parser (
    parseInstruction,
    Expression(..),
    Instruction(..)
)
where

import Tokenizer

data Expression = ExpSymbol String
                | ExpNumber Integer
                | ExpBool Bool
                | ExpList [Expression]
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

parseInstruction :: SExp -> Instruction
parseInstruction (Symbol str) = Label str
parseInstruction (List [Symbol "assign", Symbol register, exp]) = Assign register $ parseExpression exp
parseInstruction (List [Symbol "branch", cond, exp]) = Branch (parseExpression cond) (parseExpression exp)
parseInstruction (List [Symbol "save", Symbol register]) = Save register
parseInstruction (List [Symbol "restore", Symbol register]) = Restore register
parseInstruction (List [Symbol "goto", exp]) = Goto $ parseExpression exp

parseExpression :: SExp -> Expression
parseExpression (Symbol str) = ExpSymbol str
parseExpression (Number val) = ExpNumber val
parseExpression (Bool x) = ExpBool x
parseExpression (List ((Symbol op):xs)) = parseExpList op xs

unaryOperator pattern [exp] = pattern (parseExpression exp)
binaryOperator pattern [a, b] = pattern (parseExpression a) (parseExpression b)

parseExpList "fetch" = \[Symbol register] -> Fetch register
parseExpList "list?" = unaryOperator IsList
parseExpList "symbol?" = unaryOperator IsSymbol
parseExpList "=" = binaryOperator Eq
parseExpList "-" = binaryOperator Sub
parseExpList "+" = binaryOperator Add
parseExpList "*" = binaryOperator Mul
parseExpList "/" = binaryOperator Div
parseExpList "%" = binaryOperator Mod
parseExpList "cons" = binaryOperator Cons
parseExpList "car" = unaryOperator Car
parseExpList "cdr" = unaryOperator Cdr