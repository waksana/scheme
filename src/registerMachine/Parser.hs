module Parser (
    parseInstructions,
    Expression(..),
    Instruction(..)
)
where

import Tokenizer

data Expression = Concrete SExp
                | Fetch String
                | IsList Expression
                | IsSymbol Expression
                | IsNumber Expression
                | IsBool Expression
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

parseInstructions (List sexp) = map parseInstruction sexp

parseInstruction :: SExp -> Instruction
parseInstruction (Symbol str) = Label str
parseInstruction (List [Symbol "assign", Symbol register, exp]) = Assign register $ parseExpression exp
parseInstruction (List [Symbol "branch", cond, exp]) = Branch (parseExpression cond) (parseExpression exp)
parseInstruction (List [Symbol "save", Symbol register]) = Save register
parseInstruction (List [Symbol "restore", Symbol register]) = Restore register
parseInstruction (List [Symbol "goto", exp]) = Goto $ parseExpression exp

parseExpression :: SExp -> Expression
parseExpression (List ((Symbol op):xs)) = parseExpList op xs
parseExpression (List _) = error "not a expression"
parseExpression concreteExp = Concrete concreteExp

unaryOperator pattern [exp] = pattern (parseExpression exp)
binaryOperator pattern [a, b] = pattern (parseExpression a) (parseExpression b)

parseExpList "fetch" = \[Symbol register] -> Fetch register
parseExpList "list?" = unaryOperator IsList
parseExpList "symbol?" = unaryOperator IsSymbol
parseExpList "number?" = unaryOperator IsNumber
parseExpList "boolean?" = unaryOperator IsBool
parseExpList "=" = binaryOperator Eq
parseExpList "-" = binaryOperator Sub
parseExpList "+" = binaryOperator Add
parseExpList "*" = binaryOperator Mul
parseExpList "/" = binaryOperator Div
parseExpList "%" = binaryOperator Mod
parseExpList "cons" = binaryOperator Cons
parseExpList "car" = unaryOperator Car
parseExpList "cdr" = unaryOperator Cdr
parseExpList "quote" = \[concreteExp] -> Concrete concreteExp
