module P
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
parseInstruction (List [Symbol "assign", Symbol register, exp]) = Assign register (ExpSymbol "test")

parseExpression :: SExp -> Expression
parseExpression (Symbol str) = ExpSymbol str
