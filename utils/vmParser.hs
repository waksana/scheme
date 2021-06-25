import Text.Parsec

data Expression = Number Integer 
                | Bool Bool 
                | Fetch String 
                | Eq Expression Expression
                | Sub Expression Expression
                | Add Expression Expression
                | Mul Expression Expression
                | Div Expression Expression

data Instruction = Label String
                 | Assign String Expression
                 | Branch Expression String
                 | Save String
                 | Restore String
                 | Goto String

label = many1 letter <$> Label