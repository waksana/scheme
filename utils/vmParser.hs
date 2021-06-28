import Text.Parsec

data Expression = Number Integer 
                | Bool Bool 
                | Fetch String 
                | Eq Expression Expression
                | Sub Expression Expression
                | Add Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Mod Expression Expression

data Instruction = Label String
                 | Assign String Expression
                 | Branch Expression String
                 | Save String
                 | Restore String
                 | Goto String

label :: Parsec String () Instruction
label = Label <$> many1 letter 

operator :: Parsec String () Char
operator = oneOf "+-*/%="

numExpression:: Parsec String () Expression
numExpression = Number . read <$> many1 digit

matchWord :: String -> Parsec String () String
matchWord [] = return []
matchWord (x:xs) = do
    rx <- char x
    rxs <- matchWord xs
    return (rx:rxs)

parenthesis:: Parsec String () a -> Parsec String () a
parenthesis p = char '(' *> p <* char ')'