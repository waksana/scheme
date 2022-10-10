module Debug 
where

import Text.Parsec
import Parser
import Interpreter
import qualified Data.Map as Map
import Tokenizer (tokenizer)

fromRight (Left err) = error $ show err
fromRight (Right b) = b

initVM code = initState $ parseInstructions $ fromRight $ tokenizer code

debugFile filename = do
    code <- readFile filename
    debug $ initVM code

runFile filename = do
    code <- readFile filename
    return (value $ initVM code)

tokenizeFile filename = do
    code <- readFile filename
    return (tokenizer code)

parseFile filename = do
    code <- readFile filename
    return (parseInstructions $ fromRight $ tokenizer code)

