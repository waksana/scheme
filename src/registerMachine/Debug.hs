module Debug 
where

import Text.Parsec
import Parser
import Interpreter
import qualified Data.Map as Map
import Tokenizer (tokenizer)

fromRight (Left err) = error $ show err
fromRight (Right b) = b

getInstructions :: String -> [Instruction]
getInstructions = parseInstructions . fromRight . tokenizer

initVM = initState . getInstructions

initVMWithContext code = initStateWithContext (getInstructions code) . value . initState . getInstructions

debugFile filename loaderName = do
    code <- readFile filename
    loader <- readFile loaderName
    debug $ initVMWithContext code loader

runFile filename loaderName = do
    code <- readFile filename
    loader <- readFile loaderName
    return (value $ initVMWithContext code loader)

tokenizeFile filename = do
    code <- readFile filename
    return (tokenizer code)

parseFile filename = do
    code <- readFile filename
    return (getInstructions code)

