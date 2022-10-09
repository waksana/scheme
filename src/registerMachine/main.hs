import Text.Parsec
import Parser
import Interpreter
import qualified Data.Map as Map

fromRight (Left err) = error $ show err
fromRight (Right b) = b

parseVML code = initState $ fromRight $ parse instructions "" code

debugFile filename registers = do
    code <- readFile filename
    debug $ parseVML code registers []

runFile filename registers = do
    code <- readFile filename
    return (run $ parseVML code registers [])