import System.Environment
import Text.Parsec
import Parser
import Interpreter
import qualified Data.Map as Map
import Tokenizer (tokenizer)

main = do
    args <- getArgs
    res <- execute args
    putStr res

execute ["run", filename, loadername] = show <$> runFile filename loadername
execute ["run", "show", filename, loadername] = show <$> runDebugFile filename loadername
execute ["debug", filename, loadername] = show <$> debugFile filename loadername
execute ["tokenize", filename] = show <$> tokenizeFile filename
execute ["parse", filename] = show <$> parseFile filename


fromRight (Left err) = error $ show err
fromRight (Right b) = b

getInstructions :: String -> [Instruction]
getInstructions = parseInstructions . fromRight . tokenizer

initVM = initState . getInstructions

initVMWithContext code = initStateWithContext (getInstructions code) . value . initState . getInstructions

debugFile :: FilePath -> FilePath -> IO String
debugFile filename loaderName = do
    code <- readFile filename
    loader <- readFile loaderName
    debug $ initVMWithContext code loader

runDebugFile filename loaderName = do
    code <- readFile filename
    loader <- readFile loaderName
    runDebug $ initVMWithContext code loader

runFile filename loaderName = do
    code <- readFile filename
    loader <- readFile loaderName
    return (value $ initVMWithContext code loader)

tokenizeFile filename = do
    code <- readFile filename
    return (fromRight $ tokenizer code)

parseFile filename = do
    code <- readFile filename
    return (getInstructions code)