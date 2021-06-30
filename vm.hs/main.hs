import Text.Parsec
import Parser
import Interpreter
import qualified Data.Map as Map

fromRight (Left err) = error $ show err
fromRight (Right b) = b

main = do
    code <- readFile "../examples/gcd.vml"
    let inst = fromRight $ parse instructions "" code
    calculate $ initState inst $ Map.fromList [("a", Number 53), ("b", Number 189)]

calculate state = do
    print state
    let next = step state
    getLine 
    calculate next