module Interpreter (
    step,
    initState,
    value,
    debug
)
where

import Tokenizer (SExp (..))
import Parser (Instruction (..), Expression (..))
import Data.Map (Map, mapWithKey, insert)
import qualified Data.Map as Map

isList (List _) = True
isList _ = False

valueOfExpression :: Expression -> Map String SExp -> SExp
valueOfExpression exp registers = case exp of
    Concrete sexp -> sexp
    IsList expression -> Bool $ isList $ valueOfExpression expression registers
    IsSymbol expression -> Bool $ case valueOfExpression expression registers of
        Symbol _ -> True
        _ -> False
    IsBool expression -> Bool $ case valueOfExpression expression registers of
        Bool _ -> True
        _ -> False
    IsNumber expression -> Bool $ case valueOfExpression expression registers of
        Number _ -> True
        _ -> False
    Car expression -> getHead $ valueOfExpression expression registers
    Cdr expression -> getTail $ valueOfExpression expression registers
    Cons left right -> List (valueOfExpression left registers : getLs (valueOfExpression right registers))
    Fetch register -> fromJust register (Map.lookup register registers)
    Eq left right -> Bool (valueOfExpression left registers == valueOfExpression right registers)
    Add left right -> cal registers (+) left right
    Sub left right -> cal registers (-) left right
    Mul left right -> cal registers (*) left right
    Div left right -> cal registers div left right
    Mod left right -> cal registers mod left right
    where
        getHead (List (x:_)) = x
        getHead x = error $ "can not get header of " ++ show x

        getTail (List (_:xs)) = List xs
        getTail x = error $ "can not get tail of " ++ show x

        getLs (List xs) = xs
        getLs x = error $ show x ++ " is not a list"

        cal registers fn left right = expressionCal fn (valueOfExpression left registers) (valueOfExpression right registers)
            where
                expressionCal fn (Number left) (Number right) = Number $ fn left right
                expressionCal fn left right = error $ "can not calcuate " ++ show left ++ " and " ++ show right

getLabelMap [] labelMap = labelMap
getLabelMap (Label labelName : xs) labelMap = getLabelMap xs $ insert labelName (Label labelName : xs) labelMap
getLabelMap (_:xs) labelMap = getLabelMap xs labelMap

data State = State {
    pointer :: [Instruction],
    stack :: [SExp],
    registers :: Map String SExp,
    labelMap :: Map String [Instruction]
} deriving (Eq)

instance Show State where
    show a = "Instructions:"
             ++ concatMap (("\n  " ++) . show) (take 10 (pointer a))
             ++ "\nRegisters:"
             ++ concat(mapWithKey (\ k v -> "\n  " ++ k ++ ": " ++ show v) (registers a))
             ++ "\nStack:"
             ++ concatMap (("\n  " ++) . show) (stack a)

fromJust :: String -> Maybe a -> a
fromJust name Nothing  = error $ "can't find " ++ name ++ " in Map"
fromJust _ (Just x) = x

getDest (Symbol label) labelMap = fromJust label $ Map.lookup label labelMap
getDest exp _ = error $ show exp ++ "is not a Destination"

step :: State -> State
step State { pointer=[], stack=stack, registers=registers, labelMap=labelMap } = State { pointer=[], stack=stack, registers=registers, labelMap=labelMap }
step State { pointer=((Label label):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = stack, registers=registers, labelMap=labelMap }
step State { pointer=((Assign register exp):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = stack, registers=newRegisters, labelMap=labelMap }
    where
        newRegisters = insert register (valueOfExpression exp registers) registers
step State { pointer=((Branch condition destination):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=next, stack = stack, registers=registers, labelMap=labelMap }
    where
        next = if valueOfExpression condition registers == Bool True
               then getDest (valueOfExpression destination registers) labelMap
               else xs
step State { pointer=((Save register):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = newStack, registers=registers, labelMap=labelMap }
    where
        newStack = fromJust register (Map.lookup register registers):stack
step State { pointer=((Restore register):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = newStack, registers=newRegisters, labelMap=labelMap }
    where
        newStack = tail stack
        newRegisters = insert register (head stack) registers
step State { pointer=((Goto destination):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=next, stack = stack, registers=registers, labelMap=labelMap }
    where
        next = getDest (valueOfExpression destination registers) labelMap

initState instructions =
  State
    { pointer = instructions,
      registers = Map.empty,
      stack = [],
      labelMap = getLabelMap instructions Map.empty
    }

debug state = do
    print state
    let next = step state
    getLine
    debug next

value State { pointer=[], stack=stack, registers=registers, labelMap=labelMap } = State { pointer=[], stack=stack, registers=registers, labelMap=labelMap }
value state = (value . step) state
