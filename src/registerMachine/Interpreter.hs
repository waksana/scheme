module Interpreter (
    step,
    initState,
    value,
    debug
)
where

import Parser (parseInstruction, Instruction (..), Expression (..))
import Data.Map (Map, mapWithKey, insert)
import qualified Data.Map as Map

getLabelMap [] labelMap = labelMap
getLabelMap (Label labelName : xs) labelMap = getLabelMap xs $ insert labelName xs labelMap
getLabelMap (_:xs) labelMap = getLabelMap xs labelMap

data State = State {
    pointer :: [Instruction],
    stack :: [Expression],
    registers :: Map String Expression,
    labelMap :: Map String [Instruction]
} deriving (Eq)

instance Show State where
    show a = "Instructions:"
             ++ concatMap (("\n  " ++) . show) (take 10 (pointer a))
             ++ "\nRegisters:"
             ++ concat(mapWithKey (\ k v -> "\n  " ++ k ++ ": " ++ show v) (registers a))
             ++ "\nStack:"
             ++ concatMap (("\n  " ++) . show) (stack a)

fromJust :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

getDest (ExpSymbol label) labelMap = fromJust $ Map.lookup label labelMap
getDest exp _ = error $ show exp ++ "is not a Destination"

valueOfExpression :: Expression -> Map String Expression-> Expression
valueOfExpression exp registers = case exp of
    ExpList _ -> exp
    ExpSymbol _ -> exp
    ExpNumber _ -> exp
    ExpBool _ -> exp
    IsList expression -> case valueOfExpression expression registers of
        ExpList _ -> ExpBool True
        _ -> ExpBool False
    IsSymbol expression -> case valueOfExpression expression registers of
        ExpSymbol _ -> ExpBool True
        _ -> ExpBool False
    Car expression -> getHead $ valueOfExpression expression registers
    Cdr expression -> getTail $ valueOfExpression expression registers
    Cons left right -> ExpList (valueOfExpression left registers :getLs (valueOfExpression right registers))
    Fetch register -> fromJust (Map.lookup register registers)
    Eq left right -> ExpBool (valueOfExpression left registers == valueOfExpression right registers)
    Add left right -> cal registers (+) left right
    Sub left right -> cal registers (-) left right
    Mul left right -> cal registers (*) left right
    Div left right -> cal registers div left right
    Mod left right -> cal registers mod left right
    where
        getHead (ExpList (x:_)) = x
        getHead _ = error "can not get header"

        getTail (ExpList (_:xs)) = ExpList xs
        getTail _ = error "can not get header"

        getLs (ExpList xs) = xs
        getLs _ = error "not a list"

        cal registers fn left right = expressionCal fn (valueOfExpression left registers) (valueOfExpression right registers)
            where
                expressionCal fn (ExpNumber left) (ExpNumber right) = ExpNumber $ fn left right
                expressionCal fn left right = error $ "can not calcuate " ++ show left ++ " and " ++ show right

step :: State -> State
step State { pointer=[], stack=stack, registers=registers, labelMap=labelMap } = State { pointer=[], stack=stack, registers=registers, labelMap=labelMap }
step State { pointer=((Label label):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = stack, registers=registers, labelMap=labelMap }
step State { pointer=((Assign register exp):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = stack, registers=newRegisters, labelMap=labelMap }
    where
        newRegisters = insert register (valueOfExpression exp registers) registers
step State { pointer=((Branch condition destination):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=next, stack = stack, registers=registers, labelMap=labelMap }
    where
        next = if valueOfExpression condition registers == ExpBool True
               then getDest (valueOfExpression destination registers) labelMap
               else xs
step State { pointer=((Save register):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = newStack, registers=registers, labelMap=labelMap }
    where
        newStack = fromJust (Map.lookup register registers):stack
step State { pointer=((Restore register):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = newStack, registers=newRegisters, labelMap=labelMap }
    where
        newStack = tail stack
        newRegisters = insert register (head stack) registers
step State { pointer=((Goto destination):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=next, stack = stack, registers=registers, labelMap=labelMap }
    where
        next = getDest (valueOfExpression destination registers) labelMap

initState instructions registers stack =
  State
    { pointer = instructions,
      registers = registers,
      stack = stack,
      labelMap = getLabelMap instructions Map.empty
    }

debug state = do
    print state
    let next = step state
    getLine
    debug next

value State { pointer=[], stack=stack, registers=registers, labelMap=labelMap } = State { pointer=[], stack=stack, registers=registers, labelMap=labelMap }
value state = (value . step) state
