module Interpreter (
    eval,
    step,
    initState
)
where

import qualified Data.Map as Map
import Parser

getLabelMap instructions labelMap = case instructions of
    [] -> labelMap
    ((Label labelName):xs) -> getLabelMap xs $ Map.insert labelName instructions labelMap
    (_:xs) -> getLabelMap xs labelMap

data State = State {
    pointer :: [Instruction],
    stack :: [Expression],
    registers :: Map.Map String Expression ,
    labelMap :: Map.Map String [Instruction]
}

instance Show State where
    show a = "Instructions:"
             ++ concatMap (("\n  " ++) . show) (pointer a)
             ++ "\nRegisters:"
             ++ concat(Map.mapWithKey (\ k v -> "\n  " ++ k ++ ": " ++ show v) (registers a))
             ++ "\nStack:"
             ++ concatMap (("\n  " ++) . show) (stack a)

fromJust :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

cal registers fn left right = expressionCal fn (valueOfExpression left registers) (valueOfExpression right registers)
    where
        expressionCal fn (Number left) (Number right) = Number $ fn left right
        expressionCal fn left right = error $ "can not calcuate " ++ show left ++ " and " ++ show right

valueOfExpression :: Expression -> Map.Map String Expression-> Expression
valueOfExpression exp registers = case exp of
    Dest _ -> exp
    Number _ -> exp
    Bool _ -> exp
    Fetch register -> fromJust (Map.lookup register registers)
    Eq left right -> Bool (valueOfExpression left registers == valueOfExpression right registers)
    Add left right -> cal registers (+) left right
    Sub left right -> cal registers (-) left right
    Mul left right -> cal registers (*) left right
    Div left right -> cal registers div left right
    Mod left right -> cal registers mod left right

getDest (Dest label) labelMap = fromJust $ Map.lookup label labelMap
getDest exp _ = error $ show exp ++ "is not a Destination"

step :: State -> State
step State { pointer=[], stack=stack, registers=registers, labelMap=labelMap } = State { pointer=[], stack=stack, registers=registers, labelMap=labelMap }
step State { pointer=((Label label):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = stack, registers=registers, labelMap=labelMap }
step State { pointer=((Assign register exp):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = stack, registers=newRegisters, labelMap=labelMap }
    where
        newRegisters = Map.insert register (valueOfExpression exp registers) registers
step State { pointer=((Branch condition destination):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=next, stack = stack, registers=registers, labelMap=labelMap }
    where
        next = if valueOfExpression condition registers == Bool True
               then getDest (valueOfExpression destination registers) labelMap
               else xs
step State { pointer=((Save register):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = newStack, registers=registers, labelMap=labelMap }
    where
        newStack = fromJust (Map.lookup register registers):stack
step State { pointer=((Restore register):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=xs, stack = newStack, registers=newRegisters, labelMap=labelMap }
    where
        newStack = tail stack
        newRegisters = Map.insert register (head stack) registers
step State { pointer=((Goto destination):xs), stack=stack, registers=registers, labelMap=labelMap } = State { pointer=next, stack = stack, registers=registers, labelMap=labelMap }
    where
        next = getDest (valueOfExpression destination registers) labelMap

value State { pointer=[], stack=stack, registers=registers, labelMap=labelMap } = State { pointer=[], stack=stack, registers=registers, labelMap=labelMap }
value state = (value . step) state

initState instructions registers =
  State
    { pointer = instructions,
      registers = registers,
      stack = [],
      labelMap = getLabelMap instructions Map.empty
    }

eval instructions = step