-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Backend.Structure where

import qualified Data.Map as Map
import Control.Monad.State

import Grammar.AbsLatte
import Backend.Translator


-- For each variable x we remember whole stack of values of x.
-- Stack size is held as an additional Integer.
data VarStack = VarStack {
    size :: Integer,
    variables :: [Value]
}
type VarEnv = Map.Map Ident VarStack
type VarEnvItem = (Ident, VarStack)
type FunEnv = Map.Map Ident ValueType
type Strings = Map.Map String Repr
-- Compiler state and monads.
data CState = CState {
    varEnv :: VarEnv,
    funEnv :: FunEnv,
    strings :: Strings,
    currLabel :: Label,
    usedLabels :: Integer,
    usedRegisters :: Integer,
    genInstructions :: [Instruction],
    genFunctions :: [Function]
}

type CMonadGeneral a = State CState a
type CMonad = State CState Value


predefinedFunctionsEnv :: FunEnv
predefinedFunctionsEnv =
    let modifiedPredFun = map (\ (PredefinedFunction name retType _) ->
            (Ident name, retType)) predefinedFunctions in
    Map.fromList modifiedPredFun

startingState :: CState
startingState = CState Map.empty predefinedFunctionsEnv Map.empty "" 0 0 [] []

initTopdefState :: CState -> CState
initTopdefState cState = cState {
    varEnv = Map.empty,
    currLabel = "entry",
    usedLabels = 0,
    usedRegisters = 0,
    genInstructions = [ILabel "entry"]
}


getFromVarEnv :: CState -> Ident -> Value
getFromVarEnv cState ident =
    case Map.lookup ident $ varEnv cState of
        Just stack -> head $ variables stack
        -- Can't happen.
        Nothing -> VNone

getFromVarEnvDirect :: VarEnv -> Ident -> Value
getFromVarEnvDirect env ident =
    case Map.lookup ident env of
        Just stack -> head $ variables stack
        -- Can't happen.
        Nothing -> VNone

setVarEnv :: VarEnv -> CState -> CState
setVarEnv env cState = cState {
    varEnv = env
}


pushToStack :: Ident -> Value -> CState -> CState
pushToStack ident val cState =
    case Map.lookup ident $ varEnv cState of
        Just stack -> cState {
            varEnv = Map.insert ident (VarStack (size stack + 1)
                (val : (variables stack))) $ varEnv cState
        }
        Nothing -> pushToStack ident val cState {
            varEnv = Map.insert ident (VarStack 0 []) $ varEnv cState
        }

-- Update the present value of variable.
updateStack :: Ident -> Value -> CState -> CState
updateStack ident val cState = cState {
    varEnv = Map.adjust (\var -> var { variables = val : (tail $ variables var) }) ident $ varEnv cState
}


getFromFunEnv :: CState -> Ident -> Maybe ValueType
getFromFunEnv cState ident = Map.lookup ident $ funEnv cState

insertFunction :: Ident -> ValueType -> CState -> CState
insertFunction ident vType cState = cState {
    funEnv = Map.insert ident vType $ funEnv cState
}


insertGenFunction :: Function -> CState -> CState
insertGenFunction fun cState = cState {
    genFunctions = fun : (genFunctions cState)
}


increaseLabelCount :: CState -> CState
increaseLabelCount cState =
    cState { usedLabels = (usedLabels cState) + 1 }

createLabel :: CMonadGeneral Label
createLabel = do
    labels <- gets usedLabels
    modify $ increaseLabelCount
    return $ createLabelName $ show labels

increaseRegisterCount :: CState -> CState
increaseRegisterCount cState =
    cState { usedRegisters = (usedRegisters cState) + 1 }

createRegister :: CMonadGeneral Register
createRegister = do
    registers <- gets usedRegisters
    modify $ increaseRegisterCount
    return $ createRegisterName $ show registers


insertString :: String -> CState -> CState
insertString s cState =
    let sSize = Map.size $ strings cState in
    case Map.member s $ strings cState of
        True -> cState
        False -> cState {
            strings = Map.insert s ("@str_" ++ show sSize) $ strings cState
        }

getStringRepr :: String -> CMonadGeneral Repr
getStringRepr s = do
    strs <- gets strings
    let val = Map.lookup s strs
    case val of
        Just repr -> return repr
        _ -> return ""


appendInstruction :: Instruction -> CMonadGeneral ()
appendInstruction instr@(ILabel label) = do
    cState <- get
    put $ cState {
        currLabel = label,
        genInstructions = instr : (genInstructions cState)
    }
appendInstruction instr = do
    cState <- get
    put $ cState { genInstructions = instr : (genInstructions cState) }
