module Frontend.AnalyzeStructure where

import Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe()
import Control.Monad.State
import Control.Monad.Except

import Grammar.AbsLatte
import Frontend.Utils


type Env = Map.Map Ident RetType
type BlockVariables = Set.Set Ident
type Classes = Map.Map Ident Env
-- Typechecker state and monads.
data TCState = TCState {
    env :: Env,
    classes :: Classes,
    toBeDeclaredInNextBlock :: [(Ident, RetType)],
    blockVariables :: BlockVariables,
    returnOccured :: Bool
}

type TCMonadGeneral a = StateT TCState (ExceptT Exception IO) a
type TCMonad = StateT TCState (ExceptT Exception IO) RetType

-- Predefined functions are always available for use.
predefinedFunctions :: [(Ident, RetType)]
predefinedFunctions = [
    (Ident "printInt", RFun [RInt] RVoid),
    (Ident "readInt", RFun [] RInt),
    (Ident "printString", RFun [RString] RVoid),
    (Ident "readString", RFun [] RString),
    (Ident "error", RFun [] RVoid)]

startingState :: TCState
startingState = TCState (Map.fromList predefinedFunctions) Map.empty [] Set.empty False


getFromEnv :: TCState -> Ident -> Maybe RetType
getFromEnv tcState ident = Map.lookup ident $ env tcState

getClass :: TCState -> Ident -> Maybe Env
getClass tcState ident = Map.lookup ident $ classes tcState

getField :: Env -> Ident -> Maybe RetType
getField envmt ident = Map.lookup ident envmt

markReturn :: Bool -> TCState -> TCState
markReturn retValue tcState = tcState {returnOccured = retValue}

existsInBlock :: TCState -> Ident -> Bool
existsInBlock tcState ident = Set.member ident $ blockVariables tcState

clearToBeDeclared :: TCState -> TCState
clearToBeDeclared tcState = tcState {toBeDeclaredInNextBlock = []}

clearBlockVariables :: TCState -> TCState
clearBlockVariables tcState = tcState {blockVariables = Set.empty}

restoreBlockVariables :: BlockVariables -> TCState -> TCState
restoreBlockVariables vars tcState = tcState {blockVariables = vars}

insertValue :: Ident -> RetType -> TCState -> TCState
insertValue ident retType tcState = 
    tcState {
        env = Map.insert ident retType $ env tcState,
        blockVariables = Set.insert ident $ blockVariables tcState
    }

insertValues :: [(Ident, RetType)] -> TCMonadGeneral ()
insertValues [] = return ()
insertValues ((ident, retType) : vals) = do
    modify $ insertValue ident retType
    insertValues vals

insertClass :: Ident -> TCState -> TCState
insertClass ident tcState =
    tcState {
        classes = Map.insert ident Map.empty $ classes tcState
    }

insertField :: Ident -> Env -> TCState -> TCState
insertField ident envmt tcState =
    tcState {
        classes = Map.insert ident envmt $ classes tcState
    }

insertToBeDeclared :: [(Ident, RetType)] -> TCState -> TCState
insertToBeDeclared vals tcState =
    tcState {
        toBeDeclaredInNextBlock = vals
    }

checkIsFunctionOrClass :: BNFC'Position -> Ident -> TCMonadGeneral ()
checkIsFunctionOrClass pos ident = do
    tcState <- get
    let isFun = getFromEnv tcState ident
    let isClass = getClass tcState ident
    case (isFun, isClass) of
        (Just (RFun _ _), _) -> throwError $ Redeclaration pos ident
        (_, Just _) -> throwError $ Redeclaration pos ident
        _ -> return ()

checkInBlock :: BNFC'Position -> Ident -> TCMonadGeneral ()
checkInBlock pos ident = do
    tcState <- get
    let inBlock = existsInBlock tcState ident
    if inBlock then
        throwError $ Redeclaration pos ident
    else
        return ()

checkAndInsertValue :: BNFC'Position -> Ident -> RetType -> TCMonadGeneral ()
checkAndInsertValue pos ident retType = do
    checkExistingType pos retType
    checkIsFunctionOrClass pos ident
    checkInBlock pos ident
    modify $ insertValue ident retType
    return ()

checkAndInsertValues :: BNFC'Position -> [Ident] -> [RetType] -> TCMonadGeneral ()
checkAndInsertValues _ [] [] = return ()
-- This two cases below should not ever happen.
checkAndInsertValues _ _ [] = return ()
checkAndInsertValues _ [] _ = return ()
checkAndInsertValues pos (ident : idents) (retType : retTypes) = do
    checkAndInsertValue pos ident retType
    checkAndInsertValues pos idents retTypes


getValue :: BNFC'Position -> Ident -> TCMonad
getValue pos ident = do
    tcState <- get
    let retType = getFromEnv tcState ident
    case retType of
        Just ret -> return ret
        Nothing -> throwError $ UndeclaredVariable pos ident


checkClassExists :: BNFC'Position -> Ident -> TCMonad
checkClassExists pos cls = do
    tcState <- get
    let fields = getClass tcState cls
    case fields of
        Just _ -> return $ RCls cls
        Nothing -> throwError $ UndeclaredClass pos cls

getFieldTypeFromClass :: BNFC'Position -> Ident -> Ident -> TCMonad
getFieldTypeFromClass pos cls fld = do
    tcState <- get
    let fields = getClass tcState cls
    case fields of
        Just flds -> do
            let currFld = getField flds fld
            case currFld of
                Just retType -> return retType
                Nothing -> throwError $ UndefinedField pos cls fld
        Nothing -> throwError $ VariableIsNotAClass pos cls

checkAndInsertField :: BNFC'Position -> Ident -> Ident -> RetType -> TCMonadGeneral ()
checkAndInsertField pos cls fld retType = do
    tcState <- get
    let fields = getClass tcState cls
    case fields of
        Just flds -> do
            let currFld = getField flds fld
            case currFld of
                Just _ ->
                    throwError $ Redeclaration pos fld
                Nothing ->
                    modify $ insertField cls $ Map.insert fld retType flds
        Nothing -> throwError $ UndeclaredClass pos cls


checkExistingType :: BNFC'Position -> RetType -> TCMonadGeneral ()
checkExistingType pos retType =
    case retType of
        RCls ident -> do
            checkClassExists pos ident
            return ()
        RArr arrType -> checkExistingType pos arrType
        _ -> return ()

-- Only functions (not variables) can be declared as void.
checkDeclarableType :: BNFC'Position -> RetType -> TCMonadGeneral ()
checkDeclarableType pos RVoid = throwError $ CannotDeclareVoid pos
checkDeclarableType pos (RArr arrType) = checkDeclarableType pos arrType
checkDeclarableType _ _ = return ()


checkArrayIndexType :: BNFC'Position -> RetType -> TCMonadGeneral ()
checkArrayIndexType _ RInt = return ()
checkArrayIndexType pos retType = throwError $ NotIntegerArrayIndex pos retType


throwIfConflict :: BNFC'Position -> RetType -> RetType -> TCMonad
throwIfConflict pos retExpected ret =
    if ret == retExpected then
        return ret
    else
        throwError $ MismatchingTypes pos retExpected ret
