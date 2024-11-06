{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Frontend.SemanticAnalyze (semanticAnalyze) where

import Control.Monad.State
import Control.Monad.Except

import Grammar.AbsLatte
import Frontend.AnalyzeStructure
import Frontend.Utils


semanticAnalyze :: Program -> IO (Either Exception RetType)
semanticAnalyze program =
    runExceptT $ evalStateT (typeCheckStmt RVoid program) startingState


-- Checks if all expressions in a list are of a specified type.
-- Returns this type in case of success.
checkSameType :: BNFC'Position -> RetType -> [Expr] -> TCMonad
checkSameType _ expType [] = return $ expType
checkSameType pos expType (expr : exprs) = do
    exprType <- typeCheckExpr expr
    throwIfConflict pos expType exprType
    checkSameType pos expType exprs


checkEqualTypesLists :: BNFC'Position -> Ident -> [RetType] -> [RetType] -> TCMonadGeneral ()
checkEqualTypesLists _ _ [] [] = return ()
checkEqualTypesLists pos ident [] _ = throwError $ WrongNumberOfArguments pos ident
checkEqualTypesLists pos ident _ [] = throwError $ WrongNumberOfArguments pos ident
checkEqualTypesLists pos ident (ret1 : rets1) (ret2 : rets2) =
    throwIfConflict pos ret1 ret2 >> checkEqualTypesLists pos ident rets1 rets2


-- Expressions which can be used as left values in assignment (lValue = ...).
isLValue :: Expr -> TCMonadGeneral Bool
isLValue (EVar _ _) = return True
isLValue (EArrGet _ _ _) = return True
-- array.length is not mutable.
isLValue (EAccFld _ expr ident) = do
    exprType <- typeCheckExpr expr
    case (exprType, ident) of
        (RArr _, Ident "length") -> return False
        _ -> return True
isLValue _ = return False


class TypeCheckExpr a where
    typeCheckExpr :: a -> TCMonad

class TypeCheckItem a where
    typeCheckItem :: RetType -> a -> TCMonad

class TypeCheckStmt a where
    -- Sometimes we have to check if e.g. block returns a proper type.
    typeCheckStmt :: RetType -> a -> TCMonad

class TypeCheckClass a where
    -- Class block has to know the name of a class (Ident).
    typeCheckClass :: Ident -> a -> TCMonadGeneral ()


instance TypeCheckExpr Expr where
    typeCheckExpr (ELitInt pos n) =
        if (minInt <= n) && (n <= maxInt) then
            return RInt
        else
            throwError $ IntegerOutOfBounds pos n

    typeCheckExpr (ELitTrue _) = return RBool

    typeCheckExpr (ELitFalse _) = return RBool

    typeCheckExpr (EString pos s) =
        let len = length s in
        if len <= maxConstStringLength then
            return RString
        else
            throwError $ StringLengthOutOfBounds pos len

    typeCheckExpr (EVar pos ident) = getValue pos ident


    typeCheckExpr (EAccFld pos expr ident) = do
        exprType <- typeCheckExpr expr
        case exprType of
            -- Arrays can be asked about their length.
            RArr _ -> case ident of
                Ident "length" -> return RInt
                _ -> throwError $ NotAClassType pos exprType
            RCls cls -> getFieldTypeFromClass pos cls ident
            _ -> throwError $ NotAClassType pos exprType

    typeCheckExpr (ENewCls pos clsType) =
        case clsType of
            TCls _ ident -> checkClassExists pos ident
            _ -> throwError $ NotAClassType pos $ typeToRetType clsType

    typeCheckExpr (ENullCls pos ident) = checkClassExists pos ident


    typeCheckExpr (EArrGet pos expr1 expr2) = do
        exprType1 <- typeCheckExpr expr1
        case exprType1 of
            RArr arrType -> do
                exprType2 <- typeCheckExpr expr2
                checkArrayIndexType pos exprType2
                return arrType
            _ -> throwError $ NotAnArrayType pos exprType1

    typeCheckExpr (ENewArr pos arrType expr) = do
        exprType <- typeCheckExpr expr
        checkArrayIndexType pos exprType
        checkExistingType pos $ typeToRetType arrType
        checkDeclarableType pos $ typeToRetType arrType
        return $ RArr $ typeToRetType arrType


    typeCheckExpr (ENot pos expr) = checkSameType pos RBool [expr]

    typeCheckExpr (EAnd pos expr1 expr2) = checkSameType pos RBool [expr1, expr2]

    typeCheckExpr (EOr pos expr1 expr2) = checkSameType pos RBool [expr1, expr2]

    typeCheckExpr (ERel pos expr1 relOp expr2) = do
        -- We can check equality for any in-built type.
        case relOp of
            EQU _ -> do
                exprType1 <- typeCheckExpr expr1
                exprType2 <- typeCheckExpr expr2
                throwIfConflict pos exprType1 exprType2
                if canCompare exprType1 then
                    return RBool
                else
                    throwError $ UnsupportedComparision pos exprType1
            NE _ ->
                typeCheckExpr (ERel pos expr1 (EQU pos) expr2)
            _ -> do
                checkSameType pos RInt [expr1, expr2]
                return RBool


    typeCheckExpr (ENeg pos expr) = checkSameType pos RInt [expr]

    typeCheckExpr (EAdd pos expr1 (Plus _) expr2) = do
        -- We can apply "+" to e.g. two strings.
        exprType1 <- typeCheckExpr expr1
        exprType2 <- typeCheckExpr expr2
        throwIfConflict pos exprType1 exprType2
        if elem exprType1 addableTypes then
            return $ exprType1
        else
            throwError $ UnsupportedAddition pos exprType1

    typeCheckExpr (EAdd pos expr1 (Minus _) expr2) = do
        checkSameType pos RInt [expr1, expr2]

    typeCheckExpr (EMul pos expr1 _ expr2) = checkSameType pos RInt [expr1, expr2]


    typeCheckExpr (EApp pos ident args) = do
        fun <- getValue pos ident
        case fun of
            RFun funArgs funType -> do
                argTypes <- mapM typeCheckExpr args
                checkEqualTypesLists pos ident funArgs argTypes
                return $ funType
            _ -> throwError $ NotAFunction pos ident


instance TypeCheckItem Item where
    typeCheckItem expectedType (SNoInit pos ident) = do
        checkAndInsertValue pos ident expectedType
        return RVoid

    typeCheckItem expectedType (SInit pos ident expr) = do
        typeCheckExpr expr >>= throwIfConflict pos expectedType
        checkAndInsertValue pos ident expectedType
        return RVoid


instance TypeCheckStmt Stmt where
    typeCheckStmt _ (SEmpty _) = return $ RVoid

    typeCheckStmt _ (SExp _ expr) = typeCheckExpr expr

    typeCheckStmt retType (SBStmt _ block) = do
        variablesBlockBefore <- gets blockVariables
        modify $ clearBlockVariables
        envmt <- get
        insertValues $ toBeDeclaredInNextBlock envmt
        modify $ clearToBeDeclared
        typeCheckStmt retType block
        retValue <- gets returnOccured
        put envmt
        modify $ markReturn retValue
        modify $ restoreBlockVariables variablesBlockBefore
        return RVoid

    typeCheckStmt _ (SDecl pos declType items) = do
        checkDeclarableType pos $ typeToRetType declType
        mapM_ (typeCheckItem $ typeToRetType declType) items
        return RVoid

    typeCheckStmt _ (SIncr pos expr) = do
        exprType <- typeCheckExpr expr
        lValue <- isLValue expr
        if lValue then
            throwIfConflict pos RInt exprType
        else
            throwError $ CannotAssign pos

    typeCheckStmt _ (SDecr pos expr) = typeCheckStmt RVoid (SIncr pos expr)

    typeCheckStmt _ (SAss pos expr1 expr2) = do
        exprType1 <- typeCheckExpr expr1
        exprType2 <- typeCheckExpr expr2
        lValue <- isLValue expr1
        if lValue then
            throwIfConflict pos exprType1 exprType2
        else
            throwError $ CannotAssign pos

    -- Cannot return something else in void function.
    typeCheckStmt RVoid (SRet pos _) =
        throwError $ NonEmptyVoidReturn pos

    typeCheckStmt expType (SRet pos expr) = do
        modify $ markReturn True
        typeCheckExpr expr >>= throwIfConflict pos expType

    typeCheckStmt expType (SVRet pos) =
        if expType == RVoid then
            return RVoid
        else
            throwError $ MismatchingTypes pos expType RVoid

    typeCheckStmt expType (SCond pos expr stmt) = do
        typeCheckExpr expr >>= throwIfConflict pos RBool
        typeCheckStmt expType stmt
        -- It does not matter if `return` occured in a single-branch `if`.
        modify $ markReturn False
        return RVoid

    typeCheckStmt expType (SCondElse pos expr stmt1 stmt2) = do
        typeCheckExpr expr >>= throwIfConflict pos RBool
        typeCheckStmt expType stmt1
        retFirstBranch <- gets returnOccured
        modify $ markReturn False
        typeCheckStmt expType stmt2
        retSecondBranch <- gets returnOccured
        modify $ markReturn $ retFirstBranch && retSecondBranch
        return RVoid

    -- while (true) case.
    typeCheckStmt expType (SWhile _ (ELitTrue _) stmt) = do
        typeCheckStmt expType stmt

    typeCheckStmt expType (SWhile pos expr stmt) = do
        typeCheckExpr expr >>= throwIfConflict pos RBool
        typeCheckStmt expType stmt
        -- It does not matter if `return` occured in `while`,
        -- because maybe we did not enter the loop block.
        modify $ markReturn False
        return RVoid

    typeCheckStmt expType (SFor pos iterType ident expr stmt) = do
        exprType <- typeCheckExpr expr
        -- Foreach works only on arrays.
        case exprType of
            RArr arrType -> do
                let iterRetType = typeToRetType iterType
                throwIfConflict pos arrType iterRetType
                checkIsFunctionOrClass pos ident
                modify $ insertToBeDeclared [(ident, iterRetType)]
                typeCheckStmt expType stmt
                -- It does not matter if `return` occured in `for`,
                -- because maybe we did not enter the loop block.
                modify $ markReturn False
                return RVoid
            _ -> throwError $ NotAnArrayType pos exprType


-- Before checking a Stmt clears `returnOccured`.
clearReturnAndTypecheck :: [Stmt] -> RetType -> TCMonadGeneral ()
clearReturnAndTypecheck [] _ = return ()
clearReturnAndTypecheck (stmt : stmts) expType = do
    modify $ markReturn False
    typeCheckStmt expType stmt
    clearReturnAndTypecheck stmts expType

instance TypeCheckStmt Block where
    typeCheckStmt expType (Block _ stmts) = do
        clearReturnAndTypecheck stmts expType
        return RVoid


instance TypeCheckClass ClsBlock where
    typeCheckClass clsIdent (ClsBlock _ clsDefs) =
        mapM_ (typeCheckClass clsIdent) clsDefs

instance TypeCheckClass ClsDef where
    typeCheckClass clsIdent (ClsFldDef pos fldType ident) = do
        let retFldType = typeToRetType fldType
        checkExistingType pos retFldType
        checkDeclarableType pos retFldType
        checkAndInsertField pos clsIdent ident retFldType


parseTopdef :: TopDef -> TCMonadGeneral ()
parseTopdef topdef = do
    case topdef of
        FnDef pos funType ident args _ -> do
            let funRetType = typeToRetType funType
            let argTypes = map argToRetType args
            mapM_ (checkDeclarableType pos) argTypes
            checkAndInsertValue pos ident (RFun argTypes funRetType)
        ClsDef pos ident _ -> do
            checkIsFunctionOrClass pos ident
            modify $ insertClass ident

parseClassBlocks :: TopDef -> TCMonadGeneral ()
parseClassBlocks topdef = do
    case topdef of
        ClsDef _ ident block -> typeCheckClass ident block
        _ -> return ()


instance TypeCheckStmt Program where
    typeCheckStmt _ (Program _ topdefs) = do
        mapM_ parseTopdef topdefs
        -- We have to parse names of classes before parsing contents of the classes.
        mapM_ parseClassBlocks topdefs
        tcState <- get
        let mainFun = getFromEnv tcState (Ident "main")
        case mainFun of
            Just (RFun [] RInt) -> do
                mapM_ (typeCheckStmt RVoid) topdefs
                return RVoid
            _ -> throwError $ UndefinedMain


instance TypeCheckStmt TopDef where
    typeCheckStmt _ (FnDef pos _ ident args block) = do
        modify $ markReturn False
        tcState <- get
        let retFun = getFromEnv tcState ident
        -- At this point topdefs are already parsed so function must exist in env.
        case retFun of
            Just (RFun argTypes funRetType) -> do
                mapM_ (checkExistingType pos) argTypes
                let argIdents = map argToIdent args
                checkAndInsertValues pos argIdents argTypes
                typeCheckStmt funRetType block
                retValue <- gets returnOccured
                -- Check if `return` occured.
                -- Restore environment before the function definition.
                case (funRetType, retValue) of
                    -- Void function does not have to use `return` keyword.
                    (RVoid, _) -> do
                        put tcState
                        return RVoid
                    (_, False) ->
                        throwError $ FunctionDidNotReturn pos ident
                    _ -> do
                        put tcState
                        return RVoid
            _ -> throwError $ UndeclaredVariable pos ident

    -- Already parsed.
    typeCheckStmt _ (ClsDef _ _ _) = return RVoid
