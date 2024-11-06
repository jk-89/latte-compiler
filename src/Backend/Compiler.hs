{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Backend.Compiler (compile) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.List (intercalate, foldl')
import Data.Functor.Identity

import Grammar.AbsLatte
import Backend.Structure
import Backend.Translator


compile :: Program -> String
compile program =
    -- Computes final state of a monadic computation.
    let finalState = runIdentity $ execStateT (compileStmt program) startingState in
    let predFuncs = intercalate "\n" $ map show predefinedFunctions in
    let constantStrings = map (\(s, repr) -> showConstantString s repr) $ Map.toList $ strings finalState in
    let compiledProg = intercalate "\n" $ map show $ genFunctions finalState in
    intercalate "\n\n" [predFuncs, intercalate "\n" constantStrings, compiledProg]


class CompileExpr a where
    compileExpr :: a -> CMonad

class CompileStmt a where
    compileStmt :: a -> CMonadGeneral ()

class CompileItem a where
    compileItem :: Type -> a -> CMonadGeneral ()

class CompileTopdef a where
    compileTopdef :: a -> CMonadGeneral ()


compileBinaryOp :: ValueType -> Repr -> Expr -> Expr -> CMonad
compileBinaryOp resultType repr expr1 expr2 = do
    val1 <- compileExpr expr1
    val2 <- compileExpr expr2
    reg <- createRegister
    appendInstruction $ IBinaryOp repr reg val1 val2
    return $ VRegister resultType reg

compileRelOp :: Repr -> Expr -> Expr -> CMonad
compileRelOp repr expr1 expr2 =
    compileBinaryOp VTBool ("icmp " ++ repr) expr1 expr2

compileEqualityOp :: Repr -> Expr -> Expr -> CMonad
compileEqualityOp repr expr1 expr2 = do
    val1 <- compileExpr expr1
    val2 <- compileExpr expr2
    reg <- createRegister
    -- Equality of strings is handled in a different way.
    case val1 of
        (VRegister (VTPtr VTChar) _) -> do
            let name = showStringEqualityFunctionName repr
            appendInstruction $ ICall reg VTBool name [val1, val2]
            return $ VRegister VTBool reg
        _ -> do
            appendInstruction $ IBinaryOp ("icmp " ++ repr) reg val1 val2
            return $ VRegister VTBool reg


instance CompileExpr Expr where
    compileExpr (ELitInt _ n) = return $ VInt n

    compileExpr (ELitTrue _) = return $ VBool True

    compileExpr (ELitFalse _) = return $ VBool False

    compileExpr (EString _ s) = do
        modify $ insertString s
        reg <- createRegister
        repr <- getStringRepr s
        appendInstruction $ IBitcast reg (length s) repr
        return $ VRegister (VTPtr VTChar) reg

    compileExpr (EVar _ ident) = do
        env <- get
        return $ getFromVarEnv env ident


    compileExpr (EAnd _ expr1 expr2) = do
        -- Check the first branch of conjunction.
        val1 <- compileExpr expr1
        labExpr1 <- gets currLabel
        labFirstTrue <- createLabel
        labFinish <- createLabel
        appendInstruction $ IBranchCond labFirstTrue labFinish val1
        appendInstruction $ ILabel labFirstTrue
        -- Program will check the second branch only if the first was true.
        val2 <- compileExpr expr2
        labExpr2 <- gets currLabel
        appendInstruction $ IBranch labFinish
        appendInstruction $ ILabel labFinish
        reg <- createRegister
        appendInstruction $ IPhi reg [(VBool False, labExpr1), (val2, labExpr2)]
        return $ VRegister VTBool reg

    compileExpr (EOr _ expr1 expr2) = do
        -- Check the first branch of alternative.
        val1 <- compileExpr expr1
        labExpr1 <- gets currLabel
        labFirstFalse <- createLabel
        labFinish <- createLabel
        appendInstruction $ IBranchCond labFinish labFirstFalse val1
        appendInstruction $ ILabel labFirstFalse
        -- Program will check the second branch only if the first was false.
        val2 <- compileExpr expr2
        labExpr2 <- gets currLabel
        appendInstruction $ IBranch labFinish
        appendInstruction $ ILabel labFinish
        reg <- createRegister
        appendInstruction $ IPhi reg [(VBool True, labExpr1), (val2, labExpr2)]
        return $ VRegister VTBool reg


    compileExpr (ENot pos expr) = compileBinaryOp VTBool "xor" (ELitTrue pos) expr

    compileExpr (ENeg pos expr) = compileBinaryOp VTInt "sub" (ELitInt pos 0) expr


    compileExpr (EAdd _ expr1 (Plus _) expr2) = do
        val1 <- compileExpr expr1
        val2 <- compileExpr expr2
        reg <- createRegister
        let valType = typeOfValue val1
        case valType of
            VTPtr VTChar -> do
                let name = showFunctionName $ Ident "_concatenateStrings"
                appendInstruction $ ICall reg (VTPtr VTChar) name [val1, val2]
                return $ VRegister (VTPtr VTChar) reg
            _ -> do
                appendInstruction $ IBinaryOp "add" reg val1 val2
                return $ VRegister VTInt reg 

    compileExpr (EAdd _ expr1 (Minus _) expr2) = compileBinaryOp VTInt "sub" expr1 expr2

    compileExpr (EMul _ expr1 (Times _) expr2) = compileBinaryOp VTInt "mul" expr1 expr2

    compileExpr (EMul _ expr1 (Div _) expr2) = compileBinaryOp VTInt "sdiv" expr1 expr2

    compileExpr (EMul _ expr1 (Mod _) expr2) = compileBinaryOp VTInt "srem" expr1 expr2

    compileExpr (ERel _ expr1 (LTH _) expr2) = compileRelOp "slt" expr1 expr2

    compileExpr (ERel _ expr1 (LE _) expr2) = compileRelOp "sle" expr1 expr2

    compileExpr (ERel _ expr1 (GTH _) expr2) = compileRelOp "sgt" expr1 expr2

    compileExpr (ERel _ expr1 (GE _) expr2) = compileRelOp "sge" expr1 expr2

    compileExpr (ERel _ expr1 (EQU _) expr2) = compileEqualityOp "eq" expr1 expr2

    compileExpr (ERel _ expr1 (NE _) expr2) = compileEqualityOp "ne" expr1 expr2


    compileExpr (EApp _ ident args) = do
        env <- get
        compArgs <- mapM compileExpr args
        reg <- createRegister
        let val = getFromFunEnv env ident
        case val of
            Just VTVoid -> do
                appendInstruction $ ICallVoid (showFunctionName ident) compArgs
                return VNone
            Just retType -> do
                appendInstruction $ ICall reg retType (showFunctionName ident) compArgs
                return $ VRegister retType reg
            _ -> return VNone

    compileExpr _ = return VNone


instance CompileItem Item where
    compileItem declType (SNoInit pos ident) =
        compileItem declType (SInit pos ident $ defaultValue declType)

    compileItem _ (SInit _ ident expr) = do
        val <- compileExpr expr
        modify $ pushToStack ident val


instance CompileStmt Stmt where
    compileStmt (SEmpty _ ) = return ()

    compileStmt (SExp _ expr) = do
        compileExpr expr
        return ()

    compileStmt (SDecl _ declType items) =
        mapM_ (compileItem declType) items

    compileStmt (SBStmt _ block) = do
        vars <- gets varEnv
        compileStmt block
        -- For each variable we have to pop its value which go out of scope.
        -- After that the topmost value on the stack could be changed inside the block.
        newVars <- gets varEnv
        let finalVars = Map.intersectionWith finishScope vars newVars
        modify $ setVarEnv finalVars
        where
            finishScope :: VarStack -> VarStack -> VarStack
            finishScope stack1 stack2 =
                case (size stack1) < (size stack2) of
                    True -> VarStack (size stack1) (tail $ variables stack2)
                    False -> stack2


    compileStmt (SIncr pos expr) =
        compileStmt $ SAss pos expr $ EAdd pos expr (Plus pos) $ ELitInt pos 1

    compileStmt (SDecr pos expr) =
        compileStmt $ SAss pos expr $ EAdd pos expr (Minus pos) $ ELitInt pos 1

    compileStmt (SAss _ expr1 expr2) = do
        val <- compileExpr expr2
        case expr1 of
            EVar _ ident -> modify $ updateStack ident val
            _ -> return ()

    compileStmt (SRet _ expr) = do
        val <- compileExpr expr
        appendInstruction $ IReturn val

    compileStmt (SVRet _) = appendInstruction IReturnVoid


    compileStmt (SCond pos expr stmt) =
        compileStmt $ SCondElse pos expr stmt $ SEmpty pos

    compileStmt (SCondElse _ expr stmt1 stmt2) = do
        val <- compileExpr expr
        varsOrig <- gets varEnv
        labTrue <- createLabel
        labFalse <- createLabel
        labFinish <- createLabel
        appendInstruction $ IBranchCond labTrue labFalse val

        appendInstruction $ ILabel labTrue
        compileStmt stmt1
        labTrueEnd <- gets currLabel
        varsTrue <- gets varEnv
        appendInstruction $ IBranch labFinish

        appendInstruction $ ILabel labFalse
        modify $ setVarEnv varsOrig
        compileStmt stmt2
        labFalseEnd <- gets currLabel
        varsFalse <- gets varEnv
        appendInstruction $ IBranch labFinish

        -- Generate Phi function.
        -- Use Phi only over values changed inside some block.
        appendInstruction $ ILabel labFinish
        let modifiedVarsTrue = Map.intersectionWith findModified varsOrig varsTrue
        let modifiedVarsFalse = Map.intersectionWith findModified varsOrig varsFalse
        let modifiedVars = Map.unionWith mergeModified modifiedVarsTrue modifiedVarsFalse
        let phiList = Map.toList $ Map.mapMaybe id modifiedVars
        mapM_ (updateModified labTrueEnd labFalseEnd) phiList
        where
            -- We are sure that stacks already have equal lengths.
            findModified :: VarStack -> VarStack -> Maybe (Value, Value)
            findModified stack1 stack2 =
                let val1 = head $ variables stack1 in
                let val2 = head $ variables stack2 in
                case val1 /= val2 of
                    True -> Just (val1, val2)
                    False -> Nothing

            mergeModified :: Maybe (Value, Value) -> Maybe (Value, Value) -> Maybe (Value, Value)
            mergeModified Nothing Nothing = Nothing
            mergeModified Nothing (Just (val1, val2)) = Just (val1, val2)
            mergeModified (Just (val1, val2)) Nothing = Just (val2, val1)
            mergeModified (Just (_, val2)) (Just (_, val3)) = Just (val2, val3)

            updateModified :: Label -> Label -> (Ident, (Value, Value)) -> CMonadGeneral ()
            updateModified label1 label2 (ident, (val1, val2)) = do
                reg <- createRegister
                appendInstruction $ IPhi reg [(val1, label1), (val2, label2)]
                modify $ updateStack ident $ VRegister (typeOfValue val1) reg


    compileStmt (SWhile _ expr stmt) = do
        varsOrig <- gets varEnv
        labOrig <- gets currLabel
        labCond <- createLabel
        labBody <- createLabel
        labEnd <- createLabel
        envOrig <- get
        -- Find variables which will change in the body.
        compileStmt stmt
        varsBody <- gets varEnv
        put envOrig
        let modifiedVars = Map.intersectionWith findModified varsOrig varsBody
        let phiList = Map.toList $ Map.mapMaybe id modifiedVars
        -- For each variable which will be modified create a new register.
        mapM_ addRegisters phiList
        varsWithRegs <- gets varEnv

        appendInstruction $ IBranch labCond
        appendInstruction $ ILabel labBody
        compileStmt stmt
        varsBodyEnd <- gets varEnv
        labBodyEnd <- gets currLabel
        appendInstruction $ IBranch labCond
        appendInstruction $ ILabel labCond

        -- Using created registers generate correct Phi functions.
        mapM_ (updateModified varsWithRegs varsBodyEnd labOrig labBodyEnd) phiList
        val <- compileExpr expr
        appendInstruction $ IBranchCond labBody labEnd val
        appendInstruction $ ILabel labEnd
        where
            -- We are sure that stacks already have equal lengths.
            findModified :: VarStack -> VarStack -> Maybe Value
            findModified stack1 stack2 =
                let val1 = head $ variables stack1 in
                let val2 = head $ variables stack2 in
                case val1 /= val2 of
                    True -> Just val1
                    False -> Nothing

            addRegisters :: (Ident, Value) -> CMonadGeneral ()
            addRegisters (ident, val) = do
                reg <- createRegister
                modify $ updateStack ident $ VRegister (typeOfValue val) reg

            updateModified :: VarEnv -> VarEnv -> Label -> Label -> (Ident, Value) -> CMonadGeneral ()
            updateModified varWithReg varBodyEnd label1 label2 (ident, val1) = do
                let (VRegister _ reg) = getFromVarEnvDirect varWithReg ident
                let val2 = getFromVarEnvDirect varBodyEnd ident
                appendInstruction $ IPhi reg [(val1, label1), (val2, label2)]
                modify $ updateStack ident $ VRegister (typeOfValue val1) reg


    compileStmt _ = return ()


instance CompileStmt Block where
    compileStmt (Block _ stmts) =
        mapM_ compileStmt stmts


parseTopdef :: TopDef -> CMonadGeneral ()
parseTopdef topdef = do
    case topdef of
        FnDef _ funType ident _ _ -> do
            let funVType = typeToValueType funType
            modify $ insertFunction ident funVType
        _ -> return ()


instance CompileStmt Program where
    compileStmt (Program _ topdefs) = do
        mapM_ parseTopdef topdefs
        mapM_ compileTopdef topdefs


instance CompileTopdef TopDef where
    compileTopdef (FnDef _ funType ident args block) = do
        -- Parse function arguments into variable environment values.
        let funVType = typeToValueType funType
        let argsWithVTypes = zip args $ map argToValueType args
        let argValues = map (\ (Arg _ _ origIdent@(Ident ident_), argType) ->
                (origIdent, VRegister argType $ createRegisterName ident_)) argsWithVTypes
        let newEnv = Map.fromList $ map (\ (origIdent, val) ->
                (origIdent, VarStack 1 [val])) argValues
        modify $ initTopdefState
        modify $ setVarEnv newEnv
        compileStmt block
        -- After compiling the block we create a Function object and store it in state.
        -- Later it will be parsed to the final code.
        let funName = showFunctionName ident
        let funArgs = map snd argValues
        instrs <- gets genInstructions
        let funBlocks = snd $ foldl' addInstructionToBlock (emptyBlockWithLabel "", []) instrs
        let nonEmptyFunBlocks = map fillEmptyBlock funBlocks
        modify $ insertGenFunction $ Function funName funVType funArgs nonEmptyFunBlocks

    compileTopdef _ = return ()
