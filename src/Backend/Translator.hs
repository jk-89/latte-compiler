-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Backend.Translator where

import Data.List (intercalate)

import Grammar.AbsLatte

-- Representation of something in LLVM.
type Repr = String
type Name = String
type Label = String
type Register = String

createRegisterName :: String -> Register
createRegisterName reg = "%_reg_" ++ reg

createLabelName :: String -> Label
createLabelName label = "_lab_" ++ label

showLabel :: Label -> Repr
showLabel label = "label %" ++ label

showLabelPhi :: Label -> Repr
showLabelPhi label = "%" ++ label


data ValueType
    = VTBool
    | VTChar
    | VTInt
    | VTPtr ValueType
    | VTVoid
    deriving Eq

instance Show ValueType where
    show VTBool = "i1"
    show VTChar = "i8"
    show VTInt = "i32"
    show (VTPtr vType) = (show vType) ++ "*"
    show VTVoid = "void"


data Value
    = VInt Integer
    | VBool Bool
    | VRegister ValueType Register
    | VNone
    deriving Eq

instance Show Value where
    show (VInt n) = show n
    show (VBool True) = "true"
    show (VBool False) = "false"
    show (VRegister _ reg) = reg
    show VNone = ""

showTypedValue :: Value -> String
showTypedValue val = (show $ typeOfValue val) ++ " " ++ (show val)

typeOfValue :: Value -> ValueType
typeOfValue (VInt _) = VTInt
typeOfValue (VBool _) = VTBool
typeOfValue (VRegister vType _) = vType
typeOfValue VNone = VTVoid

typeToValueType :: Type -> ValueType
typeToValueType (TInt _) = VTInt
typeToValueType (TStr _) = VTPtr VTChar
typeToValueType (TBool _) = VTBool
typeToValueType (TVoid _) = VTVoid
typeToValueType _ = VTVoid

argToValueType :: Arg -> ValueType
argToValueType (Arg _ argType _) = typeToValueType argType


data Instruction
    = IBinaryOp Repr Register Value Value
    | ILabel Label
    | IBranch Label
    | IBranchCond Label Label Value
    | ICall Register ValueType Name [Value]
    | ICallVoid Name [Value]
    | IReturn Value
    | IReturnVoid
    | IBitcast Register Int Repr
    | IPhi Register [(Value, Label)]

instance Show Instruction where
    show (IBinaryOp repr reg val1 val2) = 
        concat [reg, " = ", repr, " ", show $ typeOfValue val1, " ", show val1, ", ", show val2]
    show (ILabel label) = concat [label, ":"]
    show (IBranch label) = concat ["br ", showLabel label]
    show (IBranchCond label1 label2 val) =
        concat ["br i1 ", show val, ", ", showLabel label1, ", ", showLabel label2]
    show (ICall reg vType name args) = concat [
        reg, " = call ", show vType, " ", name,
        "(", intercalate ", " $ map showTypedValue args, ")"]
    show (ICallVoid name args) =
        concat ["call void ", name, "(", intercalate ", " $ map showTypedValue args, ")"]
    show (IReturn val) = concat ["ret ", show $ typeOfValue val, " ", show val]
    show IReturnVoid = "ret void"
    show (IBitcast reg size repr) =
        concat [reg, " = bitcast [", show $ size + 1, " x i8]* ", repr, " to i8*"]
    show (IPhi reg options) =
        let optionsRepr = intercalate ", " $ map createPhiElem options in
        concat [reg, " = phi ", show $ typeOfValue $ fst $ head options, " ", optionsRepr]

createPhiElem :: (Value, Label) -> String
createPhiElem (val, label) =
    concat ["[ ", show val, ", ", showLabelPhi label, " ]"]


data BasicBlock = BasicBlock {
    initLabel :: Label,
    instructions :: [Instruction]
}

instance Show BasicBlock where
    show (BasicBlock label instrs) = concat [
        label, ":\n", intercalate "\n" $ map (\repr -> "    " ++ repr) $
        map show instrs, "\n"]

emptyBlockWithLabel :: Label -> BasicBlock
emptyBlockWithLabel label = BasicBlock label []

expandBlock :: Instruction -> BasicBlock -> BasicBlock
expandBlock instr block =
    block { instructions = instr : (instructions block) }

fillEmptyBlock :: BasicBlock -> BasicBlock
fillEmptyBlock (BasicBlock label []) = BasicBlock label [IBranch $ label]
fillEmptyBlock block = block

-- Given an instruction either enhance current block or creates a new one.
addInstructionToBlock :: (BasicBlock, [BasicBlock]) -> Instruction -> (BasicBlock, [BasicBlock])
addInstructionToBlock (block, blocks) (ILabel label) =
    (emptyBlockWithLabel "", block { initLabel = label } : blocks)
addInstructionToBlock (block, blocks) instr =
    (block { instructions = instr : instructions block }, blocks)


data Function = Function {
    fName :: Name,
    fRetType :: ValueType,
    fArgs :: [Value],
    fBlocks :: [BasicBlock]
}

instance Show Function where
    show (Function name retType args blocks) = concat [
        "define ", show retType, " ", name, "(",
        intercalate ", " $ map showTypedValue args, ") {\n",
        intercalate "\n" $ map show blocks, "}\n"]


data PredefinedFunction = PredefinedFunction {
    pfName :: Name,
    pfRetType :: ValueType,
    pfArgs :: [ValueType]
}

instance Show PredefinedFunction where
    show (PredefinedFunction name retType args) =
        concat ["declare ", show retType, " @", name, "(", intercalate ", " $ map show args, ")"]

predefinedFunctions :: [PredefinedFunction]
predefinedFunctions = [
    PredefinedFunction "printInt" VTVoid [VTInt],
    PredefinedFunction "readInt" VTInt [],
    PredefinedFunction "printString" VTVoid [VTPtr VTChar],
    PredefinedFunction "readString" (VTPtr VTChar) [],
    PredefinedFunction "error" VTVoid [],
    PredefinedFunction "_concatenateStrings" (VTPtr VTChar) [VTPtr VTChar, VTPtr VTChar],
    PredefinedFunction "_stringsEqual" VTBool [VTPtr VTChar, VTPtr VTChar],
    PredefinedFunction "_stringsNotEqual" VTBool [VTPtr VTChar, VTPtr VTChar]]


-- Uninitialized values of some types are initialized with default values.
defaultValue :: Type -> Expr
defaultValue (TInt pos) = ELitInt pos 0
defaultValue (TBool pos) = ELitFalse pos
defaultValue (TStr pos) = EString pos ""
defaultValue (TCls pos ident) = ENullCls pos ident
-- Below: can't happen.
defaultValue (TArr pos _) = ELitFalse pos
defaultValue (TVoid pos) = ELitFalse pos
defaultValue (Fun pos _ _) = ELitFalse pos


-- For a constant strings we have to rewrite special characters to LLVM representation.
specialChars :: Char -> String
specialChars '\n' = "\\0A"
specialChars '\t' = "\\09"
specialChars '\"' = "\\22"
specialChars '\\' = "\\\\"
specialChars c = [c]

showConstantString :: String -> String -> String
showConstantString s repr =
    let len = length s in
    let llvmS = concatMap specialChars s in
    concat [repr, " = internal constant [", show $ len + 1, " x i8] c\"", llvmS, "\\00\""]

showFunctionName :: Ident -> String
showFunctionName (Ident ident) = "@" ++ ident

showStringEqualityFunctionName :: Repr -> String
showStringEqualityFunctionName "eq" = showFunctionName $ Ident "_stringsEqual"
showStringEqualityFunctionName "ne" = showFunctionName $ Ident "_stringsNotEqual"
-- Cannot happen.
showStringEqualityFunctionName _ = ""
