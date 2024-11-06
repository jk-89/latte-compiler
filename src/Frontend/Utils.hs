{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Frontend.Utils where

import Prelude
import Data.List (intercalate)

import Grammar.AbsLatte


maxInt :: Integer
maxInt = (2 :: Integer) ^ (31 :: Integer) - 1

minInt :: Integer
minInt = (2 :: Integer) ^ (31 :: Integer) * (-1)

maxConstStringLength :: Int
maxConstStringLength = (2 :: Int) ^ (8 :: Int) - 1


data RetType
    = RInt
    | RBool
    | RString
    | RVoid
    | RFun [RetType] RetType
    | RCls Ident
    | RArr RetType
    deriving Eq

addableTypes :: [RetType]
addableTypes = [RInt, RString]

canCompare :: RetType -> Bool
canCompare retType =
    case retType of
        RInt -> True
        RBool -> True
        RString -> True
        RCls _ -> True
        _ -> False

instance Show RetType where
    show RInt = "Int"
    show RBool = "Bool"
    show RString = "String"
    show RVoid = "Void"
    show (RFun args retType) = (show retType) ++ "(" ++ (intercalate ", " $ map show args) ++ ")"
    show (RCls ident) = "Class " ++ (identToString ident)
    show (RArr retType) = "Array [" ++ (show retType) ++ "]"

typeToRetType :: Type -> RetType
typeToRetType (TBool _) = RBool
typeToRetType (TInt _) = RInt
typeToRetType (TStr _) = RString
typeToRetType (TVoid _) = RVoid
typeToRetType (TCls _ ident) = RCls ident
typeToRetType (TArr _ retType) = RArr $ typeToRetType retType
typeToRetType (Fun _ funType args) = RFun (map typeToRetType args) (typeToRetType funType)

argToRetType :: Arg -> RetType
argToRetType (Arg _ argType _) = typeToRetType argType

argToIdent :: Arg -> Ident
argToIdent (Arg _ _ ident) = ident


type Exception = Exception' BNFC'Position

data Exception' a
    = UndeclaredVariable a Ident
    | MismatchingTypes a RetType RetType
    | NotAFunction a Ident
    | WrongNumberOfArguments a Ident
    | Redeclaration a Ident
    | UnsupportedComparision a RetType
    | UnsupportedAddition a RetType
    | UndefinedMain
    | FunctionDidNotReturn a Ident
    | CannotDeclareVoid a
    | NonEmptyVoidReturn a
    | CannotAssign a
    | UndeclaredClass a Ident
    | NotAClassType a RetType 
    | VariableIsNotAClass a Ident
    | UndefinedField a Ident Ident
    | NotAnArrayType a RetType
    | NotIntegerArrayIndex a RetType
    | IntegerOutOfBounds a Integer
    | StringLengthOutOfBounds a Int

instance Show Exception where
    show (UndeclaredVariable pos ident) =
        appendPosToException ["Undeclared variable: ", identToString ident] pos

    show (MismatchingTypes pos retExp ret) =
        appendPosToException ["Mismatching types: expected ", show retExp, " got ", show ret] pos

    show (NotAFunction pos ident) =
        appendPosToException ["Not a function: ", identToString ident] pos

    show (WrongNumberOfArguments pos ident) =
        appendPosToException ["Wrong number of arguments passed to function: ", identToString ident] pos

    show (Redeclaration pos ident) =
        appendPosToException ["Redeclaration of: ", identToString ident] pos

    show (UnsupportedComparision pos ret) =
        appendPosToException ["Unsupported comparision between instances of type: ", show ret] pos

    show (UnsupportedAddition pos ret) =
        appendPosToException ["Unsupported addition between instances of type: ", show ret] pos

    show (UndefinedMain) = "int main() function not defined."

    show (FunctionDidNotReturn pos ident) =
        appendPosToException ["`return` keyword did not occur in each ",
            "possible branch of a function: ", identToString ident] pos

    show (CannotDeclareVoid pos) =
        appendPosToException ["Cannot declare a variable which is not a function as `void`"] pos

    show (NonEmptyVoidReturn pos) =
        appendPosToException ["Cannot return a value from `void` function"] pos

    show (CannotAssign pos) =
        appendPosToException ["Cannot assign to non-lvalue"] pos

    show (UndeclaredClass pos ident) =
        appendPosToException ["Undeclared class: ", identToString ident] pos

    show (NotAClassType pos ret) =
        appendPosToException ["Mismatching types: expected Class, got: ", show ret] pos

    show (VariableIsNotAClass pos ident) =
        appendPosToException ["Variable is not a class: ", identToString ident] pos

    show (UndefinedField pos cls fld) =
        appendPosToException ["Class: ", identToString cls, " does not have a field: ", identToString fld] pos

    show (NotAnArrayType pos ret) =
        appendPosToException ["Mismatching types: expected Array, got: ", show ret] pos

    show (NotIntegerArrayIndex pos ret) =
        appendPosToException ["Array index should be an Int, got: ", show ret] pos

    show (IntegerOutOfBounds pos n) =
        appendPosToException ["Integer value is out of bounds: ", show n] pos

    show (StringLengthOutOfBounds pos len) =
        appendPosToException ["Length of constant string is out of bounds: ", show len] pos

identToString :: Ident -> String
identToString (Ident ident) = ident

posToString :: BNFC'Position -> String
posToString (Just (line, column)) = concat ["line ", show line, ", column ", show column]
posToString _ = "Position error."

appendPosToException :: [String] -> BNFC'Position -> String
appendPosToException exception pos =
    concat [concat exception, ", at ", posToString pos, "."]
