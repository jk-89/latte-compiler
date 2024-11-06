module Frontend.ParseProgram (parseProgram) where

import Data.Maybe()

import Grammar.AbsLatte


-- This module serves three purposes:
-- 1) parse constant expressions e.g
--    * ((13 + 8) * 10) --> 210
--    * if (true) {...} --> {...}
-- 2) cut some blocks if `return` keyword occurs e.g.
--    * {printInt(x); return x; printInt(x)} --> {printInt(x); return x;}
-- 3) pack one-line statements in `if` / `while` / `for` into blocks
--    * if (x == 3) printInt(x); --> if (x == 3) {printInt(x);}


getBoolValue :: Expr -> Maybe Bool
getBoolValue (ELitTrue _) = Just True
getBoolValue (ELitFalse _) = Just False
getBoolValue _ = Nothing

getBoolExpr :: BNFC'Position -> Bool -> Expr
getBoolExpr pos True = ELitTrue pos
getBoolExpr pos False = ELitFalse pos

getIntValue :: Expr -> Maybe Integer
getIntValue (ELitInt _ n) = Just n
getIntValue _ = Nothing

getIntExpr :: Integer -> BNFC'Position -> Expr
getIntExpr n pos = ELitInt pos n

isIntLit :: Expr -> Bool
isIntLit (ELitInt _ _) = True
isIntLit _ = False

isBoolLit :: Expr -> Bool
isBoolLit (ELitTrue _) = True
isBoolLit (ELitFalse _) = True
isBoolLit _ = False


parseBBBExpr :: Expr -> Expr -> (Bool -> Bool -> Bool) -> (Expr, Expr, Maybe Bool)
parseBBBExpr expr1 expr2 op =
    let newExpr1 = parseExpr expr1 in
    let newExpr2 = parseExpr expr2 in
    let boolV1 = getBoolValue newExpr1 in
    let boolV2 = getBoolValue newExpr2 in
    case (boolV1, boolV2) of
        (Just b1, Just b2) -> (newExpr1, newExpr2, Just $ b1 `op` b2)
        _ -> (newExpr1, newExpr2, Nothing)

parseIIIExpr :: Expr -> Expr -> (Integer -> Integer -> Integer) -> (Expr, Expr, Maybe Integer)
parseIIIExpr expr1 expr2 op =
    let newExpr1 = parseExpr expr1 in
    let newExpr2 = parseExpr expr2 in
    let intV1 = getIntValue newExpr1 in
    let intV2 = getIntValue newExpr2 in
    case (intV1, intV2) of
        (Just i1, Just i2) -> (newExpr1, newExpr2, Just $ op i1 i2)
        _ -> (newExpr1, newExpr2, Nothing)

-- Needed to prevent div / mod by zero.
parseSafeIIIExpr :: Expr -> Expr -> (Integer -> Integer -> Integer) -> (Expr, Expr, Maybe Integer)
parseSafeIIIExpr expr1 expr2 op =
    let newExpr1 = parseExpr expr1 in
    let newExpr2 = parseExpr expr2 in
    let intV1 = getIntValue newExpr1 in
    let intV2 = getIntValue newExpr2 in
    case (intV1, intV2) of
        (Just _, Just 0) -> (newExpr1, newExpr2, Nothing)
        (Just i1, Just i2) -> (newExpr1, newExpr2, Just $ op i1 i2)
        _ -> (newExpr1, newExpr2, Nothing)

parseXXBRelExpr :: (Eq a) => RelOp -> BNFC'Position -> Expr -> Expr ->
    (Expr -> Maybe a) -> (a -> a -> Bool) -> Expr
parseXXBRelExpr relOp pos expr1 expr2 getValue op =
    let newExpr1 = parseExpr expr1 in
    let newExpr2 = parseExpr expr2 in
    let v1 = getValue newExpr1 in
    let v2 = getValue newExpr2 in
    case (v1, v2) of
        (Just x, Just y) -> getBoolExpr pos $ op x y
        _ -> ERel pos newExpr1 relOp newExpr2


parseExpr :: Expr -> Expr
parseExpr (ELitInt pos n) = (ELitInt pos n)
parseExpr (ELitTrue pos) = (ELitTrue pos)
parseExpr (ELitFalse pos) = (ELitFalse pos)
parseExpr (EString pos s) = (EString pos s)
parseExpr (EVar pos ident) = (EVar pos ident)

parseExpr (EAccFld pos expr ident) = EAccFld pos (parseExpr expr) ident

parseExpr (EArrGet pos expr1 expr2) = EArrGet pos (parseExpr expr1) (parseExpr expr2)

parseExpr (ENewArr pos arrType expr) = ENewArr pos arrType $ parseExpr expr


parseExpr (ENot pos expr) =
    let newExpr = parseExpr expr in
    let boolV = getBoolValue $ newExpr in
    case boolV of
        Just t -> getBoolExpr pos (not t)
        Nothing -> ENot pos newExpr

parseExpr (EAnd pos expr1 expr2) =
    let parsed = parseBBBExpr expr1 expr2 (&&) in
    case parsed of
        (_, _, Just t) -> getBoolExpr pos t
        (newExpr1, newExpr2, Nothing) -> EAnd pos newExpr1 newExpr2

parseExpr (EOr pos expr1 expr2) =
    let parsed = parseBBBExpr expr1 expr2 (||) in
    case parsed of
        (_, _, Just t) -> getBoolExpr pos t
        (newExpr1, newExpr2, Nothing) -> EOr pos newExpr1 newExpr2


parseExpr (ERel pos1 expr1 (EQU pos2) expr2) =
    let newExpr1 = parseExpr expr1 in
    let newExpr2 = parseExpr expr2 in
    if isBoolLit newExpr1 && isBoolLit newExpr2 then
        parseXXBRelExpr (EQU pos2) pos1 newExpr1 newExpr2 getBoolValue (==)
    else if isIntLit newExpr1 && isIntLit newExpr2 then
        parseXXBRelExpr (EQU pos2) pos1 newExpr1 newExpr2 getIntValue (==)
    else
        ERel pos1 expr1 (EQU pos2) expr2

parseExpr (ERel pos1 expr1 (NE pos2) expr2) =
    let newExpr1 = parseExpr expr1 in
    let newExpr2 = parseExpr expr2 in
    if isBoolLit newExpr1 && isBoolLit newExpr2 then
        parseXXBRelExpr (NE pos2) pos1 newExpr1 newExpr2 getBoolValue (/=)
    else if isIntLit newExpr1 && isIntLit newExpr2 then
        parseXXBRelExpr (NE pos2) pos1 newExpr1 newExpr2 getIntValue (/=)
    else
        ERel pos1 expr1 (NE pos2) expr2

parseExpr (ERel pos expr1 op expr2) =
    parseXXBRelExpr op pos expr1 expr2 getIntValue parsedOp where
        parsedOp = case op of
            LTH _ -> (<)
            LE _ -> (<=)
            GTH _ -> (>)
            GE _ -> (>=)
            -- Already parsed, won't happen (needed to avoid compilation warning).
            EQU _ -> (==)
            NE _ -> (/=)


parseExpr (ENeg pos expr) =
    let newExpr = parseExpr expr in
    let intV = getIntValue $ newExpr in
    case intV of
        Just n -> getIntExpr (-n) pos
        Nothing -> ENeg pos newExpr

parseExpr (EAdd pos expr1 op expr2) =
    let parsed = parseIIIExpr expr1 expr2 parsedOp where
        parsedOp = case op of
            Plus _ -> (+)
            Minus _ -> (-)
    in case parsed of
        (_, _, Just n) -> getIntExpr n pos
        (newExpr1, newExpr2, Nothing) -> EAdd pos newExpr1 op newExpr2

parseExpr (EMul pos expr1 op expr2) =
    let parsed = parseFun expr1 expr2 parsedOp where
        (parsedOp, parseFun) = case op of
            Times _ -> ((*), parseIIIExpr)
            Div _ -> (div, parseSafeIIIExpr)
            Mod _ -> (rem, parseSafeIIIExpr)
    in case parsed of
        (_, _, Just n) -> getIntExpr n pos
        (newExpr1, newExpr2, Nothing) -> EMul pos newExpr1 op newExpr2


parseExpr (EApp pos ident args) =
    let newArgs = map parseExpr args in
    EApp pos ident newArgs

parseExpr expr = expr


parseItem :: Item -> Item
parseItem (SNoInit pos ident) = SNoInit pos ident
parseItem (SInit pos ident expr) = SInit pos ident $ parseExpr expr


wrapStmtWithBlock :: (Stmt, Bool) -> (Stmt, Bool)
wrapStmtWithBlock ((SBStmt pos block), b) = ((SBStmt pos block), b)
wrapStmtWithBlock (stmt, b) =
    let pos = hasPosition stmt in (SBStmt pos (Block pos [stmt]), b)

-- Bool represents whether `return` keyword occured.
parseStmt :: Stmt -> (Stmt, Bool)
parseStmt (SExp pos expr) = (SExp pos $ parseExpr expr, False)

parseStmt (SBStmt pos block) =
    let (parsed, returnNow) = parseBlock block in
    (SBStmt pos parsed, returnNow)

parseStmt (SDecl pos declType items) = (SDecl pos declType $ map parseItem items, False)

parseStmt (SAss pos expr1 expr2) = (SAss pos (parseExpr expr1) (parseExpr expr2), False)

parseStmt (SIncr pos expr1) = (SIncr pos $ parseExpr expr1, False)

parseStmt (SDecr pos expr1) = (SDecr pos $ parseExpr expr1, False)

parseStmt (SRet pos expr) = (SRet pos $ parseExpr expr, True)

-- Here we optimize possibly useless if branches and wrap one-line statements with blocks.
parseStmt (SCond pos expr stmt) =
    let (newStmt, returnNow) = wrapStmtWithBlock $ parseStmt stmt in
    let newExpr = parseExpr expr in
    let boolV = getBoolValue newExpr in
    case boolV of
        Just True -> (newStmt, returnNow)
        Just False -> (SEmpty pos, False)
        Nothing -> (SCond pos newExpr newStmt, False)

parseStmt (SCondElse pos expr stmt1 stmt2) =
    let (newStmt1, returnNow1) = wrapStmtWithBlock $ parseStmt stmt1 in
    let (newStmt2, returnNow2) = wrapStmtWithBlock $ parseStmt stmt2 in
    let newExpr = parseExpr expr in
    let boolV = getBoolValue newExpr in
    case boolV of
        Just True -> (newStmt1, returnNow1)
        Just False -> (newStmt2, returnNow2)
        Nothing -> (SCondElse pos newExpr newStmt1 newStmt2, returnNow1 && returnNow2)

parseStmt (SWhile pos expr stmt) =
    let (newStmt, returnNow) = wrapStmtWithBlock $ parseStmt stmt in
    let newExpr = parseExpr expr in
    let boolV = getBoolValue newExpr in
    case boolV of
        Just False -> (SEmpty pos, False)
        Just True -> (SWhile pos newExpr newStmt, returnNow)
        _ -> (SWhile pos newExpr newStmt, False)

parseStmt (SFor pos varType ident expr stmt) =
    let newExpr = parseExpr expr in
    let (newStmt, returnNow) = wrapStmtWithBlock $ parseStmt stmt in
    (SFor pos varType ident newExpr newStmt, returnNow)

parseStmt stmt = (stmt, False)


parseToFirstReturn :: Bool -> [Stmt] -> [(Stmt, Bool)] -> ([Stmt], Bool)
parseToFirstReturn ret res [] = (reverse res, ret)
parseToFirstReturn _ res ((stmt, True) : _) = parseToFirstReturn True (stmt : res) []
parseToFirstReturn ret res ((stmt, False) : stmts) = parseToFirstReturn ret (stmt : res) stmts

parseBlock :: Block -> (Block, Bool)
parseBlock (Block pos stmts) =
    let (parsed, returnNow) = parseToFirstReturn False [] $ map parseStmt stmts in
    (Block pos parsed, returnNow)


addVoidReturn :: Type -> Block -> Block
addVoidReturn (TVoid _) (Block pos stmts) = Block pos (stmts ++ [SVRet pos])
addVoidReturn _ block = block

parseTopdef :: TopDef -> TopDef
parseTopdef (FnDef pos funType ident args block) =
    -- Synctatic sugar lets void function avoid `return` keyword.
    -- Therefore we add it in case it is missing.
    let (parsed, _) = parseBlock $ addVoidReturn funType block in
    (FnDef pos funType ident args parsed)

-- There is nothing to simplify in the struct definition.
parseTopdef topdef = topdef


parseProgram :: Program -> Program
parseProgram (Program pos topdefs) = Program pos $ map parseTopdef topdefs
