module TypeInference where

import Ast
import FmlError
import Environment
import Control.Monad.Trans.Except
import Control.Monad.State ( MonadTrans(lift), State )
import FmlType

data TypeInfo = TypeInfo
    { typeInfo :: FmlType
    , parseInfo :: Ast.ParseInfo
    }
    deriving (Show)

type TypedProgram = Ast.Program TypeInfo
type TypedAst = Ast.Ast TypeInfo
type TypedExpr = Ast.Expr TypeInfo
type FmlSemanticStep f t = f -> ExceptT FmlError (State Environment) t -- TODO rename this

getExprType :: TypedExpr -> FmlType
getExprType texpr = typeInfo $ getMetaExpr texpr

addTypeProgramOrFail :: FmlSemanticStep Ast.ParsedProgram TypedProgram
addTypeProgramOrFail (Ast.FmlCode info stmts) = do
    tStmts <- addTypesOrFail stmts
    return $ Ast.FmlCode (TypeInfo Unit info) tStmts

addTypesOrFail :: FmlSemanticStep [Ast.ParsedAst] [TypedAst]
addTypesOrFail = mapM addTypeAstOrFail

addTypeAstOrFail :: FmlSemanticStep Ast.ParsedAst TypedAst
addTypeAstOrFail (Ast.ConstDecl info name expr) = do
    texpr <- addTypeExprOrFail expr
    let t = getExprType texpr
    lift $ setInEnv name t
    return $ Ast.ConstDecl (TypeInfo t info) name texpr
addTypeAstOrFail (Ast.Let info name expr) = do
    texpr <- addTypeExprOrFail expr
    let t = getExprType texpr
    lift $ setInEnv name t
    return $ Ast.Let (TypeInfo t info) name texpr
addTypeAstOrFail (Ast.Assign info name expr) = do
    texpr <- addTypeExprOrFail expr
    let t = getExprType texpr
    return $ Ast.Assign (TypeInfo t info) name texpr
addTypeAstOrFail (Ast.While info condition block) = do
    tcondition <- addTypeExprOrFail condition
    tblock <- mapM addTypeAstOrFail block
    let t = Unit
    return $ Ast.While (TypeInfo t info) tcondition tblock
addTypeAstOrFail (Ast.If info condition block) = do
    tcondition <- addTypeExprOrFail condition
    tblock <- mapM addTypeAstOrFail block
    let t = Unit
    return $ Ast.If (TypeInfo t info) tcondition tblock
addTypeAstOrFail (Ast.ExprStmt info expr) = do
    texpr <- addTypeExprOrFail expr
    let t = Unit
    return $ Ast.ExprStmt (TypeInfo t info) texpr
addTypeAstOrFail (Ast.ReturnStmt info) = do
    let t = Unit
    return $ Ast.ReturnStmt (TypeInfo t info)

addTypeExprOrFail :: FmlSemanticStep Ast.ParsedExpr TypedExpr
addTypeExprOrFail (Ast.IntConst info value) = do
    return $ Ast.IntConst (TypeInfo (Primitive FmlInt64) info) value
addTypeExprOrFail (Ast.BoolConst info value) = do
    return $ Ast.BoolConst (TypeInfo (Primitive FmlBoolean) info) value
addTypeExprOrFail (Ast.FloatConst info value) = do
    return $ Ast.FloatConst (TypeInfo (Primitive FmlFloat64) info) value
addTypeExprOrFail (Ast.PrefixExpr info op rhs) = do
    trhs <- addTypeExprOrFail rhs
    let t = typeInfo $ getMetaExpr trhs
    return $ Ast.PrefixExpr (TypeInfo t info) op trhs
addTypeExprOrFail (Ast.Var info name) = do
    t <- getInEnvOrFail name (FmlError.VarNotDef name info)
    return $ Ast.Var (TypeInfo t info) name
addTypeExprOrFail (Ast.InfixExpr info lhs op rhs) = do
    tlhs <- addTypeExprOrFail lhs
    trhs <- addTypeExprOrFail rhs
    let typeOfLhs = getExprType tlhs
    let t = getInfixOpType typeOfLhs op
    return $ Ast.InfixExpr (TypeInfo t info) tlhs op trhs

getInfixOpType :: FmlType -> InfixOp -> FmlType
getInfixOpType operandType op = case op of
    Ast.InfixAdd -> operandType
    Ast.InfixSub -> operandType
    Ast.InfixMult -> operandType
    Ast.InfixDiv -> operandType
    Ast.InfixMod -> operandType
    Ast.InfixEq -> Primitive FmlBoolean
    Ast.InfixNeq -> Primitive FmlBoolean
    Ast.InfixLt -> Primitive FmlBoolean
    Ast.InfixGt -> Primitive FmlBoolean
    Ast.InfixLe -> Primitive FmlBoolean
    Ast.InfixGe -> Primitive FmlBoolean
    Ast.InfixAnd -> Primitive FmlBoolean
    Ast.InfixOr -> Primitive FmlBoolean
