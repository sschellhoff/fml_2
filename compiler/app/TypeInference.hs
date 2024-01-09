module TypeInference where

import Ast
import FmlError
import Environment
import Control.Monad.Trans.Except
import Control.Monad.State
import FmlType

data TypeInfo = TypeInfo
    { typeInfo :: FmlType
    , parseInfo :: Ast.ParseInfo
    }
    deriving (Show)

type TypedProgram = Ast.Program TypeInfo
type TypedAst = Ast.Ast TypeInfo
type TypedExpr = Ast.Expr TypeInfo
type FmlSemanticStep f t = f -> ExceptT FmlError (State Environment) t

getExprType :: TypedExpr -> FmlType
getExprType texpr = typeInfo $ getMetaExpr texpr

addTypeProgramOrFail :: FmlSemanticStep Ast.ParsedProgram TypedProgram
addTypeProgramOrFail (Ast.FmlCode parseInfi stmts) = do
    tStmts <- addTypesOrFail stmts
    return $ Ast.FmlCode (TypeInfo Unit parseInfi) tStmts

addTypesOrFail :: FmlSemanticStep [Ast.ParsedAst] [TypedAst]
addTypesOrFail = mapM addTypeAstOrFail

addTypeAstOrFail :: FmlSemanticStep Ast.ParsedAst TypedAst
addTypeAstOrFail (Ast.ConstDecl parseInfi name expr) = do
    texpr <- addTypeExprOrFail expr
    let t = getExprType texpr
    lift $ setInEnv name t
    return $ Ast.ConstDecl (TypeInfo t parseInfi) name texpr where
addTypeAstOrFail (Ast.Let parseInfi name expr) = do
    texpr <- addTypeExprOrFail expr
    let t = getExprType texpr
    lift $ setInEnv name t
    return $ Ast.Let (TypeInfo t parseInfi) name texpr
addTypeAstOrFail (Ast.Assign parseInfi name expr) = do
    texpr <- addTypeExprOrFail expr
    let t = getExprType texpr
    return $ Ast.Assign (TypeInfo t parseInfi) name texpr
addTypeAstOrFail (Ast.While parseInfi condition block) = do
    tcondition <- addTypeExprOrFail condition
    tblock <- mapM addTypeAstOrFail block
    let t = Unit
    return $ Ast.While (TypeInfo t parseInfi) tcondition tblock
addTypeAstOrFail (Ast.ExprStmt parseInfi expr) = do
    texpr <- addTypeExprOrFail expr
    let t = Unit
    return $ Ast.ExprStmt (TypeInfo t parseInfi) texpr

addTypeExprOrFail :: FmlSemanticStep Ast.ParsedExpr TypedExpr
addTypeExprOrFail (Ast.IntConst parseInfi value) = do
    return $ Ast.IntConst (TypeInfo (Primitive FmlInt64) parseInfi) value
addTypeExprOrFail (Ast.BoolConst parseInfi value) = do
    return $ Ast.BoolConst (TypeInfo (Primitive FmlBoolean) parseInfi) value
addTypeExprOrFail (Ast.FloatConst parseInfi value) = do
    return $ Ast.FloatConst (TypeInfo (Primitive FmlFloat64) parseInfi) value
addTypeExprOrFail (Ast.PrefixExpr _ op rhs) = do
    trhs <- addTypeExprOrFail rhs
    let t = getMetaExpr trhs
    return $ Ast.PrefixExpr t op trhs
addTypeExprOrFail (Ast.Var parseInfi name) = do
    t <- getInEnvOrFail name (FmlError.VarNotDef name parseInfi)
    return $ Ast.Var (TypeInfo t parseInfi) name
addTypeExprOrFail (Ast.InfixExpr parseInfi lhs op rhs) = do
    tlhs <- addTypeExprOrFail lhs
    trhs <- addTypeExprOrFail rhs
    let typeOfLhs = getExprType tlhs
    let t = getInfixOpType typeOfLhs op
    return $ Ast.InfixExpr (TypeInfo t parseInfi) tlhs op trhs

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
