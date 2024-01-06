module TypeInference where

import Ast
import Foreign.C (throwErrnoIfRetryMayBlock)

data PrimitiveType = FmlBoolean | FmlInt64 | FmlFloat64
    deriving (Show)

data FmlType = Primitive PrimitiveType | Unit
    deriving (Show)

data TypeInfo = TypeInfo
    { typeInfo :: FmlType
    , parseInfo :: Ast.ParseInfo
    }
    deriving (Show)

type TypedProgram = Ast.Program TypeInfo
type TypedAst = Ast.Ast TypeInfo
type TypedExpr = Ast.Expr TypeInfo

getExprType :: TypedExpr -> FmlType
getExprType texpr = typeInfo $ getMetaExpr texpr

addTypes :: [Ast.ParsedAst] -> [TypedAst]
addTypes = map addTypeAst

addTypeProgram :: Ast.ParsedProgram -> TypedProgram
addTypeProgram (Ast.FmlCode pi stmts) = Ast.FmlCode (TypeInfo Unit pi) $ addTypes stmts

addTypeAst :: Ast.ParsedAst -> TypedAst
addTypeAst (Ast.ConstDecl pi name expr) = Ast.ConstDecl (TypeInfo t pi) name texpr where
    texpr = addTypeExpr expr
    t = getExprType texpr
addTypeAst (Ast.Let pi name expr) = Ast.Let (TypeInfo t pi) name texpr where
    texpr = addTypeExpr expr
    t = getExprType texpr
addTypeAst (Ast.Assign pi name expr) = Ast.Assign (TypeInfo t pi) name texpr where -- TODO lookup type
    texpr = addTypeExpr expr
    t = getExprType texpr
addTypeAst (Ast.While pi condition block) = Ast.While (TypeInfo t pi) tcondition tblock where
    tcondition = addTypeExpr condition
    tblock = map addTypeAst block
    t = Unit
addTypeAst (Ast.ExprStmt pi expr) = Ast.ExprStmt (TypeInfo t pi) texpr where
    texpr = addTypeExpr expr
    t = Unit

addTypeExpr :: Ast.ParsedExpr -> TypedExpr
addTypeExpr (Ast.IntConst pi value) = Ast.IntConst (TypeInfo (Primitive FmlInt64) pi) value
addTypeExpr (Ast.BoolConst pi value) = Ast.BoolConst (TypeInfo (Primitive FmlBoolean) pi) value
addTypeExpr (Ast.FloatConst pi value) = Ast.FloatConst (TypeInfo (Primitive FmlFloat64) pi) value
addTypeExpr (Ast.InfixExpr pi lhs op rhs) = Ast.InfixExpr t tlhs op trhs where
    tlhs = addTypeExpr lhs
    trhs = addTypeExpr rhs
    t = getMetaExpr tlhs -- TODO check operator and infer type
addTypeExpr (Ast.PrefixExpr _ op rhs) = Ast.PrefixExpr t op trhs where
    trhs = addTypeExpr rhs
    t = getMetaExpr trhs
addTypeExpr (Ast.Var pi name) = Ast.Var (TypeInfo  (Primitive FmlInt64) pi) name -- TODO look up type