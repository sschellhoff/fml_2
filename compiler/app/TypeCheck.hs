module TypeCheck where
import Ast
import TypeInference
import FmlError
import FmlType (FmlType (..), PrimitiveType (..))
import Control.Monad.Trans.Except
import Control.Monad.State
import Environment

typeCheckProgramOrFail :: FmlSemanticStep TypedProgram TypedProgram
typeCheckProgramOrFail (Ast.FmlCode info stmts) = do
    _stmts <- typeCheckStmtsOrFail stmts
    ExceptT $ return $ Right $ Ast.FmlCode info _stmts

typeCheckStmtsOrFail :: FmlSemanticStep [TypedAst] [TypedAst]
typeCheckStmtsOrFail = mapM typeCheckAstOrFail

typeCheckAstOrFail :: FmlSemanticStep TypedAst TypedAst
typeCheckAstOrFail (Ast.ConstDecl info name expr) = do
    expected <- getInEnvOrFail name (FmlError.VarNotDef name $ parseInfo info)
    _expr <- typeCheckExprkOrFail expr
    let exprType = typeInfo $ getMetaExpr _expr
    let actualType = typeInfo info
    _ <- typeCheckAssignment expected exprType info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.ConstDecl info name _expr
        else Left $ TypeMismatch expected actualType (parseInfo info)   
typeCheckAstOrFail (Ast.Let info name expr) = do
    expected <- getInEnvOrFail name (FmlError.VarNotDef name $ parseInfo info)
    _expr <- typeCheckExprkOrFail expr
    let exprType = typeInfo $ getMetaExpr _expr
    let actualType = typeInfo info
    _ <- typeCheckAssignment expected exprType info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.Let info name _expr
        else Left $ TypeMismatch expected actualType (parseInfo info)  
typeCheckAstOrFail (Ast.Assign info name expr) = do -- TODO check if trying to reassign const
    expected <- getInEnvOrFail name (FmlError.VarNotDef name $ parseInfo info)
    _expr <- typeCheckExprkOrFail expr
    let exprType = typeInfo $ getMetaExpr _expr
    let actualType = typeInfo info
    _ <- typeCheckAssignment expected exprType info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.Assign info name _expr
        else Left $ TypeMismatch expected actualType (parseInfo info)  
typeCheckAstOrFail (Ast.While info condition block) = do
    _condition <- typeCheckExprkOrFail condition
    let conditionType = typeInfo $ getMetaExpr _condition
    _block <- mapM typeCheckAstOrFail block
    let actualType = typeInfo info
    _ <- typeCheckCondition conditionType info
    ExceptT $ return $ if actualType == Unit
        then Right $ Ast.While info _condition _block
        else Left $ TypeMismatch Unit actualType (parseInfo info)  
typeCheckAstOrFail (Ast.ExprStmt info expr) = do
    _expr <- typeCheckExprkOrFail expr
    let actualType = typeInfo info
    ExceptT $ return $ if actualType == Unit
        then Right $ Ast.ExprStmt info _expr
        else Left $ TypeMismatch Unit actualType (parseInfo info)  

typeCheckExprkOrFail :: FmlSemanticStep TypedExpr TypedExpr
typeCheckExprkOrFail (Ast.IntConst info value) = do
    let expected = Primitive FmlInt64
    let actualType = typeInfo info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.IntConst info value
        else Left $ TypeMismatch expected actualType (parseInfo info)
typeCheckExprkOrFail (Ast.BoolConst info value) = do
    let expected = Primitive FmlBoolean
    let actualType = typeInfo info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.BoolConst info value
        else Left $ TypeMismatch expected actualType (parseInfo info)
typeCheckExprkOrFail (Ast.FloatConst info value) = do
    let expected = Primitive FmlFloat64
    let actualType = typeInfo info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.FloatConst info value
        else Left $ TypeMismatch expected actualType (parseInfo info)
typeCheckExprkOrFail (Ast.PrefixExpr info op rhs) = do
    _rhs <- typeCheckExprkOrFail rhs
    let actualRhsType = typeInfo $ getMetaExpr _rhs
    expected <- typeCheckPrefixOp actualRhsType op info
    let actualType = typeInfo info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.PrefixExpr info op _rhs
        else Left $ TypeMismatch expected actualType (parseInfo info)
typeCheckExprkOrFail (Ast.Var info name) = do
    expected <- getInEnvOrFail name (FmlError.VarNotDef name  $ parseInfo info)
    let actualType = typeInfo info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.Var info name
        else Left $ TypeMismatch expected actualType (parseInfo info)
typeCheckExprkOrFail (Ast.InfixExpr info lhs op rhs) = do
    _lhs <- typeCheckExprkOrFail lhs
    _rhs <- typeCheckExprkOrFail rhs
    let actualLhsType = typeInfo $ getMetaExpr _lhs
    let actualRhsType = typeInfo $ getMetaExpr _rhs
    expected <- typeCheckInfixOp actualLhsType actualRhsType op info
    let actualType = typeInfo info
    ExceptT $ return $ if actualType == expected
        then Right $ Ast.InfixExpr info _lhs op _rhs
        else Left $ TypeMismatch expected actualType (parseInfo info)

typeCheckCondition :: FmlType -> TypeInfo -> ExceptT FmlError (State Environment) FmlType
typeCheckCondition condition info = do
    if condition == Primitive FmlBoolean
        then ExceptT $ return $ Right condition
        else ExceptT $ return $ Left $ InvalidConditionType condition (parseInfo info)

typeCheckAssignment :: FmlType -> FmlType -> TypeInfo -> ExceptT FmlError (State Environment) FmlType
typeCheckAssignment lhs rhs info = do
    if lhs == rhs
        then ExceptT $ return $ Right lhs
        else ExceptT $ return $ Left $ AssignmentTypeMismatch lhs rhs (parseInfo info)

typeCheckPrefixOp :: FmlType -> PrefixOp -> TypeInfo -> ExceptT FmlError (State Environment) FmlType
typeCheckPrefixOp rhs op info = do
    let test | op == OpNeg && rhs == Primitive FmlInt64 || rhs == Primitive FmlFloat64 = ExceptT $ return $ Right rhs
             | op == OpNot && rhs == Primitive FmlBoolean = ExceptT $ return $ Right rhs
             | otherwise = ExceptT $ return $ Left $ PrefixOperandMismatch op rhs (parseInfo info)
    test

typeCheckInfixOp :: FmlType -> FmlType -> InfixOp -> TypeInfo -> ExceptT FmlError (State Environment) FmlType
typeCheckInfixOp lhs rhs op info = do
    let err = ExceptT $ return $ Left $ InfixOperandMismatch lhs op rhs (parseInfo info)
    let test | lhs /= rhs = err
             | op == InfixAdd && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right lhs
             | op == InfixSub && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right lhs
             | op == InfixMult && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right lhs
             | op == InfixDiv && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right lhs
             | op == InfixMod && (lhs == Primitive FmlInt64) = ExceptT $ return $ Right lhs
             | op == InfixEq && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64 || lhs == Primitive FmlBoolean) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | op == InfixNeq && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64 || lhs == Primitive FmlBoolean) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | op == InfixLt && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | op == InfixGt && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | op == InfixLe && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | op == InfixGe && (lhs == Primitive FmlInt64 || lhs == Primitive FmlFloat64) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | op == InfixAnd && (lhs == Primitive FmlBoolean) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | op == InfixOr && (lhs == Primitive FmlBoolean) = ExceptT $ return $ Right $ Primitive FmlBoolean
             | otherwise = err
    test