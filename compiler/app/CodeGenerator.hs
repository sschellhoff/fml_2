module CodeGenerator where
import qualified Ast
import TypeInference (FmlSemanticStep, TypeInfo (parseInfo))
import qualified TypeInference
import qualified Environment
import FmlConstant (FmlConstant (ConstFloat64, ConstBool, ConstInt64))
import Control.Monad.State ( MonadTrans(lift))
import qualified FmlError

data OpCode = LABEL String
            | OP_RETURN
            | OP_ADD Int Int Int
            | OP_SUB Int Int Int
            | OP_MULT Int Int Int
            | OP_DIV Int Int Int
            | OP_MOD Int Int Int
            | OP_AND Int Int Int
            | OP_OR Int Int Int
            | OP_EQ Int Int Int
            | OP_LT Int Int Int
            | OP_GT Int Int Int
            | OP_NEG Int Int
            | OP_JUMP String
            | OP_JUMPF String Int
            | OP_MOVE Int Int
            | OP_LOADC Int Int
            | OP_RESERVE Int
            deriving (Show)

data ByteCode = ByteCode
    { code :: [OpCode]
    , constants :: [FmlConstant]
    }

instance Show ByteCode where
    show (ByteCode _code _constants) = "data:\n" ++ showConstants _constants ++ "\ncode:\n" ++ showCode _code

showConstants :: [FmlConstant] -> String
showConstants _constants = unlines $ map show _constants

showCode :: [OpCode] -> String
showCode _code = unlines $ map show _code

noByteCode :: ByteCode
noByteCode = ByteCode [] []

toByteCode :: OpCode -> ByteCode
toByteCode opCode = ByteCode [opCode] []

appendByteCode :: ByteCode -> ByteCode -> ByteCode
appendByteCode a b = ByteCode (code a ++ code b) []

concatByteCode :: [ByteCode] -> ByteCode
concatByteCode bs = ByteCode (foldr c [] bs) []
    where c a b = code a ++ b

generateCodeProgram :: FmlSemanticStep TypeInference.TypedProgram ByteCode -- TODO add constants to bytecode
generateCodeProgram (Ast.FmlCode _ stmts) = do
    _stmts <- generateStmts stmts
    numberOfRegisters <- lift Environment.numberOfUsedRegisters
    _constants <- lift Environment.getConstants
    return $ (appendByteCode (toByteCode $ OP_RESERVE numberOfRegisters) _stmts) { constants = _constants}

generateStmts :: FmlSemanticStep [TypeInference.TypedAst] ByteCode
generateStmts stmts = do
    _stmts <- mapM generateCodeAst stmts
    return $ concatByteCode _stmts

generateCodeAst :: FmlSemanticStep TypeInference.TypedAst ByteCode
generateCodeAst (Ast.ConstDecl info name expr) = do
    varSlot <- Environment.getRegisterOrFail name (FmlError.VarNotDef name $ parseInfo info)
    (stackTop, _expr) <- generateCodeExpr expr
    _ <- Environment.pop stackTop $ FmlError.IllegalStatePop $ parseInfo info
    return $ appendByteCode _expr (toByteCode $ OP_MOVE varSlot stackTop)
generateCodeAst (Ast.Let info name expr) = do
    varSlot <- Environment.getRegisterOrFail name (FmlError.VarNotDef name $ parseInfo info)
    (stackTop, _expr) <- generateCodeExpr expr
    _ <- Environment.pop stackTop $ FmlError.IllegalStatePop $ parseInfo info
    return $ appendByteCode _expr (toByteCode $ OP_MOVE varSlot stackTop)
generateCodeAst (Ast.Assign info name expr) = do
    varSlot <- Environment.getRegisterOrFail name (FmlError.VarNotDef name $ parseInfo info)
    (stackTop, _expr) <- generateCodeExpr expr
    _ <- Environment.pop stackTop $ FmlError.IllegalStatePop $ parseInfo info
    return $ appendByteCode _expr (toByteCode $ OP_MOVE varSlot stackTop)
generateCodeAst (Ast.While info condition block) = do
    label0 <- lift Environment.getLabelName
    label1 <- lift Environment.getLabelName
    (stackTop, _cond) <- generateCodeExpr condition
    _ <- Environment.pop stackTop $ FmlError.IllegalStatePop $ parseInfo info
    _block <- generateStmts block
    return $ concatByteCode [ toByteCode $ LABEL label0
                            , _cond
                            , toByteCode $ OP_JUMPF label1 stackTop
                            , _block
                            , toByteCode $ OP_JUMP label0
                            , toByteCode $ LABEL label1 ]
generateCodeAst (Ast.ExprStmt info expr) = do
    (stackTop, _expr) <- generateCodeExpr expr
    _ <- Environment.pop stackTop $ FmlError.IllegalStatePop $ parseInfo info
    return _expr

generateCodeExpr :: FmlSemanticStep TypeInference.TypedExpr (Int, ByteCode)
generateCodeExpr (Ast.IntConst _ value) = do
    constSlot <- lift $ Environment.addConstant $ FmlConstant.ConstInt64 value
    stackTop <- lift Environment.push
    return (stackTop, toByteCode (OP_LOADC stackTop constSlot))
generateCodeExpr (Ast.BoolConst _ value) = do
    constSlot <- lift $ Environment.addConstant $ FmlConstant.ConstBool value
    stackTop <- lift Environment.push
    return (stackTop, toByteCode (OP_LOADC stackTop constSlot))
generateCodeExpr (Ast.FloatConst _ value) = do
    constSlot <- lift $ Environment.addConstant $ FmlConstant.ConstFloat64 value
    stackTop <- lift Environment.push
    return (stackTop, toByteCode (OP_LOADC stackTop constSlot))
generateCodeExpr (Ast.PrefixExpr info op rhs) = do
    (stackTopRight, _rhs) <- generateCodeExpr rhs
    _ <- Environment.pop stackTopRight $ FmlError.IllegalStatePop $ parseInfo info
    stackTop <- lift Environment.push
    let _op = toByteCode $ case op of
            Ast.OpNeg -> OP_NEG stackTop stackTop
            Ast.OpNot -> OP_NEG stackTop stackTop
    return (stackTop, appendByteCode _rhs _op)
generateCodeExpr (Ast.Var info name) = do
    varSlot <- Environment.getRegisterOrFail name (FmlError.VarNotDef name $ parseInfo info)
    return (varSlot, noByteCode)
generateCodeExpr (Ast.InfixExpr info lhs op rhs) = do
    (stackTopLeft, _lhs) <- generateCodeExpr lhs
    (stackTopRight, _rhs) <- generateCodeExpr rhs
    _ <- Environment.pop stackTopLeft $ FmlError.IllegalStatePop $ parseInfo info
    _ <- Environment.pop stackTopRight $ FmlError.IllegalStatePop $ parseInfo info
    stackTop <- lift Environment.push
    let _op = case op of
            Ast.InfixAdd -> toByteCode $ OP_ADD stackTop stackTopLeft stackTopRight
            Ast.InfixSub -> toByteCode $ OP_SUB stackTop stackTopLeft stackTopRight
            Ast.InfixMult -> toByteCode $ OP_MULT stackTop stackTopLeft stackTopRight
            Ast.InfixDiv -> toByteCode $ OP_DIV stackTop stackTopLeft stackTopRight
            Ast.InfixMod -> toByteCode $ OP_MOD stackTop stackTopLeft stackTopRight
            Ast.InfixEq -> toByteCode $ OP_EQ stackTop stackTopLeft stackTopRight
            Ast.InfixNeq -> appendByteCode (toByteCode $ OP_EQ stackTop stackTopLeft stackTopRight) (toByteCode $ OP_NEG stackTop stackTop)
            Ast.InfixLt -> toByteCode $ OP_LT stackTop stackTopLeft stackTopRight
            Ast.InfixGt -> toByteCode $ OP_GT stackTop stackTopLeft stackTopRight
            Ast.InfixLe -> appendByteCode (toByteCode $ OP_GT stackTop stackTopLeft stackTopRight) (toByteCode $ OP_NEG stackTop stackTop)
            Ast.InfixGe -> appendByteCode (toByteCode $ OP_LT stackTop stackTopLeft stackTopRight) (toByteCode $ OP_NEG stackTop stackTop)
            Ast.InfixAnd -> toByteCode $ OP_AND stackTop stackTopLeft stackTopRight
            Ast.InfixOr -> toByteCode $ OP_OR stackTop stackTopLeft stackTopRight
    return (stackTop, appendByteCode _lhs (appendByteCode _rhs _op))