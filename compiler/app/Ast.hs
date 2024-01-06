module Ast where

type ParsedAst = Ast ParseInfo
type ParsedExpr = Expr ParseInfo

type Stmts meta = [Ast meta]

data ParseInfo = ParseInfo
               { posInfo :: Int
               , filenameInfo :: String
               }
               deriving (Show)

makeParseInfo :: Int -> ParseInfo
makeParseInfo pos = ParseInfo pos "something"

data Ast meta
    = ConstDecl meta String (Expr meta)
    | Let meta String (Expr meta)
    | Assign meta String (Expr meta)
    | While meta (Expr meta) (Stmts meta)
    | ExprStmt meta (Expr meta)
    deriving (Show)

data Expr meta
    = IntConst meta Integer
    | BoolConst meta Bool
    | FloatConst meta Double
    | InfixExpr meta (Expr meta) InfixOp (Expr meta)
    | PrefixExpr meta PrefixOp (Expr meta)
    | Var meta String
    deriving (Show)

getMetaAst :: Ast meta -> meta
getMetaAst (ConstDecl m _ _) = m
getMetaAst (Let m _ _) = m
getMetaAst (Assign m _ _) = m
getMetaAst (While m _ _) = m
getMetaAst (ExprStmt m _) = m

getMetaExpr :: Expr meta -> meta
getMetaExpr (IntConst m _) = m
getMetaExpr (BoolConst m _) = m
getMetaExpr (FloatConst m _) = m
getMetaExpr (InfixExpr m _ _ _) = m
getMetaExpr (PrefixExpr m _ _) = m
getMetaExpr (Var m _) = m

toParsedInfixExpr :: InfixOp -> ParseInfo -> ParsedExpr -> ParsedExpr -> Expr ParseInfo
toParsedInfixExpr op pi lhs = InfixExpr pi lhs op

toParsedPrefixExpr :: PrefixOp -> ParseInfo -> ParsedExpr -> ParsedExpr
toParsedPrefixExpr op pi = PrefixExpr pi op

data InfixOp = InfixAdd | InfixSub | InfixMult | InfixDiv | InfixMod | InfixEq | InfixNeq | InfixLt | InfixGt | InfixLe | InfixGe | InfixAnd | InfixOr deriving (Show)
data PrefixOp = OpNeg | OpNot deriving (Show)