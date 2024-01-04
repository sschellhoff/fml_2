module Ast where

type ParsedAst = Ast ()
type ParsedExpr = Expr ()

data Ast meta
    = ConstDecl meta String (Expr meta)
    | Let meta String (Expr meta)
    | Assign meta String (Expr meta)
    | ExprStmt meta (Expr meta)
    deriving (Show)

data Expr meta
    = IntConst meta Integer
    | BoolConst meta Bool
    | FloatConst meta Double
    | NULL meta
    | InfixExpr meta (Expr meta) InfixOp (Expr meta)
    | PrefixExpr meta PrefixOp (Expr meta)
    deriving (Show)

toParsedInfixExpr :: InfixOp -> Expr () -> Expr () -> Expr ()
toParsedInfixExpr op lhs = InfixExpr () lhs op

toParsedPrefixExpr :: PrefixOp -> Expr () -> Expr ()
toParsedPrefixExpr = PrefixExpr ()

data InfixOp = InfixAdd | InfixSub | InfixMult | InfixDiv | InfixMod | InfixEq | InfixLt | InfixGt | InfixAnd | InfixOr deriving (Show)
data PrefixOp = OpNeg | OpNot deriving (Show)