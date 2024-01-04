module Ast where

type ParsedAst = Ast ()
type ParsedExpr = Expr ()

type Stmts meta = [Ast meta]

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
    | NULL meta
    | InfixExpr meta (Expr meta) InfixOp (Expr meta)
    | PrefixExpr meta PrefixOp (Expr meta)
    | Var meta String
    deriving (Show)

toParsedInfixExpr :: InfixOp -> Expr () -> Expr () -> Expr ()
toParsedInfixExpr op lhs = InfixExpr () lhs op

toParsedPrefixExpr :: PrefixOp -> Expr () -> Expr ()
toParsedPrefixExpr = PrefixExpr ()

data InfixOp = InfixAdd | InfixSub | InfixMult | InfixDiv | InfixMod | InfixEq | InfixNeq | InfixLt | InfixGt | InfixLe | InfixGe | InfixAnd | InfixOr deriving (Show)
data PrefixOp = OpNeg | OpNot deriving (Show)