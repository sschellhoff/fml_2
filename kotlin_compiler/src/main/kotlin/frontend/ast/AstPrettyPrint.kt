package de.sschellhoff.frontend.ast

fun Ast.prettyPrint(): String {
    return when (this) {
        is Ast.Program -> TODO()
        is Ast.Block -> TODO()
        is Ast.Parameter -> TODO()
        is Ast.Type -> TODO()
        is Ast.Expression.IntegerLiteral -> value.toString()
        is Ast.Expression.Identifier -> name
        is Ast.Expression.Unary -> "($operator${operand.prettyPrint()})"
        is Ast.Expression.Binary -> "(${left.prettyPrint()} $operator ${right.prettyPrint()})"
        is Ast.Expression.BoolLiteral -> TODO()
        is Ast.Expression.Call -> TODO()
        is Ast.Statement.Assignment -> TODO()
        is Ast.Statement.Const -> TODO()
        is Ast.Statement.ExpressionStatement -> TODO()
        is Ast.Statement.Function -> TODO()
        is Ast.Statement.If -> TODO()
        is Ast.Statement.Let -> TODO()
        is Ast.Statement.Return -> TODO()
        is Ast.Statement.While -> TODO()
    }
}