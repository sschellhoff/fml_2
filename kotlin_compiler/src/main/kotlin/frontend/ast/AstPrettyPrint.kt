package de.sschellhoff.frontend.ast

fun Ast.prettyPrint(): String {
    return when (this) {
        is Ast.Program -> statements.joinToString("\n") { it.prettyPrint() }
        is Ast.Block -> "{\n${statements.joinToString("\n") { it.prettyPrint() }}\n}"
        is Ast.Type -> ": $name"
        is Ast.Expression.IntegerLiteral -> "$value"
        is Ast.Expression.Identifier -> name
        is Ast.Expression.Unary -> "($operator${operand.prettyPrint()})"
        is Ast.Expression.Binary -> "(${left.prettyPrint()} $operator ${right.prettyPrint()})"
        is Ast.Expression.BoolLiteral -> "$value"
        is Ast.Statement.Const -> "(const $name${typeHint()} = ${value.prettyPrint()})"
        is Ast.Statement.Let -> "(let $name${typeHint()} = ${value.prettyPrint()})"
        is Ast.Statement.While -> "(while ${condition.prettyPrint()} ${block.prettyPrint()})"
    }
}

private fun Ast.Statement.Let.typeHint(): String = type?.let { ": ${it.name}" } ?: ""
private fun Ast.Statement.Const.typeHint(): String = type?.let { ": ${it.name}" } ?: ""