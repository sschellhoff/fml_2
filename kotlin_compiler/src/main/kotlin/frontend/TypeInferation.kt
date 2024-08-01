package de.sschellhoff.frontend

import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.ast.BinaryOperator
import de.sschellhoff.frontend.ast.UnaryOperator
import de.sschellhoff.frontend.fmltypes.Environment
import de.sschellhoff.frontend.fmltypes.FmlType

class TypeInferation {
    private var currentEnvironment: Environment = Environment()

    fun inferTypes(program: Ast.Program) {
        currentEnvironment = program.environment
        program.statements.forEach { statement ->
            val type = inferType(statement)
            check(type == FmlType.Unit) { "Top level statements must have type Unit" }
        }
    }

    fun inferType(statement: Ast.Statement): FmlType {
        return when (statement) {
            is Ast.Statement.Let -> {
                val type = statement.type?.toFmlType()
                val name = statement.name
                val expressionType = inferType(statement.value)
                check(type == null || type == expressionType) { "Type mismatch in let statement" }
                currentEnvironment.declare(name, expressionType, false)
                FmlType.Unit
            }
            is Ast.Statement.Const -> {
                val type = statement.type?.toFmlType()
                val name = statement.name
                val expressionType = inferType(statement.value)
                check(type == null || type == expressionType) { "Type mismatch in let statement" }
                currentEnvironment.declare(name, expressionType, true)
                FmlType.Unit
            }
            is Ast.Statement.While -> {
                val conditionType = inferType(statement.condition)
                check(conditionType == FmlType.Bool) { "Type mismatch in while condition" }
                return inferType(statement.block)
            }
        }
    }

    private fun inferType(block: Ast.Block): FmlType {
        val oldEnvironment = currentEnvironment
        currentEnvironment = block.environment
        block.statements.forEach { statement ->
            inferType(statement)
        }
        currentEnvironment = oldEnvironment
        return FmlType.Unit
    }

    fun inferType(expression: Ast.Expression): FmlType {
        return when (expression) {
            is Ast.Expression.IntegerLiteral -> FmlType.Int
            is Ast.Expression.BoolLiteral -> FmlType.Bool
            is Ast.Expression.Unary -> {
                val operandType = inferType(expression.operand)
                when (expression.operator) {
                    UnaryOperator.MINUS -> {
                        check(operandType == FmlType.Int) { "Type mismatch in unary minus" }
                        FmlType.Int
                    }
                    UnaryOperator.NOT -> {
                        check(operandType == FmlType.Bool) { "Type mismatch in unary not" }
                        FmlType.Bool
                    }
                }
            }
            is Ast.Expression.Binary -> {
                val leftType = inferType(expression.left)
                val rightType = inferType(expression.right)
                when (expression.operator) {
                    BinaryOperator.PLUS, BinaryOperator.MINUS, BinaryOperator.MULTIPLY, BinaryOperator.DIVIDE, BinaryOperator.MODULO -> {
                        check(leftType == FmlType.Int && rightType == FmlType.Int) { "Type mismatch in arithmetic operation" }
                        FmlType.Int
                    }
                    BinaryOperator.EQ, BinaryOperator.NEQ -> {
                        check(leftType == rightType) { "Type mismatch in comparison operation" }
                        FmlType.Bool
                    }
                    BinaryOperator.LT, BinaryOperator.GT, BinaryOperator.LTE, BinaryOperator.GTE -> {
                        check(leftType == FmlType.Int && rightType == FmlType.Int) { "Type mismatch in comparison operation" }
                        FmlType.Bool
                    }
                    BinaryOperator.AND, BinaryOperator.OR -> {
                        check(leftType == FmlType.Bool && rightType == FmlType.Bool) { "Type mismatch in logical operation" }
                        FmlType.Bool
                    }
                }
            }
            is Ast.Expression.Identifier -> {
                val type = currentEnvironment.get(expression.name)
                check(type != null) { "Variable ${expression.name} not declared" }
                type
            }
        }
    }

    private fun Ast.Type.toFmlType(): FmlType {
        return when (name) {
            "Int" -> FmlType.Int
            "Bool" -> FmlType.Bool
            else -> throw IllegalArgumentException("Unknown type $name")
        }
    }
}