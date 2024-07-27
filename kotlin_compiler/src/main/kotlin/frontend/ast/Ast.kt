package de.sschellhoff.frontend.ast

import de.sschellhoff.frontend.SourcePosition

sealed interface Ast {
    val position: SourcePosition

    sealed interface Statement : Ast {
        data class If(val condition: Expression, val thenBlock: Statement, val elseBlock: Statement?, override val position: SourcePosition) :
            Statement
        data class While(val condition: Expression, val block: Statement, override val position: SourcePosition) :
            Statement
        data class Let(val name: String, val type: Type?, val value: Expression, override val position: SourcePosition) :
            Statement
        data class Const(val name: String, val type: Type?, val value: Expression, override val position: SourcePosition) :
            Statement
        data class Assignment(val name: String, val value: Expression, override val position: SourcePosition) :
            Statement
        data class ExpressionStatement(val expression: Expression, override val position: SourcePosition) : Statement
        data class Return(val expression: Expression, override val position: SourcePosition) : Statement
        data class Function(val name: String, val parameters: List<Parameter>, val returnType: Type?, val body: Statement, override val position: SourcePosition) :
            Statement
    }
    sealed interface Expression : Ast {
        data class Binary(val left: Expression, val operator: BinaryOperator, val right: Expression, override val position: SourcePosition) :
            Expression
        data class Unary(val operator: UnaryOperator, val operand: Expression, override val position: SourcePosition) :
            Expression
        data class Call(val name: String, val arguments: List<Expression>, override val position: SourcePosition) :
            Expression
        data class Identifier(val name: String, override val position: SourcePosition) : Expression
        data class IntegerLiteral(val value: Int, override val position: SourcePosition) : Expression
        data class BoolLiteral(val value: Boolean, override val position: SourcePosition) : Expression
    }

    data class Program(val statements: List<Statement>, override val position: SourcePosition) : Ast
    data class Block(val statements: List<Statement>, override val position: SourcePosition) : Ast

    data class Parameter(val name: String, val type: Type, override val position: SourcePosition) : Ast
    data class Type(val name: String, override val position: SourcePosition) : Ast
}

enum class BinaryOperator {
    PLUS, MINUS, MULTIPLY, DIVIDE, MODULO, EQ, NEQ, LT, GT, LTE, GTE, AND, OR
}

enum class UnaryOperator {
    MINUS, NOT
}