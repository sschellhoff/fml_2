package de.sschellhoff.frontend.parser

import de.sschellhoff.frontend.Token
import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.ast.BinaryOperator
import de.sschellhoff.frontend.ast.UnaryOperator
import de.sschellhoff.frontend.lexer.BufferedLexer
import de.sschellhoff.frontend.unexpectedToken

class ExpressionParser(private val lexer: BufferedLexer) : Parser<Ast.Expression> {
    override fun parse(): Ast.Expression = parseExpression(0)

    override fun isDone(): Boolean = lexer.getNext() is Token.Eof

    private fun parseExpression(precedence: Int): Ast.Expression {
        val lhsParser = unaryParser[lexer.peekNext()::class] ?: ::parseAtomic
        var lhs = lhsParser()
        var token = lexer.peekNext()

        while ((precedenceOf[token::class] ?: 0) > precedence) {
            val parser = infixParser[token::class]
            checkNotNull(parser) { "No infix parser for token ${token.javaClass.name}" }
            lhs = parser(lhs)
            token = lexer.peekNext()
        }

        return lhs
    }

    private fun parseUnary(): Ast.Expression {
        val operatorToken = lexer.getNext()
        val operator = operatorToken.toUnaryOperator()
        val operandParser = unaryParser[lexer.peekNext()::class] ?: ::parseAtomic
        val operand = operandParser()
        return Ast.Expression.Unary(operator = operator, operand = operand, position = operatorToken.position)
    }

    private fun parseParenthesized(): Ast.Expression {
        require(lexer.getNext() is Token.LParen)
        val expression = parse()
        require(lexer.getNext() is Token.RParen)
        return expression
    }

    private fun parseBinary(lhs: Ast.Expression): Ast.Expression {
        val operatorToken = lexer.getNext()
        val operator = operatorToken.toBinaryOperator()
        val rhs = parseExpression(precedenceOf[operatorToken::class]!!)

        return Ast.Expression.Binary(left = lhs, operator = operator, right = rhs, position = operatorToken.position)
    }

    private fun parseAtomic(): Ast.Expression = when (val token = lexer.getNext()) {
        is Token.IntegerLiteral -> Ast.Expression.IntegerLiteral(value = token.value, position = token.position)
        is Token.Identifier -> Ast.Expression.Identifier(name = token.name, position = token.position)
        is Token.BoolLiteral -> Ast.Expression.BoolLiteral(value = token.value, position = token.position)
        else -> unexpectedToken(token)
    }

    private val unaryParser = mapOf(
        Token.Minus::class to ::parseUnary,
        Token.Bang::class to ::parseUnary,
        Token.LParen::class to ::parseParenthesized
    )
    private val infixParser = mapOf(
        Token.Or::class to ::parseBinary,
        Token.And::class to ::parseBinary,
        Token.Eq::class to ::parseBinary,
        Token.NotEq::class to ::parseBinary,
        Token.Less::class to ::parseBinary,
        Token.LessEq::class to ::parseBinary,
        Token.Greater::class to ::parseBinary,
        Token.GreaterEq::class to ::parseBinary,
        Token.Plus::class to ::parseBinary,
        Token.Minus::class to ::parseBinary,
        Token.Asterisk::class to ::parseBinary,
        Token.Slash::class to ::parseBinary,
        Token.Percent::class to ::parseBinary,
    )
    private val precedenceOf = mapOf(
        Token.Or::class to 1,
        Token.And::class to 2,
        Token.Eq::class to 3,
        Token.NotEq::class to 3,
        Token.Less::class to 4,
        Token.LessEq::class to 4,
        Token.Greater::class to 4,
        Token.GreaterEq::class to 4,
        Token.Plus::class to 5,
        Token.Minus::class to 5,
        Token.Asterisk::class to 6,
        Token.Slash::class to 6,
        Token.Percent::class to 6,
    )
}

private fun Token.toBinaryOperator(): BinaryOperator = when (this) {
    is Token.Or -> BinaryOperator.OR
    is Token.And -> BinaryOperator.AND
    is Token.Eq -> BinaryOperator.EQ
    is Token.NotEq -> BinaryOperator.NEQ
    is Token.Less -> BinaryOperator.LT
    is Token.LessEq -> BinaryOperator.LTE
    is Token.Greater -> BinaryOperator.GT
    is Token.GreaterEq -> BinaryOperator.GTE
    is Token.Plus -> BinaryOperator.PLUS
    is Token.Minus -> BinaryOperator.MINUS
    is Token.Asterisk -> BinaryOperator.MULTIPLY
    is Token.Slash -> BinaryOperator.DIVIDE
    is Token.Percent -> BinaryOperator.MODULO
    else -> throw IllegalArgumentException("Token $this is not a binary operator")
}

private fun Token.toUnaryOperator(): UnaryOperator = when (this) {
    is Token.Minus -> UnaryOperator.MINUS
    is Token.Bang -> UnaryOperator.NOT
    else -> throw IllegalArgumentException("Token $this is not a unary operator")
}