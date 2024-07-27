package frontend

import de.sschellhoff.frontend.*
import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.ast.BinaryOperator
import de.sschellhoff.frontend.ast.UnaryOperator
import de.sschellhoff.frontend.lexer.lexerFromText
import de.sschellhoff.frontend.parser.ExpressionParser

import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

class ExpressionParserTest {

    @Test
    fun parseInteger() {
        val source = "42"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)

        val expression = parser.parse()

        assertEquals(Ast.Expression.IntegerLiteral(value = 42, position = getPosition()), expression)
    }

    @Test
    fun parseVariableReference() {
        val source = "x"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)

        val expression = parser.parse()

        assertEquals(Ast.Expression.Identifier(name = "x", position = getPosition()), expression)
    }

    @Test
    fun parseTrue() {
        val source = "true"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)

        val expression = parser.parse()

        assertEquals(Ast.Expression.BoolLiteral(value = true, position = getPosition()), expression)
    }

    @Test
    fun parseFalse() {
        val source = "false"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)

        val expression = parser.parse()

        assertEquals(Ast.Expression.BoolLiteral(value = false, position = getPosition()), expression)
    }

    @Test
    fun parseNegativeNumber() {
        val source = "-42"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Unary(
            operator = UnaryOperator.MINUS,
            operand = Ast.Expression.IntegerLiteral(value = 42, position = getPosition(column = 2)),
            position = getPosition())

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseDoubleNegate() {
        val source = "--42"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Unary(
            operator = UnaryOperator.MINUS,
            operand = Ast.Expression.Unary(
                operator = UnaryOperator.MINUS,
                operand = Ast.Expression.IntegerLiteral(value = 42, position = getPosition(column = 3)),
                position = getPosition(column = 2)),
            position = getPosition())

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseNot() {
        val source = "!true"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Unary(
            operator = UnaryOperator.NOT,
            operand = Ast.Expression.BoolLiteral(value = true, position = getPosition(column = 2)),
            position = getPosition())

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseDoubleNot() {
        val source = "!!true"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Unary(
            operator = UnaryOperator.NOT,
            operand = Ast.Expression.Unary(
                operator = UnaryOperator.NOT,
                operand = Ast.Expression.BoolLiteral(value = true, position = getPosition(column = 3)),
                position = getPosition(column = 2)),
            position = getPosition())

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseAddition() {
        val source = "1 + 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
            operator = BinaryOperator.PLUS,
            right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
            position = getPosition(column = 3))

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseAdditionWithNegativeNumberLhs() {
        val source = "-1 + 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.Unary(
                operator = UnaryOperator.MINUS,
                operand = Ast.Expression.IntegerLiteral(value = 1, position = getPosition(column = 2)),
                position = getPosition()),
            operator = BinaryOperator.PLUS,
            right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 6)),
            position = getPosition(column = 4))

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseSubtraction() {
        val source = "1 - 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
                left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
                operator = BinaryOperator.MINUS,
                right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
                position = getPosition(column = 3)
            )

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseDivision() {
        val source = "1 / 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
            operator = BinaryOperator.DIVIDE,
            right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
            position = getPosition(column = 3))

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseMultiplication() {
        val source = "1 * 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
            operator = BinaryOperator.MULTIPLY,
            right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
            position = getPosition(column = 3))

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseModulo() {
        val source = "1 % 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
            operator = BinaryOperator.MODULO,
            right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
            position = getPosition(column = 3))

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseEquals() {
        val source = "1 == 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
            operator = BinaryOperator.EQ,
            right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 6)),
            position = getPosition(column = 3))

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseNotEquals() {
        val source = "1 != 2"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
            operator = BinaryOperator.NEQ,
            right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 6)),
            position = getPosition(column = 3))

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseAnd() {
        val source = "true && false"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.BoolLiteral(value = true, position = getPosition()),
            operator = BinaryOperator.AND,
            right = Ast.Expression.BoolLiteral(value = false, position = getPosition(column = 9)),
            position = getPosition(column = 6)
        )

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseOr() {
        val source = "true || false"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.BoolLiteral(value = true, position = getPosition()),
            operator = BinaryOperator.OR,
            right = Ast.Expression.BoolLiteral(value = false, position = getPosition(column = 9)),
            position = getPosition(column = 6)
        )

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseSubtractionAndAddition() {
        val source = "1 - 2 + 3"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.Binary(
                left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
                operator = BinaryOperator.MINUS,
                right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
                position = getPosition(column = 3)
            ),
            operator = BinaryOperator.PLUS,
            right = Ast.Expression.IntegerLiteral(value = 3, position = getPosition(column = 9)),
            position = getPosition(column = 7)
        )

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseMultiplicationAndDivision() {
        val source = "1 * 2 / 3"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.Binary(
                left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
                operator = BinaryOperator.MULTIPLY,
                right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
                position = getPosition(column = 3)
            ),
            operator = BinaryOperator.DIVIDE,
            right = Ast.Expression.IntegerLiteral(value = 3, position = getPosition(column = 9)),
            position = getPosition(column = 7)
        )

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseAdditionAndMultiplication() {
        val source = "1 + 2 * 3"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition()),
            operator = BinaryOperator.PLUS,
            right = Ast.Expression.Binary(
                left = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 5)),
                operator = BinaryOperator.MULTIPLY,
                right = Ast.Expression.IntegerLiteral(value = 3, position = getPosition(column = 9)),
                position = getPosition(column = 7)
            ),
            position = getPosition(column = 3)
        )

        val expression = parser.parse()

        assertEquals(expected, expression)
    }

    @Test
    fun parseParentheses() {
        val source = "(1 + 2) * 3"
        val lexer = lexerFromText(source)
        val parser = ExpressionParser(lexer)
        val expected = Ast.Expression.Binary(
            left = Ast.Expression.Binary(
                left = Ast.Expression.IntegerLiteral(value = 1, position = getPosition(column = 2)),
                operator = BinaryOperator.PLUS,
                right = Ast.Expression.IntegerLiteral(value = 2, position = getPosition(column = 6)),
                position = getPosition(column = 4)
            ),
            operator = BinaryOperator.MULTIPLY,
            right = Ast.Expression.IntegerLiteral(value = 3, position = getPosition(column = 11)),
            position = getPosition(column = 9)
        )

        val expression = parser.parse()

        assertEquals(expected, expression)
    }
}

fun getPosition(line: Int = 1, column: Int = 1) = SourcePosition(line = line, column = column)