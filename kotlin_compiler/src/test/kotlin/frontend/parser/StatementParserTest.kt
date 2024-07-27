package frontend.parser

import de.sschellhoff.frontend.SourcePosition
import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.lexer.lexerFromText
import de.sschellhoff.frontend.parser.ExpressionParser
import de.sschellhoff.frontend.parser.StatementParser
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class StatementParserTest {
    @Test
    fun `can parse let`() {
        val source = "let x: Int = 1337"
        val parser = getParser(source)

        val result = parser.parse()

        assertEquals(Ast.Statement.Let("x", Ast.Type("Int", getPosition(column = 8)), Ast.Expression.IntegerLiteral(1337, getPosition(column = 14)), getPosition()), result)
    }

    @Test
    fun `can parse const`() {
        val source = "const x: Int = 1337"
        val parser = getParser(source)

        val result = parser.parse()

        assertEquals(Ast.Statement.Const("x", Ast.Type("Int", getPosition(column = 10)), Ast.Expression.IntegerLiteral(1337, getPosition(column = 16)), getPosition()), result)
    }
}

fun getParser(source: String): StatementParser {
    val lexer = lexerFromText(source)
    return StatementParser(lexer, ExpressionParser(lexer))
}

fun getPosition(line: Int = 1, column: Int = 1) = SourcePosition(line = line, column = column)