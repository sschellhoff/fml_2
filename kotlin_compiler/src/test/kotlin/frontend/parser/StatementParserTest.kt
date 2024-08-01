package frontend.parser

import de.sschellhoff.frontend.SourcePosition
import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.fmltypes.Environment
import de.sschellhoff.frontend.lexer.lexerFromText
import de.sschellhoff.frontend.parser.ExpressionParser
import de.sschellhoff.frontend.parser.StatementParser
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class StatementParserTest {
    @Test
    fun `can parse let`() {
        val source = "let x: Int = 1337"
        val parser = getStatementParserParser(source)

        val result = parser.parse()

        assertEquals(Ast.Statement.Let("x", Ast.Type("Int", getPosition(column = 8)), Ast.Expression.IntegerLiteral(1337, getPosition(column = 14)), getPosition()), result)
    }

    @Test
    fun `can parse const`() {
        val source = "const x: Int = 1337"
        val parser = getStatementParserParser(source)

        val result = parser.parse()

        assertEquals(Ast.Statement.Const("x", Ast.Type("Int", getPosition(column = 10)), Ast.Expression.IntegerLiteral(1337, getPosition(column = 16)), getPosition()), result)
    }

    @Test
    fun `can parse while`() {
        val source = "while true { }"
        val parser = getStatementParserParser(source)

        val result = parser.parse()

        assertEquals(Ast.Statement.While(Ast.Expression.BoolLiteral(true, getPosition(column = 7)), Ast.Block(emptyList(), getPosition(column = 12), Environment(parent =  Environment())), getPosition()), result)
    }
}

fun getStatementParserParser(source: String): StatementParser {
    val lexer = lexerFromText(source)
    return StatementParser(lexer, ExpressionParser(lexer), Environment())
}

fun getPosition(line: Int = 1, column: Int = 1) = SourcePosition(line = line, column = column)