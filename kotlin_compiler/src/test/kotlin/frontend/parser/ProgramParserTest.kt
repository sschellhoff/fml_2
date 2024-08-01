package frontend.parser

import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.ast.BinaryOperator
import de.sschellhoff.frontend.ast.prettyPrint
import de.sschellhoff.frontend.fmltypes.Environment
import de.sschellhoff.frontend.lexer.lexerFromText
import de.sschellhoff.frontend.parser.ExpressionParser
import de.sschellhoff.frontend.parser.ProgramParser
import de.sschellhoff.frontend.parser.StatementParser
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class ProgramParserTest {
    @Test
    fun `can parse some program`() {
        val source = "let x: Int = 1337\nlet y: Int = 42\nconst z = x + y"
        val parser = getProgramParser(source)

        val result = parser.parse()

        assertEquals(
            Ast.Program(listOf(
            Ast.Statement.Let("x", Ast.Type("Int", getPosition(column = 8)), Ast.Expression.IntegerLiteral(1337, getPosition(column = 14)), getPosition()),
            Ast.Statement.Let("y", Ast.Type("Int", getPosition(line = 2, column = 8)), Ast.Expression.IntegerLiteral(42, getPosition(line = 2, column = 14)), getPosition(line = 2)),
            Ast.Statement.Const("z", null, Ast.Expression.Binary(Ast.Expression.Identifier("x", getPosition(line = 3, column = 11)), BinaryOperator.PLUS, Ast.Expression.Identifier("y", getPosition(line = 3, column = 15)), getPosition(line = 3, column = 13)), getPosition(line = 3))
        ), getPosition()), result)
    }

    @Test
    fun `can parse a program with a while loop`() {
        val source = "while true { let x: Int = 1337 }"
        val parser = getProgramParser(source)
        val expected = """(while true {
            |(let x: Int = 1337)
            |})""".trimMargin()

        val result = parser.parse()

        assertEquals(expected, result.prettyPrint())
    }
}

fun getProgramParser(source: String): ProgramParser {
    val lexer = lexerFromText(source)
    return ProgramParser(StatementParser(lexer, ExpressionParser(lexer), Environment()))
}