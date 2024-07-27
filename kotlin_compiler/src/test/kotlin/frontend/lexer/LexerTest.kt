package frontend.lexer

import de.sschellhoff.frontend.SourcePosition
import de.sschellhoff.frontend.Token
import de.sschellhoff.frontend.lexer.lexerFromText
import org.junit.jupiter.api.Assertions.assertEquals
import kotlin.test.Test

class LexerTest {
    @Test
    fun `can read EOF`() {
        val sut = lexerFromText("")

        val result = sut.getNext()

        assertEquals(Token.Eof::class, result::class)
    }

    @Test
    fun `can read plus`() {
        val sut = lexerFromText("+")

        val result = sut.getNext()

        assertEquals(Token.Plus::class, result::class)
    }

    @Test
    fun `can read minus`() {
        val sut = lexerFromText("-")

        val result = sut.getNext()

        assertEquals(Token.Minus::class, result::class)
    }

    @Test
    fun `can read asterisk`() {
        val sut = lexerFromText("*")

        val result = sut.getNext()

        assertEquals(Token.Asterisk::class, result::class)
    }

    @Test
    fun `can read slash`() {
        val sut = lexerFromText("/")

        val result = sut.getNext()

        assertEquals(Token.Slash::class, result::class)
    }

    @Test
    fun `can read percent`() {
        val sut = lexerFromText("%")

        val result = sut.getNext()

        assertEquals(Token.Percent::class, result::class)
    }

    @Test
    fun `can read bang`() {
        val sut = lexerFromText("!")

        val result = sut.getNext()

        assertEquals(Token.Bang::class, result::class)
    }

    @Test
    fun `can read less`() {
        val sut = lexerFromText("<")

        val result = sut.getNext()

        assertEquals(Token.Less::class, result::class)
    }

    @Test
    fun `can read lessEqual`() {
        val sut = lexerFromText("<=")

        val result = sut.getNext()

        assertEquals(Token.LessEq::class, result::class)
    }

    @Test
    fun `can read greater`() {
        val sut = lexerFromText(">")

        val result = sut.getNext()

        assertEquals(Token.Greater::class, result::class)
    }

    @Test
    fun `can read greaterEqual`() {
        val sut = lexerFromText(">=")

        val result = sut.getNext()

        assertEquals(Token.GreaterEq::class, result::class)
    }

    @Test
    fun `can read assign`() {
        val sut = lexerFromText("=")

        val result = sut.getNext()

        assertEquals(Token.Assign::class, result::class)
    }

    @Test
    fun `can read equal`() {
        val sut = lexerFromText("==")

        val result = sut.getNext()

        assertEquals(Token.Eq::class, result::class)
    }

    @Test
    fun `can read notEqual`() {
        val sut = lexerFromText("!=")

        val result = sut.getNext()

        assertEquals(Token.NotEq::class, result::class)
    }

    @Test
    fun `can read and`() {
        val sut = lexerFromText("&&")

        val result = sut.getNext()

        assertEquals(Token.And::class, result::class)
    }

    @Test
    fun `can read or`() {
        val sut = lexerFromText("||")

        val result = sut.getNext()

        assertEquals(Token.Or::class, result::class)
    }

    @Test
    fun `can read integer literal`() {
        val sut = lexerFromText("123")

        val result = sut.getNext()

        assertEquals(Token.IntegerLiteral::class, result::class)
        assertEquals(123, (result as Token.IntegerLiteral).value)
    }

    @Test
    fun `can read identifier`() {
        val sut = lexerFromText("abc")

        val result = sut.getNext()

        assertEquals(Token.Identifier::class, result::class)
    }

    @Test
    fun `can read true`() {
        val sut = lexerFromText("true")

        val result = sut.getNext()

        assertEquals(Token.BoolLiteral::class, result::class)
        assertEquals(true, (result as Token.BoolLiteral).value)
    }

    @Test
    fun `can read false`() {
        val sut = lexerFromText("false")

        val result = sut.getNext()

        assertEquals(Token.BoolLiteral::class, result::class)
        assertEquals(false, (result as Token.BoolLiteral).value)
    }

    @Test
    fun `can read LParen`() {
        val sut = lexerFromText("(")

        val result = sut.getNext()

        assertEquals(Token.LParen::class, result::class)
    }

    @Test
    fun `can read RParen`() {
        val sut = lexerFromText(")")

        val result = sut.getNext()

        assertEquals(Token.RParen::class, result::class)
    }

    @Test
    fun `can read LBrace`() {
        val sut = lexerFromText("{")

        val result = sut.getNext()

        assertEquals(Token.LBrace::class, result::class)
    }

    @Test
    fun `can read RBrace`() {
        val sut = lexerFromText("}")

        val result = sut.getNext()

        assertEquals(Token.RBrace::class, result::class)
    }

    @Test
    fun `can read comma`() {
        val sut = lexerFromText(",")

        val result = sut.getNext()

        assertEquals(Token.Comma::class, result::class)
    }

    @Test
    fun `can read dot`() {
        val sut = lexerFromText(".")

        val result = sut.getNext()

        assertEquals(Token.Dot::class, result::class)
    }

    @Test
    fun `can read colon`() {
        val sut = lexerFromText(":")

        val result = sut.getNext()

        assertEquals(Token.Colon::class, result::class)
    }

    @Test
    fun `peek does not consume token`() {
        val sut = lexerFromText("abc")

        val result = sut.peekNext()

        assertEquals(Token.Identifier::class, result::class)
        assertEquals(Token.Identifier::class, sut.peekNext()::class)
    }

    @Test
    fun `next consumes token`() {
        val sut = lexerFromText("abc")

        val result = sut.getNext()

        assertEquals(Token.Identifier::class, result::class)
        assertEquals(Token.Eof::class, sut.getNext()::class)
    }

    @Test
    fun skipsWhitespace() {
        val sut = lexerFromText("  \t\n\rabc")

        val result = sut.getNext()

        assertEquals(Token.Identifier::class, result::class)
        assertEquals(getPosition(line= 2, column = 2), result.position)
    }

    @Test
    fun `reads let keyword`() {
        val sut = lexerFromText("let")

        val result = sut.getNext()

        assertEquals(Token.Let::class, result::class)
    }

    @Test
    fun `reads const keyword`() {
        val sut = lexerFromText("const")

        val result = sut.getNext()

        assertEquals(Token.Const::class, result::class)
    }

    @Test
    fun `reads while keyword`() {
        val sut = lexerFromText("while")

        val result = sut.getNext()

        assertEquals(Token.While::class, result::class)
    }

    @Test
    fun `reads if keyword`() {
        val sut = lexerFromText("if")

        val result = sut.getNext()

        assertEquals(Token.If::class, result::class)
    }

    @Test
    fun `reads else keyword`() {
        val sut = lexerFromText("else")

        val result = sut.getNext()

        assertEquals(Token.Else::class, result::class)
    }
}

fun getPosition(line: Int = 1, column: Int = 1) = SourcePosition(line = line, column = column)