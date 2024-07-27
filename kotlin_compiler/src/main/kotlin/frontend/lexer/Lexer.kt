package de.sschellhoff.frontend.lexer

import de.sschellhoff.frontend.Token
import de.sschellhoff.frontend.source.Source
import de.sschellhoff.frontend.source.consume

class Lexer(private val source: Source) : BufferedLexer {
    private var buffer: Token? = null

    override tailrec fun getNext(): Token {
        val staticBuffer = buffer
        if (staticBuffer != null) {
            buffer = null
            return staticBuffer
        }
        if (source.isEof()) {
            return Token.Eof(position = source.getPosition())
        }
        if (source.peekChar().isWhitespace()) {
            source.nextChar()
            return getNext()
        }
        when {
            source.peekChar().isDigit() -> return readIntegerLiteral()
            source.peekChar().isJavaIdentifierStart() -> return readIdentifierOrKeyword()
        }

        val position = source.getPosition()
        val c = source.nextChar()

        return when (c) {
            '+' -> Token.Plus(position)
            '-' -> Token.Minus(position)
            '/' -> Token.Slash(position)
            '*' -> Token.Asterisk(position)
            '%' -> Token.Percent(position)
            '(' -> Token.LParen(position)
            ')' -> Token.RParen(position)
            '{' -> Token.LBrace(position)
            '}' -> Token.RBrace(position)
            '<' -> if (source.consume('=')) Token.LessEq(position) else Token.Less(position)
            '>' -> if (source.consume('=')) Token.GreaterEq(position) else Token.Greater(position)
            '=' -> if (source.consume('=')) Token.Eq(position) else Token.Assign(position)
            '!' -> if (source.consume('=')) Token.NotEq(position) else Token.Bang(position = position)
            '&' -> if (source.consume('&')) Token.And(position) else Token.Error(value = c, position = position)
            '|' -> if (source.consume('|')) Token.Or(position) else Token.Error(value = c, position = position)
            ',' -> Token.Comma(position)
            '.' -> Token.Dot(position)
            ':' -> Token.Colon(position)
            else -> Token.Error(value = source.nextChar(), position = position)
        }
    }

    override fun peekNext(): Token {
        val staticBuffer = buffer
        if (staticBuffer != null) {
            return staticBuffer
        }
        val token = getNext()
        buffer = token
        return token
    }

    private fun readIdentifierOrKeyword(): Token {
        val start = source.getPosition()
        val sb = StringBuilder()
        while (!source.isEof() && source.peekChar().isJavaIdentifierPart()) {
            sb.append(source.nextChar())
        }
        return when (val text = sb.toString()) {
            "if" -> Token.If(position = start)
            "else" -> Token.Else(position = start)
            "while" -> Token.While(position = start)
            "let" -> Token.Let(position = start)
            "const" -> Token.Const(position = start)
            "true" -> Token.BoolLiteral(value = true, position = start)
            "false" -> Token.BoolLiteral(value = false, position = start)
            else -> Token.Identifier(name = text, position = start)
        }
    }

    private fun readIntegerLiteral(): Token {
        val start = source.getPosition()
        var value = 0
        while (!source.isEof() && source.peekChar().isDigit()) {
            value = value * 10 + source.nextChar().code - '0'.code
        }
        return Token.IntegerLiteral(value = value, position = start)
    }
}
