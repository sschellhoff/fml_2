package de.sschellhoff.frontend.parser

import de.sschellhoff.frontend.Token
import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.lexer.BufferedLexer
import de.sschellhoff.frontend.lexer.accept

class StatementParser(private val lexer: BufferedLexer, private val expressionParser: Parser<Ast.Expression>) : Parser<Ast.Statement> {
    override fun parse(): Ast.Statement {
        val token = lexer.peekNext()
        return when (token) {
            is Token.Let -> parseLet()
            is Token.Const -> parseConst()
            //is Token.While -> parseWhile()
            else -> throw Exception("Unexpected token: $token")
        }
    }

    private fun parseLet(): Ast.Statement {
        val letToken = lexer.accept<Token.Let>()
        val nameToken = lexer.accept<Token.Identifier>()
        val type = parseType()
        lexer.accept<Token.Assign>()
        val expression = ExpressionParser(lexer).parse()
        return Ast.Statement.Let(nameToken.name, type, expression, letToken.position)
    }

    private fun parseConst(): Ast.Statement {
        val constToken = lexer.accept<Token.Const>()
        val nameToken = lexer.accept<Token.Identifier>()
        val type = parseType()
        lexer.accept<Token.Assign>()
        val expression = ExpressionParser(lexer).parse()
        return Ast.Statement.Const(nameToken.name, type, expression, constToken.position)
    }

    private fun parseType(): Ast.Type {
        lexer.accept<Token.Colon>()
        val typeToken = lexer.accept<Token.Identifier>()
        return Ast.Type(typeToken.name, typeToken.position)
    }
}