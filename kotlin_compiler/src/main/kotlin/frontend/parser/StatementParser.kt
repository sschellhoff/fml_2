package de.sschellhoff.frontend.parser

import de.sschellhoff.frontend.Token
import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.fmltypes.Environment
import de.sschellhoff.frontend.lexer.BufferedLexer
import de.sschellhoff.frontend.lexer.accept

class StatementParser(private val lexer: BufferedLexer, private val expressionParser: Parser<Ast.Expression>, private val currentEnvironment: Environment) : Parser<Ast.Statement> {
    override fun parse(): Ast.Statement {
        return when (val token = lexer.peekNext()) {
            is Token.Let -> parseLet()
            is Token.Const -> parseConst()
            is Token.While -> parseWhile()
            else -> throw Exception("Unexpected token: $token")
        }
    }

    override fun isDone(): Boolean = lexer.peekNext() is Token.Eof

    private fun parseLet(): Ast.Statement {
        val letToken = lexer.accept<Token.Let>()
        val nameToken = lexer.accept<Token.Identifier>()
        val type = parseType()
        lexer.accept<Token.Assign>()
        val expression = expressionParser.parse()
        return Ast.Statement.Let(nameToken.name, type, expression, letToken.position)
    }

    private fun parseConst(): Ast.Statement {
        val constToken = lexer.accept<Token.Const>()
        val nameToken = lexer.accept<Token.Identifier>()
        val type = parseType()
        lexer.accept<Token.Assign>()
        val expression = expressionParser.parse()
        return Ast.Statement.Const(nameToken.name, type, expression, constToken.position)
    }

    private fun parseWhile(): Ast.Statement {
        val whileToken = lexer.accept<Token.While>()
        val condition = expressionParser.parse()
        val block = parseBlock()
        return Ast.Statement.While(condition, block, whileToken.position)
    }

    private fun parseBlock(): Ast.Block {
        val position = lexer.peekNext().position
        lexer.accept<Token.LBrace>()
        val statements = mutableListOf<Ast.Statement>()
        while (lexer.peekNext() !is Token.RBrace && lexer.peekNext() !is Token.Eof) {
            statements.add(parse())
        }
        lexer.accept<Token.RBrace>()
        return Ast.Block(statements, position, Environment(parent = currentEnvironment))
    }

    private fun parseType(): Ast.Type? {
        if (lexer.peekNext() !is Token.Colon) {
            return null
        }
        lexer.accept<Token.Colon>()
        val typeToken = lexer.accept<Token.Identifier>()
        return Ast.Type(typeToken.name, typeToken.position)
    }
}