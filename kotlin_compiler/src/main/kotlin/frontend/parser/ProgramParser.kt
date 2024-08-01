package de.sschellhoff.frontend.parser

import de.sschellhoff.frontend.SourcePosition
import de.sschellhoff.frontend.ast.Ast

class ProgramParser(private val statementParser: Parser<Ast.Statement>) : Parser<Ast.Program> {
    override fun parse(): Ast.Program {
        val statements = mutableListOf<Ast.Statement>()
        while (!statementParser.isDone()) {
            statements.add(statementParser.parse())
        }
        return Ast.Program(statements, SourcePosition(1, 1))
    }

    override fun isDone(): Boolean = statementParser.isDone()
}