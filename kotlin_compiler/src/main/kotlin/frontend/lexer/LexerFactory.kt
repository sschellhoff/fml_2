package de.sschellhoff.frontend.lexer

import de.sschellhoff.frontend.source.BufferedSource
import de.sschellhoff.frontend.source.SimpleTextSource

fun lexerFromText(data: String): BufferedLexer {
    val source = BufferedSource(SimpleTextSource(data))
    return Lexer(source)
}