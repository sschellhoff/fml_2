package de.sschellhoff.frontend.lexer

import de.sschellhoff.frontend.Token

interface BufferedLexer : SimpleLexer {
    fun peekNext(): Token
}