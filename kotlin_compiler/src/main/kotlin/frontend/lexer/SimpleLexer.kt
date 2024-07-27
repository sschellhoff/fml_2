package de.sschellhoff.frontend.lexer

import de.sschellhoff.frontend.Token

interface SimpleLexer {
    fun getNext(): Token
}

inline fun <reified TokenType : Token> SimpleLexer.accept(): TokenType {
    val token = getNext()
    require(token is TokenType) { "Expected token of type ${TokenType::class.simpleName}, but got $token" }
    return token
}
