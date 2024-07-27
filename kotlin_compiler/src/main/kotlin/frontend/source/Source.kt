package de.sschellhoff.frontend.source

interface Source : SimpleSource {
    fun peekChar(): Char
}

fun Source.consume(c: Char): Boolean {
    if (peekChar() == c) {
        nextChar()
        return true
    }
    return false
}