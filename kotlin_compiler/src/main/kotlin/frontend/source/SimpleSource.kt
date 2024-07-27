package de.sschellhoff.frontend.source

import de.sschellhoff.frontend.SourcePosition

interface SimpleSource {
    fun nextChar(): Char

    fun isEof(): Boolean

    fun getSourceId(): String

    fun getPosition(): SourcePosition
}