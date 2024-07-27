package de.sschellhoff.frontend.source

import de.sschellhoff.frontend.SourcePosition

class BufferedSource(private val simpleSource: SimpleSource) : Source {
    private var buffer: Pair<Char, SourcePosition>? = null

    override fun peekChar(): Char = getBuffered().first

    override fun nextChar(): Char {
        val c = getBuffered().first
        buffer = null
        return c
    }

    override fun isEof(): Boolean {
        val (c, _) = getBuffered()
        return c == '\u0000'
    }

    override fun getSourceId(): String = simpleSource.getSourceId()

    override fun getPosition(): SourcePosition = getBuffered().second

    private fun getBuffered(): Pair<Char, SourcePosition> {
        val next = buffer ?: getNewBuffer()
        buffer = next
        return next
    }

    private fun getNewBuffer(): Pair<Char, SourcePosition> {
        val position = simpleSource.getPosition()
        val c = simpleSource.nextChar()
        return c to position
    }
}