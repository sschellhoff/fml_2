package de.sschellhoff.frontend.source

import de.sschellhoff.frontend.SourcePosition
import java.util.UUID

class SimpleTextSource(private val data: String, private val id: String = "TEXT_${UUID.randomUUID()}") : SimpleSource {
    private var position = 0
    private var currentColumn: Int = 1
    private var currentLine: Int = 1

    override fun nextChar(): Char {
        if (isEof()) {
            return '\u0000'
        }
        val c = data[position++]
        if (c == '\n') {
            currentLine++
            currentColumn = 1
        } else {
            currentColumn++
        }
        return c
    }

    override fun isEof(): Boolean = position >= data.length

    override fun getSourceId(): String = id

    override fun getPosition(): SourcePosition = SourcePosition(line = currentLine, column = currentColumn)
}