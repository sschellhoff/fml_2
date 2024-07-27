package frontend.source

import de.sschellhoff.frontend.source.BufferedSource
import de.sschellhoff.frontend.source.SimpleTextSource
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class BufferedSourceTest {
    @Test
    fun `peekChar should return the next character without consuming it`() {
        val sut = createSut("abc")

        assertEquals('a', sut.peekChar())
        assertEquals('a', sut.peekChar())
    }

    @Test
    fun `nextChar should return the next character and consume it`() {
        val sut = createSut("abc")

        assertEquals('a', sut.nextChar())
        assertEquals('b', sut.nextChar())
    }

    @Test
    fun `isEof should return true if there are no more characters to read`() {
        val sut = createSut("a")

        sut.nextChar()

        assertTrue(sut.isEof())
    }

    @Test
    fun `isEof should return false if there are more characters to read`() {
        val sut = createSut("a")

        assertFalse(sut.isEof())
    }
}

fun createSut(data: String) = BufferedSource(SimpleTextSource(data))