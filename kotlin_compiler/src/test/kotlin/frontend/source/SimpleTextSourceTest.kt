package frontend.source

import de.sschellhoff.frontend.source.SimpleTextSource
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class SimpleTextSourceTest {
    @Test
    fun `isEof returns true on empty data`() {
        val sut = SimpleTextSource("")

        assertTrue(sut.isEof())
    }

    @Test
    fun `isEof returns false on non empty data`() {
        val sut = SimpleTextSource("a")

        assertFalse(sut.isEof())
    }

    @Test
    fun `isEof returns true after reading all data`() {
        val sut = SimpleTextSource("a")
        sut.nextChar()

        assertTrue(sut.isEof())
    }

    @Test
    fun `nextChar returns null char on empty data`() {
        val sut = SimpleTextSource("")

        assertEquals('\u0000', sut.nextChar())
    }

    @Test
    fun `nextChar returns null char after reading all data`() {
        val sut = SimpleTextSource("a")
        sut.nextChar()

        assertEquals('\u0000', sut.nextChar())
    }

    @Test
    fun `nextChar returns first char on non empty data`() {
        val sut = SimpleTextSource("a")

        assertEquals('a', sut.nextChar())
    }

    @Test
    fun `nextChar returns second char on non empty data`() {
        val sut = SimpleTextSource("ab")
        sut.nextChar()

        assertEquals('b', sut.nextChar())
    }

    @Test
    fun `nextChar increments line on newline char`() {
        val sut = SimpleTextSource("\n")

        sut.nextChar()

        assertEquals(2, sut.getPosition().line)
    }

    @Test
    fun `nextChar increments column on char other than newline`() {
        val sut = SimpleTextSource("a")

        sut.nextChar()

        assertEquals(2, sut.getPosition().column)
    }

    @Test
    fun `nextChar resets column on newline char`() {
        val sut = SimpleTextSource("a\n")

        sut.nextChar()
        sut.nextChar()

        assertEquals(1, sut.getPosition().column)
    }
}