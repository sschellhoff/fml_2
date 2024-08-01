package frontend

import de.sschellhoff.frontend.SourcePosition
import de.sschellhoff.frontend.TypeInferation
import de.sschellhoff.frontend.ast.Ast
import de.sschellhoff.frontend.ast.BinaryOperator
import de.sschellhoff.frontend.ast.UnaryOperator
import de.sschellhoff.frontend.fmltypes.Environment
import de.sschellhoff.frontend.fmltypes.FmlType
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class TypeInferationTest {
    @Test
    fun `integer constant results in int`() {
        val sut = TypeInferation()
        val result = sut.inferType(Ast.Expression.IntegerLiteral(42, getPosition()))

        assertEquals(FmlType.Int, result)
    }

    @Test
    fun `boolean constant results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(Ast.Expression.BoolLiteral(true, getPosition()))

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `adding two integers results in int`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.PLUS,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Int, result)
    }

    @Test
    fun `adding two booleans results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Binary(
                    Ast.Expression.BoolLiteral(true, getPosition()),
                    BinaryOperator.PLUS,
                    Ast.Expression.BoolLiteral(false, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `subtracting two integers results in int`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.MINUS,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Int, result)
    }

    @Test
    fun `subtracting two booleans results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Binary(
                    Ast.Expression.BoolLiteral(true, getPosition()),
                    BinaryOperator.MINUS,
                    Ast.Expression.BoolLiteral(false, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `multiplying two integers results in int`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.MULTIPLY,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Int, result)
    }

    @Test
    fun `multiplying two booleans results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Binary(
                    Ast.Expression.BoolLiteral(true, getPosition()),
                    BinaryOperator.MULTIPLY,
                    Ast.Expression.BoolLiteral(false, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `dividing two integers results in int`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.DIVIDE,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Int, result)
    }

    @Test
    fun `dividing two booleans results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Binary(
                    Ast.Expression.BoolLiteral(true, getPosition()),
                    BinaryOperator.DIVIDE,
                    Ast.Expression.BoolLiteral(false, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `modulo two integers results in int`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.MODULO,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Int, result)
    }

    @Test
    fun `modulo two booleans results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Binary(
                    Ast.Expression.BoolLiteral(true, getPosition()),
                    BinaryOperator.MODULO,
                    Ast.Expression.BoolLiteral(false, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `comparing two integers with less than results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.LT,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two integers with less than or equal results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.LTE,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two integers with greater than results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.GT,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two integers with greater than or equal results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.GTE,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two integers with equal results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.EQ,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two integers with not equal results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.IntegerLiteral(1, getPosition()),
                BinaryOperator.NEQ,
                Ast.Expression.IntegerLiteral(2, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two booleans with equal results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.BoolLiteral(true, getPosition()),
                BinaryOperator.EQ,
                Ast.Expression.BoolLiteral(false, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two booleans with not equal results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.BoolLiteral(true, getPosition()),
                BinaryOperator.NEQ,
                Ast.Expression.BoolLiteral(false, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two integers with and results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Binary(
                    Ast.Expression.IntegerLiteral(1, getPosition()),
                    BinaryOperator.AND,
                    Ast.Expression.IntegerLiteral(2, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `comparing two integers with or results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Binary(
                    Ast.Expression.IntegerLiteral(1, getPosition()),
                    BinaryOperator.OR,
                    Ast.Expression.IntegerLiteral(2, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `comparing two booleans with and results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.BoolLiteral(true, getPosition()),
                BinaryOperator.AND,
                Ast.Expression.BoolLiteral(false, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `comparing two booleans with or results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Binary(
                Ast.Expression.BoolLiteral(true, getPosition()),
                BinaryOperator.OR,
                Ast.Expression.BoolLiteral(false, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `negative int constant results in int`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Unary(
                UnaryOperator.MINUS,
                Ast.Expression.IntegerLiteral(42, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Int, result)
    }

    @Test
    fun `negative bool constant results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Unary(
                    UnaryOperator.MINUS,
                    Ast.Expression.BoolLiteral(true, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `negating int constant results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Expression.Unary(
                    UnaryOperator.NOT,
                    Ast.Expression.IntegerLiteral(42, getPosition()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `negating bool constant results in bool`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Expression.Unary(
                UnaryOperator.NOT,
                Ast.Expression.BoolLiteral(true, getPosition()),
                getPosition()
            )
        )

        assertEquals(FmlType.Bool, result)
    }

    @Test
    fun `can infer integer type of let statement`() {
        val sut = TypeInferation()
        val result =
            sut.inferType(Ast.Statement.Let("x", null, Ast.Expression.IntegerLiteral(42, getPosition()), getPosition()))
        val inferedType = sut.inferType(Ast.Expression.Identifier("x", getPosition()))

        assertEquals(FmlType.Unit, result)
        assertEquals(FmlType.Int, inferedType)
    }

    @Test
    fun `can infer bool type of let statement`() {
        val sut = TypeInferation()
        val result =
            sut.inferType(Ast.Statement.Let("x", null, Ast.Expression.BoolLiteral(true, getPosition()), getPosition()))
        val inferedType = sut.inferType(Ast.Expression.Identifier("x", getPosition()))

        assertEquals(FmlType.Unit, result)
        assertEquals(FmlType.Bool, inferedType)
    }

    @Test
    fun `let declares variable which cannot be redeclared`() {
        val sut = TypeInferation()
        sut.inferType(Ast.Statement.Let("x", null, Ast.Expression.IntegerLiteral(42, getPosition()), getPosition()))

        assertFailsWith<IllegalStateException> {
            sut.inferType(Ast.Statement.Let("x", null, Ast.Expression.IntegerLiteral(42, getPosition()), getPosition()))
        }
    }

    @Test
    fun `can infer integer type of const statement`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Statement.Const(
                "x",
                null,
                Ast.Expression.IntegerLiteral(42, getPosition()),
                getPosition()
            )
        )
        val inferedType = sut.inferType(Ast.Expression.Identifier("x", getPosition()))

        assertEquals(FmlType.Unit, result)
        assertEquals(FmlType.Int, inferedType)
    }

    @Test
    fun `can infer bool type of const statement`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Statement.Const(
                "x",
                null,
                Ast.Expression.BoolLiteral(true, getPosition()),
                getPosition()
            )
        )
        val inferedType = sut.inferType(Ast.Expression.Identifier("x", getPosition()))

        assertEquals(FmlType.Unit, result)
        assertEquals(FmlType.Bool, inferedType)
    }

    @Test
    fun `const declares variable which cannot be redeclared`() {
        val sut = TypeInferation()
        sut.inferType(Ast.Statement.Const("x", null, Ast.Expression.IntegerLiteral(42, getPosition()), getPosition()))

        assertFailsWith<IllegalStateException> {
            sut.inferType(Ast.Statement.Let("x", null, Ast.Expression.IntegerLiteral(42, getPosition()), getPosition()))
        }
    }

    @Test
    fun `can infer type of while statement`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Statement.While(
                Ast.Expression.BoolLiteral(true, getPosition()),
                Ast.Block(emptyList(), getPosition(), Environment()),
                getPosition()
            )
        )

        assertEquals(FmlType.Unit, result)
    }

    @Test
    fun `can infer type of while statement with block`() {
        val sut = TypeInferation()
        val result = sut.inferType(
            Ast.Statement.While(
                Ast.Expression.BoolLiteral(true, getPosition()),
                Ast.Block(
                    listOf(
                        Ast.Statement.Let(
                            "x",
                            null,
                            Ast.Expression.IntegerLiteral(42, getPosition()),
                            getPosition()
                        )
                    ), getPosition(), Environment()
                ),
                getPosition()
            )
        )

        assertEquals(FmlType.Unit, result)
    }

    @Test
    fun `int type as condition in while results in error`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Statement.While(
                    Ast.Expression.IntegerLiteral(42, getPosition()),
                    Ast.Block(emptyList(), getPosition(), Environment()),
                    getPosition()
                )
            )
        }
    }

    @Test
    fun `detects type error in block of while`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferType(
                Ast.Statement.While(
                    Ast.Expression.BoolLiteral(true, getPosition()), Ast.Block(
                        listOf(
                            Ast.Statement.Let(
                                "x",
                                null,
                                value = Ast.Expression.Unary(
                                    UnaryOperator.MINUS,
                                    operand = Ast.Expression.BoolLiteral(false, getPosition()),
                                    position = getPosition()
                                ),
                                getPosition()
                            ),
                        ), getPosition(), Environment()
                    ), getPosition()
                )
            )
        }
    }

    @Test
    fun `detects type error in program`() {
        val sut = TypeInferation()

        assertFailsWith<IllegalStateException> {
            sut.inferTypes(
                Ast.Program(
                    listOf(
                        Ast.Statement.Let(
                            "x",
                            null,
                            value = Ast.Expression.Unary(
                                UnaryOperator.MINUS,
                                Ast.Expression.BoolLiteral(false, getPosition()),
                                getPosition()
                            ),
                            getPosition()
                        ),
                    ), getPosition(), Environment()
                )
            )
        }
    }
}

fun getPosition() = SourcePosition(1, 1)