package de.sschellhoff.frontend

data class SourcePosition(val line: Int, val column: Int)

sealed class Token(val position: SourcePosition) {
    class Eof(position: SourcePosition) : Token(position)
    class Error(val value: Char, position: SourcePosition) : Token(position)

    class Identifier(val name: String, position: SourcePosition) : Token(position)
    class IntegerLiteral(val value: Int, position: SourcePosition) : Token(position)
    class BoolLiteral(val value: Boolean, position: SourcePosition) : Token(position)

    class Let(position: SourcePosition) : Token(position)
    class Const(position: SourcePosition) : Token(position)

    class If(position: SourcePosition) : Token(position)
    class Else(position: SourcePosition) : Token(position)
    class While(position: SourcePosition) : Token(position)

    class Eq(position: SourcePosition) : Token(position)
    class NotEq(position: SourcePosition) : Token(position)
    class Greater(position: SourcePosition) : Token(position)
    class Less(position: SourcePosition) : Token(position)
    class GreaterEq(position: SourcePosition) : Token(position)
    class LessEq(position: SourcePosition) : Token(position)

    class And(position: SourcePosition) : Token(position)
    class Or(position: SourcePosition) : Token(position)
    class Plus(position: SourcePosition) : Token(position)
    class Minus(position: SourcePosition) : Token(position)
    class Asterisk(position: SourcePosition) : Token(position)
    class Slash(position: SourcePosition) : Token(position)
    class Percent(position: SourcePosition) : Token(position)
    class Bang(position: SourcePosition) : Token(position)

    class LParen(position: SourcePosition) : Token(position)
    class RParen(position: SourcePosition) : Token(position)
    class LBrace(position: SourcePosition) : Token(position)
    class RBrace(position: SourcePosition) : Token(position)

    class Assign(position: SourcePosition) : Token(position)
    class Comma(position: SourcePosition) : Token(position)
    class Dot(position: SourcePosition) : Token(position)
    class Colon(position: SourcePosition) : Token(position)
}
