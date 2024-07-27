package de.sschellhoff.frontend

class UnexpectedTokenException: Exception {
    constructor(expected: String, got: String, at: SourcePosition) : super("Unexpected token: $got, expected: $expected at $at")
    constructor(got: String, at: SourcePosition) : super("Unexpected token: $got at $at")
}

inline fun <reified E: Token> expectedToken(got: Token): Nothing {
    val expected = E::class.simpleName ?: "UNKNOWN"
    val actual = got::class.simpleName ?: "UNKNOWN"
    val at = got.position
    throw UnexpectedTokenException(expected = expected, got = actual, at = at)
}

fun unexpectedToken(got: Token): Nothing {
    val actual = got::class.simpleName ?: "UNKNOWN"
    val at = got.position
    throw UnexpectedTokenException(got = actual, at = at)
}