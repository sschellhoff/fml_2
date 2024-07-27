package de.sschellhoff.frontend.parser

interface Parser<RESULT> {
    fun parse(): RESULT
}