package de.sschellhoff.frontend.fmltypes

sealed interface FmlType {
    data object Unit: FmlType
    data object Int : FmlType
    data object Bool : FmlType
}