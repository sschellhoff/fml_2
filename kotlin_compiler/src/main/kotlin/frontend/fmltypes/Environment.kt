package de.sschellhoff.frontend.fmltypes

data class Environment(private val variables: MutableMap<String, FmlType> = mutableMapOf(), val parent: Environment? = null) {
    fun declare(name: String, type: FmlType, isConstant: Boolean) {
        check(!variables.containsKey(name)) { "Variable $name already declared" }
        variables[name] = type
    }

    fun get(name: String): FmlType? {
        return variables[name]
    }
}