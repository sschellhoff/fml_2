package de.sschellhoff

import de.sschellhoff.backend.TestGenerator
import java.io.FileOutputStream

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
fun main() {
    val testGenerator = TestGenerator()
    val mainClass = testGenerator.generateClass("Test", listOf("de", "sschellhoff"))
    val mainMethod = testGenerator.generateMethod(mainClass, "main")
    testGenerator.writePrintln(mainMethod, "Hello, World!")
    testGenerator.writeMethod(mainMethod)
    testGenerator.writeClass(mainClass)
    val out = FileOutputStream("./generated/de/sschellhoff/Test.class")
    out.write(mainClass.toByteArray())
    out.close()
    // cd /tmp
    // java de.sschellhoff.Test
    // java -cp . de.sschellhoff.Test
}
