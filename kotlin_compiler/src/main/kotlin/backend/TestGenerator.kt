package de.sschellhoff.backend

import de.sschellhoff.frontend.fmltypes.FmlType
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes.*

class TestGenerator {
    fun generateClass(name: String, packages: List<String>): ClassWriter {
        val cw = ClassWriter(0)

        val fullName = "${packages.joinToString("/")}/$name"
        println(fullName)

        cw.visit(V1_6, ACC_PUBLIC + ACC_SUPER, fullName, null, "java/lang/Object", null)
        return cw
    }

    fun writeClass(cw: ClassWriter) {
        cw.visitEnd()
    }

    fun generateMethod(cw: ClassWriter, name: String): MethodVisitor {
        val mw = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, name, "([Ljava/lang/String;)V", null, null)
        return mw
    }

    fun writeMethod(mw: MethodVisitor) {
        mw.visitEnd()
    }

    fun writePrintln(mw: MethodVisitor, message: String) {
        mw.visitCode()
        mw.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
        mw.visitLdcInsn(message)
        mw.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
        mw.visitInsn(RETURN)
        mw.visitMaxs(2, 1)
    }

    fun writeSomething(i: Int): Boolean {
        return true
    }
}