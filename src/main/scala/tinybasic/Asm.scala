package tinybasic

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}

object Asm {

  def getstatic(owner: String, name: String, desc: String)(implicit mv: MethodVisitor): Unit = {
    mv.visitFieldInsn(GETSTATIC, owner, name, desc)
  }

  def invokevirtual(owner: String, name: String, desc: String)(implicit mv: MethodVisitor): Unit = {
    mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, desc, false)
  }

  def invokespecial(owner: String, name: String, desc: String)(implicit mv: MethodVisitor): Unit = {
    mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, false)
  }

  def istore(slot: Int)(implicit mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ISTORE, slot)
  }

  def iload(slot: Int)(implicit mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ILOAD, slot)
  }

  def ineg()(implicit mv: MethodVisitor): Unit = {
    mv.visitInsn(INEG)
  }

  def branch(opcode: Int, label: Label)(implicit mv: MethodVisitor): Unit = {
    mv.visitJumpInsn(opcode, label)
  }

  def `new`(t: String)(implicit mv: MethodVisitor): Unit = {
    mv.visitTypeInsn(NEW, t)
  }

  def dup()(implicit mv: MethodVisitor): Unit = {
    mv.visitInsn(DUP)
  }

  def dup2()(implicit mv: MethodVisitor): Unit = {
    mv.visitInsn(DUP2)
  }

  def ldc(value: Any)(implicit mv: MethodVisitor): Unit = {
    mv.visitLdcInsn(value)
  }

  def `return`()(implicit mv: MethodVisitor): Unit = {
    mv.visitInsn(RETURN)
  }
  object newClassWriter {
    def apply(flags: Int,
              version: Int,
              access: Int,
              name: String,
              signature: String = null,
              superName: String = "java/lang/Object",
              interfaces: Array[String] = Array.empty[String]): ClassWriter = {
      val cw = new ClassWriter(flags)
      cw.visit(version, access, name, signature, superName, interfaces)
      cw
    }
  }
}
