package tinybasic

import org.objectweb.asm.ClassWriter._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{Label, MethodVisitor}
import tinybasic.ArithOp._
import tinybasic.Asm._
import tinybasic.RelOp._

class Compiler(name: String, program: Program) {
  private val labels = List.fill(program.lines.size)(new Label())
  private val labelMap = program.lines.zip(labels).toMap
  private val cv = Asm.newClassWriter(
    flags = COMPUTE_FRAMES | COMPUTE_FRAMES,
    version = V1_8,
    access = ACC_PUBLIC,
    name = name)
  private var locals = List.empty[String]

  private implicit val mv: MethodVisitor = cv.visitMethod(
    ACC_PUBLIC | ACC_STATIC,
    "main",
    "([Ljava/lang/String;)V",
    null,
    Array.empty
  )
  def compile(): Array[Byte] = {
    mv.visitCode()
    program.lines.foreach(line => {
      val label = labelMap(line)
      mv.visitLabel(label)
      compile(label, line.statement)
    })
    mv.visitMaxs(-1, -1)
    mv.visitEnd()

    cv.visitEnd()
    cv.toByteArray
  }
  private def getLocalSlot(name: String): Int = {
    if (!locals.contains(name)) locals :+= name
    locals.indexOf(name)
  }
  private def getLabel(num: Int): Label = {
    val line = labelMap.keys.single(_.num.exists(_ == num))
    labelMap(line)
  }
  private def compile(label: Label, stmt: Stmt): Unit = {
    stmt match {
      case PrintStmt(exprs)              =>
        getstatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        1 until exprs.size foreach { _ => dup() }

        exprs.foreach(expr => {
          compile(expr)
          val arg = expr match {
            case StringConstExpr(_) => "Ljava/lang/String;"
            case _                  => "I"
          }

          invokevirtual("java/io/PrintStream", "println", s"($arg)V")
        })
      case AssignStmt(target, expr)      =>
        compile(expr)
        istore(getLocalSlot(target))
      case IfStmt(left, right, op, body) =>
        compile(left)
        compile(right)

        val fallthrough = labels(labels.indexOf(label) + 1)
        val opcode = op match {
          case Eq => IF_ICMPNE
          case Ne => IF_ICMPEQ
          case Lt => IF_ICMPGE
          case Gt => IF_ICMPLE
          case Le => IF_ICMPGT
          case Ge => IF_ICMPLT
        }

        branch(opcode, fallthrough)
        compile(label, body)
      case GotoStmt(target)              =>
        assert(target.isInstanceOf[IntConstExpr])

        val num = target.asInstanceOf[IntConstExpr].value
        val label = getLabel(num)
        branch(GOTO, label)
      //      case GosubStmt(target)             =>
      case InputStmt(vars)      =>
        `new`("java/util/Scanner")
        dup()
        getstatic("java/lang/System", "in", "Ljava/io/InputStream;")
        invokespecial("java/util/Scanner", "<init>", "(Ljava/io/InputStream;)V")

        getstatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        1 until vars.size foreach { _ => dup2() }

        for (name <- vars) {
          ldc(s"$name=")
          invokevirtual("java/io/PrintStream", "print", "(Ljava/lang/String;)V")
          invokevirtual("java/util/Scanner", "nextInt", "()I")
          istore(getLocalSlot(name))
        }
      case ReturnStmt | EndStmt => `return`()
      case other                => throw new UnsupportedOperationException(s"unknown statement $other")
    }
  }
  private def compile(expr: Expr): Unit = {
    expr match {
      case ArithExpr(left, right, op) =>
        compile(left)
        compile(right)
        mv.visitInsn(op match {
          case Add => IADD
          case Sub => ISUB
          case Mul => IMUL
          case Div => IDIV
        })
      case NegateExpr(inner)          =>
        compile(inner)
        ineg()
      case IntConstExpr(value)        => ldc(value)
      case StringConstExpr(value)     => ldc(value)
      case VarExpr(n)                 => iload(getLocalSlot(n))
    }
  }
}
