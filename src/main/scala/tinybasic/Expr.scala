package tinybasic

sealed trait Expr

case class VarExpr(name: String) extends Expr

case class IntConstExpr(value: Int) extends Expr

case class StringConstExpr(value: String) extends Expr

case class ArithExpr(left: Expr, right: Expr, operator: ArithOp) extends Expr

sealed trait ArithOp

object ArithOp {
  case object Add extends ArithOp
  case object Sub extends ArithOp
  case object Mul extends ArithOp
  case object Div extends ArithOp
}

case class NegateExpr(expr: Expr) extends Expr