package tinybasic

sealed trait Stmt

case class PrintStmt(exprs: List[Expr]) extends Stmt

case class IfStmt(left: Expr, right: Expr, operator: RelOp, target: Stmt) extends Stmt

sealed trait RelOp

object RelOp {
  case object Eq extends RelOp
  case object Ne extends RelOp
  case object Lt extends RelOp
  case object Gt extends RelOp
  case object Le extends RelOp
  case object Ge extends RelOp
}

case class GotoStmt(target: Expr) extends Stmt

case class InputStmt(vars: List[String]) extends Stmt

case class AssignStmt(name: String, value: Expr) extends Stmt

case class GosubStmt(target: Expr) extends Stmt

case object ReturnStmt extends Stmt

case object EndStmt extends Stmt