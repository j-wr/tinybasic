package tinybasic

import tinybasic.Token._

class Parser(lexer: Lexer) {

  private var tokens = Stream.continually(lexer.next()).takeWhile(_ != Eof)
  def parse(): Program = {
    var lines = List[Line]()
    while (tokens.nonEmpty) {
      lines ::= line()
    }

    Program(lines.reverse)
  }
  private def advance() = {
    val tmp = token
    tokens = tokens.tail
    tmp
  }
  private def token = tokens.head
  private def line() = {
    tokens.head match {
      case Number(num) =>
        advance()
        Line(Some(num), statement())
      case _           => Line(None, statement())
    }
  }

  private def statement(): Stmt = {
    advance() match {
      case Print  => PrintStmt(exprs())
      case If     =>
        val left = expr()
        val op = comparison()
        val right = expr()
        assert(advance() == Then)
        IfStmt(left, right, op, statement())
      case Goto   => GotoStmt(expr())
      case Input  => InputStmt(vars())
      case Let    =>
        val name = advance() match {
          case Variable(n) => n
          case other       => throw new IllegalStateException(s"expected var got $other")
        }
        assert(advance() == Eq)
        AssignStmt(name, expr())
      case Gosub  => GosubStmt(expr())
      case Return => ReturnStmt
      case End    => EndStmt
      case other  => throw new IllegalStateException(s"unexpected token $other")
    }
  }

  private def exprs(es: List[Expr] = List.empty): List[Expr] = {
    if (es.nonEmpty) {
      if (token != Comma) {
        return es
      }
      advance()
    }

    exprs(es :+ (token match {
      case Literal(value) =>
        advance()
        StringConstExpr(value)
      case _              => expr()
    }))
  }

  private def vars(vs: List[String] = List.empty): List[String] = {
    if (vs.nonEmpty) {
      if (token != Comma) {
        return vs
      }
      advance()
    }

    vars(vs :+ (advance() match {
      case Variable(name) => name
      case other          => throw new IllegalStateException(s"expected var got $other")
    }))
  }

  private def comparison() = advance() match {
    case Eq    => RelOp.Eq
    case Ne    => RelOp.Ne
    case Lt    => RelOp.Lt
    case Gt    => RelOp.Gt
    case Le    => RelOp.Le
    case Ge    => RelOp.Ge
    case other => throw new UnsupportedOperationException(s"unexpected token $other")
  }

  private def expr() = {
    val expr = token match {
      case Plus  =>
        advance()
        term()
      case Minus =>
        advance()
        NegateExpr(term())
      case _     => term()
    }

    token match {
      case Plus  =>
        advance()
        ArithExpr(expr, term(), ArithOp.Add)
      case Minus =>
        advance()
        ArithExpr(expr, term(), ArithOp.Sub)
      case _     => expr
    }
  }

  private def term(): Expr = {
    val f = factor()
    token match {
      case Times  =>
        advance()
        ArithExpr(f, term(), ArithOp.Mul)
      case Divide =>
        advance()
        ArithExpr(f, term(), ArithOp.Div)
      case _      => f
    }
  }

  private def factor() = {
    advance() match {
      case Variable(name) => VarExpr(name)
      case Number(value)  => IntConstExpr(value)
      case LPar           =>
        val e = expr()
        assert(advance() == RPar)
        e
      case other          => throw new IllegalStateException(s"expected var | number | (expr) got $other")
    }
  }
}
