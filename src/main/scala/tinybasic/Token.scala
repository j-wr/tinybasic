package tinybasic

sealed trait Token

object Token {
  case class Literal(value: String) extends Token

  case class Number(value: Int) extends Token

  case class Variable(name: String) extends Token

  case object Comma extends Token

  case object LPar extends Token

  case object RPar extends Token

  case object NewLine extends Token

  case object Eof extends Token

  case object Print extends Token

  case object If extends Token

  case object Then extends Token

  case object Goto extends Token

  case object Input extends Token

  case object Let extends Token

  case object Gosub extends Token

  case object Return extends Token

  case object End extends Token

  case object Plus extends Token

  case object Minus extends Token

  case object Times extends Token

  case object Divide extends Token

  case object Eq extends Token

  case object Ne extends Token

  case object Ge extends Token

  case object Gt extends Token

  case object Le extends Token

  case object Lt extends Token
}