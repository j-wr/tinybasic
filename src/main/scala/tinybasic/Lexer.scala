package tinybasic

import java.io.Reader

import tinybasic.Token._

import scala.annotation.tailrec

class Lexer(reader: Reader) {

  def next(): Token = {
    @tailrec
    def next0(): Token = {
      val x = reader.read()
      if (x == -1) return Eof

      val ch = x.toChar
      if (ch.isWhitespace) next0() else token(ch)
    }

    next0()
  }

  private def read(): Option[Char] = {
    val x = reader.read()
    if (x == -1) None else Some(x.toChar)
  }

  private def peek(): Option[Char] = {
    reader.mark(1)
    try {
      read()
    } finally {
      reader.reset()
    }
  }

  @tailrec
  private def take(test: (Char) => Boolean, buf: String = ""): String = {
    if (peek().exists(test)) take(test, buf + read().get) else buf
  }

  private def token(ch: Char) = ch match {
    case '\n'                                        => NewLine
    case '+'                                         => Plus
    case '-'                                         => Minus
    case '*'                                         => Times
    case '/'                                         => Divide
    case '='                                         => Eq
    case ','                                         => Comma
    case '('                                         => LPar
    case ')'                                         => RPar
    case '"'                                         => string()
    case '<' | '>'                                   => comparison(ch)
    case _ if ch.isDigit                             => number(ch)
    case _ if ch.isUpper && peek().exists(_.isUpper) => keyword(ch)
    case _ if ch.isUpper                             => Variable(ch.toString)
    case _                                           => throw new IllegalStateException(s"Unknown character: $ch")
  }

  private def string(): Token = {
    val tmp = take(_ != '"')
    read()
    Literal(tmp)
  }

  private def comparison(ch: Char): Token = {
    val n = peek()
    (ch, n) match {
      case ('>', Some('='))                    => Ge
      case ('<', Some('='))                    => Le
      case ('>', Some('<')) | ('<', Some('>')) => Ne
      case ('>', _)                            => Gt
      case ('<', _)                            => Lt
      case _                                   => throw new AssertionError()
    }
  }

  private def number(ch: Char): Token = {
    Number(take(_.isDigit, ch.toString).toInt)
  }

  private def keyword(ch: Char): Token = {
    take(_.isUpper, ch.toString) match {
      case "PRINT"  => Print
      case "IF"     => If
      case "THEN"   => Then
      case "LET"    => Let
      case "INPUT"  => Input
      case "GOTO"   => Goto
      case "GOSUB"  => Gosub
      case "RETURN" => Return
      case "END"    => End
      case default  => throw new IllegalStateException(s"$default is not a valid keyword.")
    }
  }
}