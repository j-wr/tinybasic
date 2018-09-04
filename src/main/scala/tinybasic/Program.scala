package tinybasic

case class Program(lines: List[Line]) {

}

case class Line(num: Option[Int], statement: Stmt)