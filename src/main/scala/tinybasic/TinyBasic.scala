package tinybasic

import java.nio.file.{Files, Paths}

object TinyBasic {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("use as: tinybasic <input>")
      return
    }

    val input = Paths.get(args(0))
    val name = input.getFileName.toString.replaceAll("\\..+$", "")

    val reader = Files.newBufferedReader(input)
    val program = new Parser(new Lexer(reader)).parse()

    val bytes = new Compiler(name, program).compile()
    val output = input.getParent.resolve(s"$name.class")
    Files.write(output, bytes)
  }
}
