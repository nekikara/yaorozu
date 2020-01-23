package yaorozu.query.sql

import org.scalatest._

class ParserSpec extends FunSuite with DiagrammedAssertions {
  test("Parser should not throw a RuntimeError") {
    val lexer = Lexer("CREATE DATABASE    \n test")
    val parser = Parser(lexer)
    try {
      parser.readCreate()
    } catch {
      case ex: RuntimeException => fail(s"Should not call here : $ex")
    }
  }
}
