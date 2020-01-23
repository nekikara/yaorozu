package yaorozu.query.sql

import org.scalatest._

class LexerSpec extends FunSuite with DiagrammedAssertions {
  test("Lexer should return a list of String from a query statement") {
    val lexer = Lexer("CREATE DATABASE    \n test")
    val expect = List("CREATE", "DATABASE", "test")
    assert(lexer.listTokens() == expect)
  }
}
