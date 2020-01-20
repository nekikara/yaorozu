package yaorozu.query.sql

import org.scalatest._

class LexerSpec extends FunSuite with DiagrammedAssertions {
  test("Lexer should return a list of String from a query statement") {
    val actual = Lexer.listTokens("CREATE DATABASE    \n test")
    val expect = List("CREATE", " ",  "DATABASE", "    \n ", "test")
    assert(actual == expect)
  }
  test("Lexer should know ") {
    val actual = Lexer("h", 1).isOver
    val expect = true
    assert(actual == expect)
  }
  test("Lexer should know a space") {
    val actual = Lexer.isWS(' ')
    val expect = true
    assert(actual == expect)
    val actual2 = Lexer.isWord(' ')
    val expect2 = false
    assert(actual2 == expect2)
  }
}
