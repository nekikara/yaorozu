package yaorozu.query.sql

import org.scalatest._

class LexerSpec extends FunSuite with DiagrammedAssertions {
  test("Interpreter should return a Literal that is an Int") {
    val tokens = Lexer.listTokens("CREATE DATABASE test")
  }
}
