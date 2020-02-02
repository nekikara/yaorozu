package yaorozu.query.sql

import org.scalatest._

class LexerSpec extends FunSuite with DiagrammedAssertions {
  test("Lexer should return a list of String from a query statement") {
    val lexer = Lexer("CREATE DATABASE    \n test")
    val expect = List(Create, Database, Word("test"))
    assert(lexer.listTokens() == expect)
  }
  test("Lexer should return tokens of the create-table-statement which has one column") {
    val lexer = Lexer("CREATE table test (id int)")
    val expect = List(Create, Table, Word("test"), L_Parenthesis, Word("id"), Word("int"), R_Parenthesis)
    assert(lexer.listTokens() == expect)
    assert(lexer.nextToken() == End)
  }
}
