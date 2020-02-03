package yaorozu.query.sql

import org.scalatest._

class TokenSpec extends FunSuite with DiagrammedAssertions {
  test("Empty string should be mapped to Word") {
    val actual = Token.mapWord("")
    val expect = Word("")
    assert(actual == expect)
  }
  test("CReate string should be mapped to Create") {
    val actual = Token.mapWord("CReate")
    val expect = Create
    assert(actual == expect)
  }
}
