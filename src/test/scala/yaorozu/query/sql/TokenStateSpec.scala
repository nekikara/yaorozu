package yaorozu.query.sql

import org.scalatest._

class TokenStateSpec extends FunSuite with DiagrammedAssertions {
  test("NeutralState should return WordState after reading a letter") {
    val actual = NeutralState.event('c')
    val expect = (WordState("c"), None)
    assert(actual == expect)
  }
  test("NeutralState should return WhiteState after reading a space") {
    val actual = NeutralState.event(' ')
    val expect = (WhiteSpaceState(" "), None)
    assert(actual == expect)
  }
}
