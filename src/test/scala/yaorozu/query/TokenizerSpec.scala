package yaorozu.query

import org.scalatest._

class TokenizerSpec extends FunSuite with DiagrammedAssertions {
  test("Tokenizer should return a Integer token") {
    val actual = Tokenizer.next(" 1 ")
    val expect = CypToken("1")
    assert(actual == expect)
  }
}
