package yaorozu.query

import org.scalatest._

class TokenizerSpec extends FunSuite with DiagrammedAssertions {
  test("Tokenizer should return a Clause token") {
    val (actual, _) = Tokenizer.next(" RETURN 1 ", 0)
    val expect = CypToken("RETURN")
    assert(actual == expect)
  }
  test("Tokenizer should return a Integer token") {
    val (actual, _) = Tokenizer.next(" 1 ", 0)
    val expect = CypToken("1")
    assert(actual == expect)
  }
  test("Tokenizer should return a List token") {
    val (actual, _) = Tokenizer.next(" [1, 2,   3,4] ", 0)
    val expect = CypToken("[1,2,3,4]")
    assert(actual == expect)

    val (actual2, _) = Tokenizer.next(" [         ] ", 0)
    val expect2 = CypToken("[]")
    assert(actual2 == expect2)
  }
}
