package yaorozu.query

import org.scalatest._

class InterpreterSpec extends FunSuite with DiagrammedAssertions {
  test("Interpreter should return an Integer if an integer is request") {
    val actual = Interpreter.eval("RETURN 1")
    val expect = 1
    assert(actual == expect)
  }
  test("Interpreter should return a list if a list is requested") {
    val actual = Interpreter.eval("RETURN [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] AS list")
    val expect = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(actual == expect)
  }
}
