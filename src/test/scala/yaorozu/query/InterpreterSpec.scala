package yaorozu.query

import org.scalatest._

class InterpreterSpec extends FunSuite with DiagrammedAssertions {
  test("Interpreter should return a Literal that is an Int") {
    val actual = Interpreter.run("RETURN 1")
    val expect = 1
    assert(actual == expect)
  }
  test("Interpreter should return a Literal that is a List of Int") {
    val actual = Interpreter.run("RETURN [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]")
    val expect = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(actual == expect)
  }
}
