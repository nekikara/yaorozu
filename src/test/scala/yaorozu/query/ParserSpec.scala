package yaorozu.query

import org.scalatest._

class ParserSpec extends FunSuite with DiagrammedAssertions {
  test("Parser parses a query") {
    val actual = Parser.parse("SELECT '1' '1';")
    val expect = List(Token("SELECT"), Token("'1'"), Token("'1'"), Token(";"))
    assert(actual == expect)
  }
  test("Nope read and should return a result with a candidate and the next state") {
    val nope = NopeState().read("", ' ')
    assert(nope == AnalyzeState(" ", NopeState(), None))

    val constant = NopeState().read("", 'a')
    assert(constant == AnalyzeState("a", ConstantState(), None))

    val quote = NopeState().read("", '\'')
    assert(quote == AnalyzeState("'", QuoteContentState(), None))
  }
}
