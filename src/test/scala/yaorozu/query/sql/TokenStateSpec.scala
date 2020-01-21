package yaorozu.query.sql

import org.scalatest._
import cats.syntax.applicative._

class TokenStateSpec extends FunSuite with DiagrammedAssertions {
  test("xxx") {
    val actual = List('c', ' ', 'd').foldLeft("".pure[TokenState.TokenReadState]) { (acc, c) =>
      acc.flatMap(_ => TokenState.readOne(c))
    }
    val expect = WordState("c")
    assert(actual.runS(NeutralState()).value == expect)
  }
}
