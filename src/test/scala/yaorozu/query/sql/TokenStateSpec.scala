package yaorozu.query.sql

import org.scalatest._
import cats.syntax.applicative._
import TokenState.TokenReadState

class TokenStateSpec extends FunSuite with DiagrammedAssertions {
  test("xxx") {
    val actual = List('c', 'c').foldLeft("".pure[TokenReadState]) { (acc, c) =>
      acc.flatMap(_ => TokenState.readOne(c))
    }
    val expect = WordState("cc")
    assert(actual.runS(NeutralState).value == expect)
  }
}
