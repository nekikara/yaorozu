package yaorozu.query

import scala.annotation.tailrec
import scala.util.matching.Regex

case class CypToken(value: String)

object Tokenizer {
  case class Token(value: String)

  def next(query: String): CypToken = {
    @tailrec
    def catchToken(candidate: String, target: Char, rest: String, st: TokenState): CypToken = {
      st.process(candidate, target) match {
        case (t, _, true) => CypToken(t)
        case (nextCandidate, nextState, _) => catchToken(nextCandidate, rest.head, rest.tail, nextState)
        case _ => throw new RuntimeException("Can't catch a token")
      }
    }

    catchToken("", query.head, query.tail, WaitingState())
  }
}

sealed trait TokenState {
  val toClause: Regex = """([a-zA-Z])""".r
  val toList: Regex = """(\[)""".r
  val toInteger: Regex = """(\d)""".r
  val toWaiting: Regex = """([\s|\S])""".r
  def process(candidate: String, target: Char): (String, TokenState, Boolean)
}
case class WaitingState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    val nextState = target match {
      case toClause(_) => ClauseState()
      case toList(_) => ListState()
      case toInteger(_) => IntegerState()
      case toWaiting(_) => this
      case _ => throw new RuntimeException("Can't find a next state")
    }

    (target.toString, nextState, false)
  }
}
case class ClauseState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    target match {
      case toClause() => (candidate :+ target, this, false)
      case toWaiting() => (candidate, WaitingState(), true)
      case _ => (target.toString, WaitingState(), true)
    }
  }
}
case class ListState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    ("", WaitingState(), false)
  }
}
case class IntegerState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    target match {
      case toInteger() => (candidate :+ target, this, false)
      case _ => (candidate, WaitingState(), true)
    }
  }
}
