package yaorozu.query

import scala.annotation.tailrec

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
  def process(candidate: String, target: Char): (String, TokenState, Boolean)
}
case class WaitingState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    val toClause = """([a-zA-Z])""".r
    val toList = """(\[)""".r
    val toInteger = """(\d)""".r
    val toStay = """([\s|\S])""".r
    val nextState = target match {
      case toClause(_) => ClauseState()
      case toList(_) => ListState()
      case toInteger(_) => IntegerState()
      case toStay(_) => this
      case _ => throw new RuntimeException("Can't find a next state")
    }

    (target.toString, nextState, false)
  }
}
case class ClauseState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    ("", ClauseState(), false)
  }
}
case class ListState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    ("", WaitingState(), false)
  }
}
case class IntegerState() extends TokenState {
  override def process(candidate: String, target: Char): (String, TokenState, Boolean) = {
    ("", WaitingState(), false)
  }
}

