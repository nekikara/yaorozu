package yaorozu.query

import java.security.InvalidParameterException

object Parser {
  def parse(query: String): List[Token] = {
    @scala.annotation.tailrec
    def parseQuery(thunk: List[Token], analyzer: AnalyzeState, target: Char, rest: String): List[Token] = {
      val nextAnalyzer = analyzer.read(analyzer.candidate, target)
      val nextThunk = nextAnalyzer.token match {
        case Some(t) => thunk :+ t
        case None => thunk
      }
      if (rest.isEmpty) {
        val x = nextThunk :+ Token(nextAnalyzer.candidate)
        x
      } else {
        parseQuery(nextThunk, nextAnalyzer, rest.head, rest.tail)
      }
    }

    parseQuery(List.empty[Token], AnalyzeState.initial(), query.head, query.tail)
  }
}

case class Token(value: String)

case class AnalyzeState(candidate: String, nextState: ParseState, token: Option[Token]) extends ParseState {
  override def read(candidate: String, next: Char): AnalyzeState = nextState.read(candidate, next)
}
object AnalyzeState {
  def initial(): AnalyzeState = AnalyzeState("", NopeState(), None)
}

sealed trait ParseState {
  def read(candidate: String, next: Char): AnalyzeState
}
case class NopeState() extends ParseState {
  override def read(candidate: String, target: Char): AnalyzeState = {
    val toConstant = "([a-zA-Z])".r
    val toQuote = "(')".r
    val nextState = target match {
      case toConstant(_) => ConstantState()
      case toQuote(_) => QuoteContentState()
      case _ => this
    }
    AnalyzeState(target.toString, nextState, None)
  }
}
case class ConstantState() extends ParseState {
  override def read(candidate: String, target: Char): AnalyzeState = {
    val toConstant = """([a-zA-Z0-9])""".r
    val toNope = """(\s)""".r
    target match {
      case toConstant(_) => AnalyzeState(candidate :+ target, this, None)
      case toNope(_) => AnalyzeState("", NopeState(), Some(Token(candidate)))
      case _ => throw new InvalidParameterException(s"Can't read $target as a Token candidate")
    }
  }
}
case class QuoteContentState() extends ParseState {
  override def read(candidate: String, target: Char): AnalyzeState = {
    val waitQuote = "(')".r
    val nextCandidate = candidate :+ target
    target match {
      case waitQuote(_) => AnalyzeState("", NopeState(), Some(Token(nextCandidate)))
      case _ => AnalyzeState(nextCandidate, this, None)
    }
  }
}
