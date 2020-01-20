package yaorozu.query.sql

import cats.data.State

sealed trait TokenState {
  def event(c: Char): TokenState = this
  def endEvent(): EndState = EndState("")
}
case object Neutral extends TokenState {
  override def event(c: Char): TokenState = c match {
    case ' ' => WhiteSpaceState(c.toString)
    case _ => WordState(c.toString)
  }
}
case class WhiteSpaceState(candidate: String) extends TokenState {
  override def event(c: Char): TokenState = c match {
    case ' ' => WhiteSpaceState(candidate :+ c)
    case _ => EndState(candidate)
  }
}
case class WordState(candidate: String) extends TokenState {
  override def event(c: Char): TokenState = c match {
    case ' ' => EndState(candidate)
    case _ => WordState(candidate :+ c)
  }
  override def endEvent(): EndState = EndState(candidate)
}

case class EndState(result: String) extends TokenState {
  override def endEvent(): EndState = this
}

object TokenState {
  type TokenReadState[A] = State[TokenState, A]

  def readOne(c: Char): TokenReadState[String] = State[TokenState, String] {
    case x: WhiteSpaceState => (x.event(c), "")
  }
}
